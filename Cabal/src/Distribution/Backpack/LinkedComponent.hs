{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.LinkedComponent
  ( LinkedComponent (..)
  , lc_insts
  , lc_uid
  , lc_cid
  , lc_pkgid
  , toLinkedComponent
  , toLinkedComponents
  , dispLinkedComponent
  , LinkedComponentMap
  , extendLinkedComponentMap
  ) where

import Distribution.Compat.Prelude hiding ((<>))
import Prelude ()

import Distribution.Backpack
import Distribution.Backpack.ConfiguredComponent
import Distribution.Backpack.FullUnitId
import Distribution.Backpack.MixLink
import Distribution.Backpack.ModuleScope
import Distribution.Backpack.ModuleShape
import Distribution.Backpack.PreModuleShape
import Distribution.Backpack.UnifyM
import Distribution.Utils.MapAccum

import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.AnnotatedId
import Distribution.Types.ComponentInclude
import Distribution.Utils.LogProgress
import Distribution.Verbosity

import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Pretty (pretty)
import Text.PrettyPrint (Doc, hang, hsep, quotes, text, vcat, ($+$))

-- | A linked component is a component that has been mix-in linked, at
-- which point we have determined how all the dependencies of the
-- component are explicitly instantiated (in the form of an OpenUnitId).
-- 'ConfiguredComponent' is mix-in linked into 'LinkedComponent', which
-- is then instantiated into 'ReadyComponent'.
data LinkedComponent = LinkedComponent
  { lc_ann_id :: AnnotatedId ComponentId
  -- ^ Uniquely identifies linked component
  , lc_component :: Component
  -- ^ Corresponds to 'cc_component'.
  , lc_exe_deps :: [AnnotatedId OpenUnitId]
  -- ^ @build-tools@ and @build-tool-depends@ dependencies.
  -- Corresponds to 'cc_exe_deps'.
  , lc_public :: Bool
  -- ^ Is this the public library of a package?  Corresponds to
  -- 'cc_public'.
  , lc_includes :: [ComponentInclude OpenUnitId ModuleRenaming]
  -- ^ Corresponds to 'cc_includes', but (1) this does not contain
  -- includes of signature packages (packages with no exports),
  -- and (2) the 'ModuleRenaming' for requirements (stored in
  -- 'IncludeRenaming') has been removed, as it is reflected in
  -- 'OpenUnitId'.)
  , lc_sig_includes :: [ComponentInclude OpenUnitId ModuleRenaming]
  -- ^ Like 'lc_includes', but this specifies includes on
  -- signature packages which have no exports.
  , lc_shape :: ModuleShape
  -- ^ The module shape computed by mix-in linking.  This is
  -- newly computed from 'ConfiguredComponent'
  }

-- | Uniquely identifies a 'LinkedComponent'.  Corresponds to
-- 'cc_cid'.
lc_cid :: LinkedComponent -> ComponentId
lc_cid = ann_id . lc_ann_id

-- | Corresponds to 'cc_pkgid'.
lc_pkgid :: LinkedComponent -> PackageId
lc_pkgid = ann_pid . lc_ann_id

-- | The 'OpenUnitId' of this component in the "default" instantiation.
-- See also 'lc_insts'.  'LinkedComponent's cannot be instantiated
-- (e.g., there is no 'ModSubst' instance for them).
lc_uid :: LinkedComponent -> OpenUnitId
lc_uid lc = IndefFullUnitId (lc_cid lc) . Map.fromList $ lc_insts lc

-- | The instantiation of 'lc_uid'; this always has the invariant
-- that it is a mapping from a module name @A@ to @<A>@ (the hole A).
lc_insts :: LinkedComponent -> [(ModuleName, OpenModule)]
lc_insts lc =
  [ (req, OpenModuleVar req)
  | req <- Set.toList (modShapeRequires (lc_shape lc))
  ]

dispLinkedComponent :: LinkedComponent -> Doc
dispLinkedComponent lc =
  hang (text "unit" <+> pretty (lc_uid lc)) 4 $
    vcat
      [ text "include" <+> pretty (ci_id incl) <+> pretty (ci_renaming incl)
      | incl <- lc_includes lc
      ]
      $+$ vcat
        [ text "signature include" <+> pretty (ci_id incl)
        | incl <- lc_sig_includes lc
        ]
      $+$ dispOpenModuleSubst (modShapeProvides (lc_shape lc))

instance Package LinkedComponent where
  packageId = lc_pkgid

toLinkedComponent
  :: Verbosity
  -> Bool
  -- ^ Whether there are any "promised" package dependencies which we won't find already installed.
  -> FullDb
  -> PackageId
  -> LinkedComponentMap
  -> ConfiguredComponent
  -> LogProgress LinkedComponent
toLinkedComponent
  verbosity
  anyPromised
  db
  this_pid
  pkg_map
  ConfiguredComponent
    { cc_ann_id = aid@AnnotatedId{ann_id = this_cid}
    , cc_component = component
    , cc_exe_deps = exe_deps
    , cc_public = is_public
    , cc_includes = cid_includes
    } = do
    let
      -- The explicitly specified requirements, provisions and
      -- reexports from the Cabal file.  These are only non-empty for
      -- libraries; everything else is trivial.
      ( src_reqs :: [ModuleName]
        , src_provs :: [ModuleName]
        , src_reexports :: [ModuleReexport]
        ) =
          case component of
            CLib lib ->
              ( signatures lib
              , exposedModules lib
              , reexportedModules lib
              )
            _ -> ([], [], [])
      src_hidden = otherModules (componentBuildInfo component)

      -- Take each included ComponentId and resolve it into an
      -- \*unlinked* unit identity.  We will use unification (relying
      -- on the ModuleShape) to resolve these into linked identities.
      unlinked_includes :: [ComponentInclude (OpenUnitId, ModuleShape) IncludeRenaming]
      unlinked_includes =
        [ ComponentInclude (fmap lookupUid dep_aid) rns i
        | ComponentInclude dep_aid rns i <- cid_includes
        ]

      lookupUid :: ComponentId -> (OpenUnitId, ModuleShape)
      lookupUid cid =
        fromMaybe
          (error "linkComponent: lookupUid")
          (Map.lookup cid pkg_map)

    let orErr (Right x) = return x
        orErr (Left [err]) = dieProgress err
        orErr (Left errs) = do
          dieProgress
            ( vcat
                ( intersperse
                    (text "") -- double newline!
                    [hang (text "-") 2 err | err <- errs]
                )
            )

    -- Pre-shaping
    let pre_shape =
          mixLinkPreModuleShape $
            PreModuleShape
              { preModShapeProvides = Set.fromList (src_provs ++ src_hidden)
              , preModShapeRequires = Set.fromList src_reqs
              }
              : [ renamePreModuleShape (toPreModuleShape sh) rns
                | ComponentInclude (AnnotatedId{ann_id = (_, sh)}) rns _ <- unlinked_includes
                ]
        reqs = preModShapeRequires pre_shape
        insts =
          [ (req, OpenModuleVar req)
          | req <- Set.toList reqs
          ]
        this_uid = IndefFullUnitId this_cid . Map.fromList $ insts

    -- OK, actually do unification
    -- TODO: the unification monad might return errors, in which
    -- case we have to deal.  Use monadic bind for now.
    ( linked_shape0 :: ModuleScope
      , linked_includes0 :: [ComponentInclude OpenUnitId ModuleRenaming]
      , linked_sig_includes0 :: [ComponentInclude OpenUnitId ModuleRenaming]
      ) <-
      orErr $ runUnifyM verbosity this_cid db $ do
        -- The unification monad is implemented using mutable
        -- references.  Thus, we must convert our *pure* data
        -- structures into mutable ones to perform unification.

        let convertMod :: (ModuleName -> ModuleSource) -> ModuleName -> UnifyM s (ModuleScopeU s)
            convertMod from m = do
              m_u <- convertModule (OpenModule this_uid m)
              return (Map.singleton m [WithSource (from m) m_u], Map.empty)
        -- Handle 'exposed-modules'
        exposed_mod_shapes_u <- traverse (convertMod FromExposedModules) src_provs
        -- Handle 'other-modules'
        other_mod_shapes_u <- traverse (convertMod FromOtherModules) src_hidden

        -- Handle 'signatures'
        let convertReq :: ModuleName -> UnifyM s (ModuleScopeU s)
            convertReq req = do
              req_u <- convertModule (OpenModuleVar req)
              return (Map.empty, Map.singleton req [WithSource (FromSignatures req) req_u])
        req_shapes_u <- traverse convertReq src_reqs

        -- Handle 'mixins'
        (incl_shapes_u, all_includes_u) <- fmap unzip (traverse convertInclude unlinked_includes)

        failIfErrs -- Prevent error cascade
        -- Mix-in link everything!  mixLink is the real workhorse.
        shape_u <-
          mixLink $
            exposed_mod_shapes_u
              ++ other_mod_shapes_u
              ++ req_shapes_u
              ++ incl_shapes_u

        -- src_reqs_u <- traverse convertReq src_reqs
        -- Read out all the final results by converting back
        -- into a pure representation.
        let convertIncludeU (ComponentInclude dep_aid rns i) = do
              let component_name = pretty $ ann_cname dep_aid
              uid <- convertUnitIdU (ann_id dep_aid) component_name
              return
                ( ComponentInclude
                    { ci_ann_id = dep_aid{ann_id = uid}
                    , ci_renaming = rns
                    , ci_implicit = i
                    }
                )

        shape <- convertModuleScopeU shape_u
        let (includes_u, sig_includes_u) = partitionEithers all_includes_u
        incls <- traverse convertIncludeU includes_u
        sig_incls <- traverse convertIncludeU sig_includes_u
        return (shape, incls, sig_incls)

    let isNotLib (CLib _) = False
        isNotLib _ = True
    when (not (Set.null reqs) && isNotLib component) $
      dieProgress $
        hang
          (text "Non-library component has unfilled requirements:")
          4
          (vcat [pretty req | req <- Set.toList reqs])

    -- NB: do NOT include hidden modules here: GHC 7.10's ghc-pkg
    -- won't allow it (since someone could directly synthesize
    -- an 'InstalledPackageInfo' that violates abstraction.)
    -- Though, maybe it should be relaxed?
    let src_hidden_set = Set.fromList src_hidden
        linked_shape =
          linked_shape0
            { modScopeProvides =
                -- Would rather use withoutKeys but need BC
                Map.filterWithKey
                  (\k _ -> not (k `Set.member` src_hidden_set))
                  (modScopeProvides linked_shape0)
            }

    -- OK, compute the reexports
    -- TODO: This code reports the errors for reexports one reexport at
    -- a time.  Better to collect them all up and report them all at
    -- once.
    let hdl :: [Either Doc a] -> LogProgress [a]
        hdl es =
          case partitionEithers es of
            ([], rs) -> return rs
            (ls, _) ->
              dieProgress $
                hang
                  (text "Problem with module re-exports:")
                  2
                  (vcat [hang (text "-") 2 l | l <- ls])
    reexports_list <- hdl . (flip map) src_reexports $ \reex@(ModuleReexport mb_pn from to) -> do
      case Map.lookup from (modScopeProvides linked_shape) of
        Just cands@(x0 : xs0) -> do
          -- Make sure there is at least one candidate
          (x, xs) <-
            case mb_pn of
              Just pn ->
                let matches_pn (FromMixins pn' _ _) = pn == pn'
                    matches_pn (FromBuildDepends pn' _) = pn == pn'
                    matches_pn (FromExposedModules _) = pn == packageName this_pid
                    matches_pn (FromOtherModules _) = pn == packageName this_pid
                    matches_pn (FromSignatures _) = pn == packageName this_pid
                 in case filter (matches_pn . getSource) cands of
                      (x1 : xs1) -> return (x1, xs1)
                      _ -> Left (brokenReexportMsg reex)
              Nothing -> return (x0, xs0)
          -- Test that all the candidates are consistent
          case filter (\x' -> unWithSource x /= unWithSource x') xs of
            [] -> return ()
            _ -> Left $ ambiguousReexportMsg reex x xs
          return (to, Just (unWithSource x))
        _ ->
          -- Can't resolve it right now.. carry on with the assumption it will be resolved
          -- dynamically later by an in-memory package which hasn't been installed yet.
          if anyPromised
            then return (to, Nothing)
            else -- But if nothing is promised, eagerly report an error, as we already know everything.
              Left (brokenReexportMsg reex)

    -- TODO: maybe check this earlier; it's syntactically obvious.
    let build_reexports m (k, v)
          | Map.member k m =
              dieProgress $
                hsep
                  [text "Module name ", pretty k, text " is exported multiple times."]
          | otherwise = return (Map.insert k v m)
    provs <-
      foldM build_reexports Map.empty $
        -- TODO: doublecheck we have checked for
        -- src_provs duplicates already!
        -- These are normal module exports.
        [(mod_name, (OpenModule this_uid mod_name)) | mod_name <- src_provs]
          ++
          -- These are reexports, which we managed to resolve to something in an external package.
          [(mn_new, om) | (mn_new, Just om) <- reexports_list]
          ++
          -- These ones.. we didn't resolve but also we might not have to
          -- resolve them because they could come from a promised unit,
          -- which we don't know anything about yet. GHC will resolve
          -- these itself when it is dealing with the multi-session.
          -- These ones will not be built, registered and put
          -- into a package database, we only need them to make it as far
          -- as generating GHC options where the info will be used to
          -- pass the reexported-module option to GHC.

          -- We also know that in the case there are promised units that
          -- we will not be doing anything to do with backpack like
          -- unification etc..
          [ ( mod_name
            , OpenModule
                ( DefiniteUnitId
                    ( unsafeMkDefUnitId
                        (mkUnitId "fake")
                    )
                )
                mod_name
            )
          | (mod_name, Nothing) <- reexports_list
          ]

    let final_linked_shape = ModuleShape provs (Map.keysSet (modScopeRequires linked_shape))

    -- See Note Note [Signature package special case]
    let (linked_includes, linked_sig_includes)
          | Set.null reqs = (linked_includes0 ++ linked_sig_includes0, [])
          | otherwise = (linked_includes0, linked_sig_includes0)

    return $
      LinkedComponent
        { lc_ann_id = aid
        , lc_component = component
        , lc_public = is_public
        , -- These must be executables
          lc_exe_deps = map (fmap (\cid -> IndefFullUnitId cid Map.empty)) exe_deps
        , lc_shape = final_linked_shape
        , lc_includes = linked_includes
        , lc_sig_includes = linked_sig_includes
        }

-- Note [Signature package special case]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Suppose we have p-indef, which depends on str-sig and inherits
-- the hole from that signature package.  When we instantiate p-indef,
-- it's a bit pointless to also go ahead and build str-sig, because
-- str-sig cannot possibly have contributed any code to the package
-- in question.  Furthermore, because the signature was inherited to
-- p-indef, if we test matching against p-indef, we also have tested
-- matching against p-sig.  In fact, skipping p-sig is *mandatory*,
-- because p-indef may have thinned it (so that an implementation may
-- match p-indef but not p-sig.)
--
-- However, suppose that we have a package which mixes together str-sig
-- and str-bytestring, with the intent of *checking* that str-sig is
-- implemented by str-bytestring.  Here, it's quite important to
-- build an instantiated str-sig, since that is the only way we will
-- actually end up testing if the matching works.  Note that this
-- admonition only applies if the package has NO requirements; if it
-- has any requirements, we will typecheck it as an indefinite
-- package, at which point the signature includes will be passed to
-- GHC who will in turn actually do the checking to make sure they
-- are instantiated correctly.

-- Handle mix-in linking for components.  In the absence of Backpack,
-- every ComponentId gets converted into a UnitId by way of SimpleUnitId.
toLinkedComponents
  :: Verbosity
  -> Bool
  -- ^ Whether there are any "promised" package dependencies which we won't
  -- find already installed.
  -> FullDb
  -> PackageId
  -> LinkedComponentMap
  -> [ConfiguredComponent]
  -> LogProgress [LinkedComponent]
toLinkedComponents verbosity anyPromised db this_pid lc_map0 comps =
  fmap snd (mapAccumM go lc_map0 comps)
  where
    go
      :: Map ComponentId (OpenUnitId, ModuleShape)
      -> ConfiguredComponent
      -> LogProgress (Map ComponentId (OpenUnitId, ModuleShape), LinkedComponent)
    go lc_map cc = do
      lc <-
        addProgressCtx (text "In the stanza" <+> text (componentNameStanza (cc_name cc))) $
          toLinkedComponent verbosity anyPromised db this_pid lc_map cc
      return (extendLinkedComponentMap lc lc_map, lc)

type LinkedComponentMap = Map ComponentId (OpenUnitId, ModuleShape)

extendLinkedComponentMap
  :: LinkedComponent
  -> LinkedComponentMap
  -> LinkedComponentMap
extendLinkedComponentMap lc m =
  Map.insert (lc_cid lc) (lc_uid lc, lc_shape lc) m

brokenReexportMsg :: ModuleReexport -> Doc
brokenReexportMsg (ModuleReexport (Just pn) from _to) =
  vcat
    [ text "The package" <+> quotes (pretty pn)
    , text "does not export a module" <+> quotes (pretty from)
    ]
brokenReexportMsg (ModuleReexport Nothing from _to) =
  vcat
    [ text "The module" <+> quotes (pretty from)
    , text "is not exported by any suitable package."
    , text "It occurs in neither the 'exposed-modules' of this package,"
    , text "nor any of its 'build-depends' dependencies."
    ]

ambiguousReexportMsg :: ModuleReexport -> ModuleWithSource -> [ModuleWithSource] -> Doc
ambiguousReexportMsg (ModuleReexport mb_pn from _to) y1 ys =
  vcat
    [ text "Ambiguous reexport" <+> quotes (pretty from)
    , hang
        (text "It could refer to either:")
        2
        (vcat (msg : msgs))
    , help_msg mb_pn
    ]
  where
    msg = text "  " <+> displayModuleWithSource y1
    msgs = [text "or" <+> displayModuleWithSource y | y <- ys]
    help_msg Nothing =
      -- TODO: This advice doesn't help if the ambiguous exports
      -- come from a package named the same thing
      vcat
        [ text "The ambiguity can be resolved by qualifying the"
        , text "re-export with a package name."
        , text "The syntax is 'packagename:ModuleName [as NewName]'."
        ]
    -- Qualifying won't help that much.
    help_msg (Just _) =
      vcat
        [ text "The ambiguity can be resolved by using the"
        , text "mixins field to rename one of the module"
        , text "names differently."
        ]
    displayModuleWithSource y =
      vcat
        [ quotes (pretty (unWithSource y))
        , text "brought into scope by"
            <+> dispModuleSource (getSource y)
        ]
