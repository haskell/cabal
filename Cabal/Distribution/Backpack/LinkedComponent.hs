{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.LinkedComponent (
    LinkedComponent(..),
    lc_insts,
    lc_uid,
    toLinkedComponent,
    toLinkedComponents,
    dispLinkedComponent,
    LinkedComponentMap,
    extendLinkedComponentMap,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding ((<>))

import Distribution.Backpack
import Distribution.Backpack.FullUnitId
import Distribution.Backpack.ConfiguredComponent
import Distribution.Backpack.ModuleShape
import Distribution.Backpack.ModuleScope
import Distribution.Backpack.UnifyM
import Distribution.Backpack.MixLink
import Distribution.Utils.MapAccum

import Distribution.Types.ComponentName
import Distribution.Types.ModuleRenaming
import Distribution.Types.IncludeRenaming
import Distribution.Types.ComponentInclude
import Distribution.Types.ComponentId
import Distribution.Types.PackageId
import Distribution.Package
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.ModuleName
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Utils.LogProgress

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Traversable
    ( mapM )
import Distribution.Text
    ( Text(disp) )
import Text.PrettyPrint
import Data.Either

-- | A linked component is a component that has been mix-in linked, at
-- which point we have determined how all the dependencies of the
-- component are explicitly instantiated (in the form of an OpenUnitId).
-- 'ConfiguredComponent' is mix-in linked into 'LinkedComponent', which
-- is then instantiated into 'ReadyComponent'.
data LinkedComponent
    = LinkedComponent {
        -- | Uniquely identifies a 'LinkedComponent'.  Corresponds to
        -- 'cc_cid'.
        lc_cid :: ComponentId,
        -- | Corresponds to 'cc_pkgid'.
        lc_pkgid :: PackageId,
        -- | Corresponds to 'cc_component'.
        lc_component :: Component,
        -- | Local @build-tools@ and @build-tool-depends@ dependencies on
        -- executables from the same package.  Corresponds to
        -- 'cc_internal_build_tools'.
        lc_internal_build_tools :: [OpenUnitId],
        -- | Is this the public library of a package?  Corresponds to
        -- 'cc_public'.
        lc_public :: Bool,
        -- | Corresponds to 'cc_includes', but (1) this does not contain
        -- includes of signature packages (packages with no exports),
        -- and (2) the 'ModuleRenaming' for requirements (stored in
        -- 'IncludeRenaming') has been removed, as it is reflected in
        -- 'OpenUnitId'.)
        lc_includes :: [ComponentInclude OpenUnitId ModuleRenaming],
        -- | Like 'lc_includes', but this specifies includes on
        -- signature packages which have no exports.
        lc_sig_includes :: [ComponentInclude OpenUnitId ModuleRenaming],
        -- | The module shape computed by mix-in linking.  This is
        -- newly computed from 'ConfiguredComponent'
        lc_shape :: ModuleShape
      }

-- | The 'OpenUnitId' of this component in the "default" instantiation.
-- See also 'lc_insts'.  'LinkedComponent's cannot be instantiated
-- (e.g., there is no 'ModSubst' instance for them).
lc_uid :: LinkedComponent -> OpenUnitId
lc_uid lc = IndefFullUnitId (lc_cid lc) . Map.fromList $ lc_insts lc

-- | The instantiation of 'lc_uid'; this always has the invariant
-- that it is a mapping from a module name @A@ to @<A>@ (the hole A).
lc_insts :: LinkedComponent -> [(ModuleName, OpenModule)]
lc_insts lc = [ (req, OpenModuleVar req)
              | req <- Set.toList (modShapeRequires (lc_shape lc)) ]

dispLinkedComponent :: LinkedComponent -> Doc
dispLinkedComponent lc =
    hang (text "unit" <+> disp (lc_uid lc)) 4 $
         vcat [ text "include" <+> disp (ci_id incl) <+> disp (ci_renaming incl)
              | incl <- lc_includes lc ]
            $+$
         vcat [ text "signature include" <+> disp (ci_id incl)
              | incl <- lc_sig_includes lc ]
            $+$ dispOpenModuleSubst (modShapeProvides (lc_shape lc))

instance Package LinkedComponent where
    packageId = lc_pkgid

toLinkedComponent
    :: Verbosity
    -> FullDb
    -> PackageId
    -> LinkedComponentMap
    -> ConfiguredComponent
    -> LogProgress LinkedComponent
toLinkedComponent verbosity db this_pid pkg_map ConfiguredComponent {
    cc_cid = this_cid,
    cc_pkgid = pkgid,
    cc_component = component,
    cc_internal_build_tools = btools,
    cc_public = is_public,
    cc_includes = cid_includes
   } = do
    let
        -- The explicitly specified requirements, provisions and
        -- reexports from the Cabal file.  These are only non-empty for
        -- libraries; everything else is trivial.
        (src_reqs      :: [ModuleName],
         src_provs     :: [ModuleName],
         src_reexports :: [ModuleReexport]) =
            case component of
                CLib lib -> (signatures lib,
                             exposedModules lib,
                             reexportedModules lib)
                _ -> ([], [], [])

        -- Take each included ComponentId and resolve it into an
        -- *unlinked* unit identity.  We will use unification (relying
        -- on the ModuleShape) to resolve these into linked identities.
        unlinked_includes :: [ComponentInclude (OpenUnitId, ModuleShape) IncludeRenaming]
        unlinked_includes = [ ComponentInclude (lookupUid cid) pid rns i
                            | ComponentInclude cid pid rns i <- cid_includes ]

        lookupUid :: ComponentId -> (OpenUnitId, ModuleShape)
        lookupUid cid = fromMaybe (error "linkComponent: lookupUid")
                                    (Map.lookup cid pkg_map)

    let orErr (Right x) = return x
        orErr (Left [err]) = dieProgress err
        orErr (Left errs) = do
            dieProgress (vcat (intersperse (text "") -- double newline!
                                [ hang (text "-") 2 err | err <- errs]))

    -- OK, actually do unification
    -- TODO: the unification monad might return errors, in which
    -- case we have to deal.  Use monadic bind for now.
    (linked_shape0   :: ModuleScope,
     linked_includes :: [ComponentInclude OpenUnitId ModuleRenaming],
     linked_sig_includes :: [ComponentInclude OpenUnitId ModuleRenaming])
      <- orErr $ runUnifyM verbosity db $ do
        -- The unification monad is implemented using mutable
        -- references.  Thus, we must convert our *pure* data
        -- structures into mutable ones to perform unification.
        --
        {-
        let convertReq :: ModuleName -> UnifyM s (ModuleScopeU s)
            convertReq req = do
                req_u <- convertModule (OpenModuleVar req)
                return (Map.empty, Map.singleton req req_u)
            -- NB: We DON'T convert locally defined modules, as in the
            -- absence of mutual recursion across packages they
            -- cannot participate in mix-in linking.
        -}
        (shapes_u, all_includes_u) <- fmap unzip (mapM convertInclude unlinked_includes)
        failIfErrs -- Prevent error cascade
        -- Mix-in link everything!  mixLink is the real workhorse.
        shape_u <- mixLink shapes_u
        -- shape_u <- foldM mixLink emptyModuleScopeU (shapes_u ++ src_reqs_u)
        -- src_reqs_u <- mapM convertReq src_reqs
        -- Read out all the final results by converting back
        -- into a pure representation.
        let convertIncludeU (ComponentInclude uid_u pid rns i) = do
                uid <- convertUnitIdU uid_u
                return (ComponentInclude {
                            ci_id = uid,
                            ci_pkgid = pid,
                            ci_renaming = rns,
                            ci_implicit = i
                        })
        shape <- convertModuleScopeU shape_u
        let (includes_u, sig_includes_u) = partitionEithers all_includes_u
        incls <- mapM convertIncludeU includes_u
        sig_incls <- mapM convertIncludeU sig_includes_u
        return (shape, incls, sig_incls)

    -- linked_shape0 is almost complete, but it doesn't contain
    -- the actual modules we export ourselves.  Add them!
    let reqs = Map.keysSet (modScopeRequires linked_shape0)
                `Set.union` Set.fromList src_reqs
        -- TODO: check that there aren't pre-filled requirements...
        insts = [ (req, OpenModuleVar req)
                | req <- Set.toList reqs ]
        this_uid = IndefFullUnitId this_cid . Map.fromList $ insts

        -- add the local exports to the scope
        local_source m = [ModuleSource (packageName this_pid) defaultIncludeRenaming m True]
        local_exports = Map.fromListWith (++) $
          [ (mod_name, local_source (OpenModule this_uid mod_name)) | mod_name <- src_provs ]
        local_reqs = Map.fromListWith (++) $
          [ (mod_name, local_source (OpenModuleVar mod_name)) | mod_name <- src_reqs ]
          -- NB: do NOT include hidden modules here: GHC 7.10's ghc-pkg
          -- won't allow it (since someone could directly synthesize
          -- an 'InstalledPackageInfo' that violates abstraction.)
          -- Though, maybe it should be relaxed?
        linked_shape = linked_shape0 {
                          modScopeProvides =
                            Map.unionWith (++)
                              local_exports
                              (modScopeProvides linked_shape0),
                          -- TODO: test that requirements aren't already
                          -- in scope
                          modScopeRequires =
                            Map.unionWith (++)
                              local_reqs
                              (modScopeRequires linked_shape0)
                        }

    let isNotLib (CLib _) = False
        isNotLib _        = True
    when (not (Set.null reqs) && isNotLib component) $
        dieProgress $
            hang (text "Non-library component has unfilled requirements:")
                4 (vcat [disp req | req <- Set.toList reqs])

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
                     hang (text "Problem with module re-exports:") 2
                        (vcat [hang (text "-") 2 l | l <- ls])
    reexports_list <- hdl . (flip map) src_reexports $ \reex@(ModuleReexport mb_pn from to) -> do
      case Map.lookup from (modScopeProvides linked_shape) of
        Just cands@(x0:xs0) -> do
          -- Make sure there is at least one candidate
          (x, xs) <-
            case mb_pn of
              Just pn ->
                case filter ((pn==) . msrc_pkgname) cands of
                  (x1:xs1) -> return (x1, xs1)
                  _ -> Left (brokenReexportMsg reex)
              Nothing -> return (x0, xs0)
          -- Test that all the candidates are consistent
          case filter (\x' -> msrc_module x /= msrc_module x') xs of
            [] -> return ()
            _ -> Left $ ambiguousReexportMsg reex x xs
          return (to, msrc_module x)
        _ ->
          Left (brokenReexportMsg reex)

    -- TODO: maybe check this earlier; it's syntactically obvious.
    let build_reexports m (k, v)
            | Map.member k m =
                dieProgress $ hsep
                    [ text "Module name ", disp k, text " is exported multiple times." ]
            | otherwise = return (Map.insert k v m)
    provs <- foldM build_reexports Map.empty $
                -- TODO: doublecheck we have checked for
                -- src_provs duplicates already!
                [ (mod_name, OpenModule this_uid mod_name) | mod_name <- src_provs ] ++
                reexports_list

    let final_linked_shape = ModuleShape provs (Map.keysSet (modScopeRequires linked_shape))

    return $ LinkedComponent {
                lc_cid = this_cid,
                lc_pkgid = pkgid,
                lc_component = component,
                lc_public = is_public,
                -- These must be executables
                lc_internal_build_tools = map (\cid -> IndefFullUnitId cid Map.empty) btools,
                lc_shape = final_linked_shape,
                lc_includes = linked_includes,
                lc_sig_includes = linked_sig_includes
           }

-- Handle mix-in linking for components.  In the absence of Backpack,
-- every ComponentId gets converted into a UnitId by way of SimpleUnitId.
toLinkedComponents
    :: Verbosity
    -> FullDb
    -> PackageId
    -> LinkedComponentMap
    -> [ConfiguredComponent]
    -> LogProgress [LinkedComponent]
toLinkedComponents verbosity db this_pid lc_map0 comps
   = fmap snd (mapAccumM go lc_map0 comps)
 where
  go :: Map ComponentId (OpenUnitId, ModuleShape)
     -> ConfiguredComponent
     -> LogProgress (Map ComponentId (OpenUnitId, ModuleShape), LinkedComponent)
  go lc_map cc = do
    lc <- addProgressCtx (text "In the stanza" <+> text (componentNameStanza (cc_name cc))) $
            toLinkedComponent verbosity db this_pid lc_map cc
    return (extendLinkedComponentMap lc lc_map, lc)

type LinkedComponentMap = Map ComponentId (OpenUnitId, ModuleShape)

extendLinkedComponentMap :: LinkedComponent
                         -> LinkedComponentMap
                         -> LinkedComponentMap
extendLinkedComponentMap lc m =
    Map.insert (lc_cid lc) (lc_uid lc, lc_shape lc) m

brokenReexportMsg :: ModuleReexport -> Doc
brokenReexportMsg (ModuleReexport (Just pn) from _to) =
  vcat [ text "The package" <+> quotes (disp pn)
       , text "does not export a module" <+> quotes (disp from) ]
brokenReexportMsg (ModuleReexport Nothing from _to) =
  vcat [ text "The module" <+> quotes (disp from)
       , text "is not exported by any suitable package."
       , text "It occurs in neither the 'exposed-modules' of this package,"
       , text "nor any of its 'build-depends' dependencies." ]

ambiguousReexportMsg :: ModuleReexport -> ModuleSource -> [ModuleSource] -> Doc
ambiguousReexportMsg (ModuleReexport mb_pn from _to) y1 ys =
  vcat [ text "Ambiguous reexport" <+> quotes (disp from)
       , hang (text "It could refer to either:") 2
            (vcat (msg : msgs))
       , help_msg mb_pn ]
  where
    msg  = text "  " <+> displaySource y1
    msgs = [text "or" <+> displaySource y | y <- ys]
    help_msg Nothing =
      -- TODO: This advice doesn't help if the ambiguous exports
      -- come from a package named the same thing
      vcat [ text "The ambiguity can be resolved by qualifying the"
           , text "re-export with a package name."
           , text "The syntax is 'packagename:ModuleName [as NewName]'." ]
    -- Qualifying won't help that much.
    help_msg (Just _) =
      vcat [ text "The ambiguity can be resolved by using the"
           , text "mixins field to rename one of the module"
           , text "names differently." ]
    displaySource y
      = vcat [ quotes (disp (msrc_module y))
             , text "brought into scope by" <+>
                if not (isDefaultIncludeRenaming (msrc_renaming y))
                  then text "the mixin" <+>
                       disp (msrc_pkgname y) <+>
                       parens (disp (includeProvidesRn (msrc_renaming y)))
                  else text "the build dependency on" <+> disp (msrc_pkgname y)
             ]
