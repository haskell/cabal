{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.LinkedComponent (
    LinkedComponent(..),
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
import Distribution.Backpack.ModSubst
import Distribution.Backpack.ModuleShape
import Distribution.Backpack.ModuleScope
import Distribution.Backpack.UnifyM
import Distribution.Backpack.MixLink
import Distribution.Utils.MapAccum

import Distribution.Types.ModuleRenaming
import Distribution.Types.IncludeRenaming
import Distribution.Package
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.ModuleName
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Utils.Progress
import Distribution.Utils.LogProgress

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Traversable
    ( mapM )
import Distribution.Text
    ( Text(disp) )
import Text.PrettyPrint

-- | A linked component, we know how it is instantiated and thus how we are
-- going to build it.
data LinkedComponent
    = LinkedComponent {
        lc_uid :: OpenUnitId,
        lc_cid :: ComponentId,
        lc_pkgid :: PackageId,
        lc_insts :: [(ModuleName, OpenModule)],
        lc_component :: Component,
        lc_shape :: ModuleShape,
        -- | Local buildTools dependencies
        lc_internal_build_tools :: [OpenUnitId],
        lc_public :: Bool,
        lc_includes :: [(OpenUnitId, ModuleRenaming)],
        -- PackageId here is a bit dodgy, but its just for
        -- BC so it shouldn't matter.
        lc_depends :: [(OpenUnitId, PackageId)]
      }

dispLinkedComponent :: LinkedComponent -> Doc
dispLinkedComponent lc =
    hang (text "unit" <+> disp (lc_uid lc)) 4 $
         vcat [ text "include" <+> disp uid <+> disp prov_rn
              | (uid, prov_rn) <- lc_includes lc ]
            -- YARRR $+$ dispModSubst (modShapeProvides (lc_shape lc))

instance Package LinkedComponent where
    packageId = lc_pkgid

instance ModSubst LinkedComponent where
    modSubst subst lc
        = lc {
            lc_uid = modSubst subst (lc_uid lc),
            lc_insts = modSubst subst (lc_insts lc),
            lc_shape = modSubst subst (lc_shape lc),
            lc_includes = map (\(uid, rns) -> (modSubst subst uid, rns)) (lc_includes lc),
            lc_depends = map (\(uid, pkgid) -> (modSubst subst uid, pkgid)) (lc_depends lc)
          }

{-
instance IsNode LinkedComponent where
    type Key LinkedComponent = UnitId
    nodeKey = lc_uid
    nodeNeighbors n =
        if Set.null (openUnitIdFreeHoles (lc_uid n))
            then map fst (lc_depends n)
            else ordNub (map (generalizeUnitId . fst) (lc_depends n))
-}

-- We can't cache these values because they need to be changed
-- when we substitute over a 'LinkedComponent'.  By varying
-- these over 'UnitId', we can support old GHCs. Nice!

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
        unlinked_includes :: [((OpenUnitId, ModuleShape), PackageId, IncludeRenaming)]
        unlinked_includes = [ (lookupUid cid, pid, rns)
                            | (cid, pid, rns) <- cid_includes ]

        lookupUid :: ComponentId -> (OpenUnitId, ModuleShape)
        lookupUid cid = fromMaybe (error "linkComponent: lookupUid")
                                    (Map.lookup cid pkg_map)

    let orErr (Right x) = return x
        orErr (Left err) = failProgress (text err)

    -- OK, actually do unification
    -- TODO: the unification monad might return errors, in which
    -- case we have to deal.  Use monadic bind for now.
    (linked_shape0   :: ModuleScope,
     linked_deps     :: [(OpenUnitId, PackageId)],
     linked_includes :: [(OpenUnitId, ModuleRenaming)]) <- orErr $ runUnifyM verbosity db $ do
        -- The unification monad is implemented using mutable
        -- references.  Thus, we must convert our *pure* data
        -- structures into mutable ones to perform unification.
        --
        let convertReq :: ModuleName -> UnifyM s (ModuleScopeU s)
            convertReq req = do
                req_u <- convertModule (OpenModuleVar req)
                return (Map.empty, Map.singleton req req_u)
            -- NB: We DON'T convert locally defined modules, as in the
            -- absence of mutual recursion across packages they
            -- cannot participate in mix-in linking.
        (shapes_u, includes_u) <- fmap unzip (mapM convertInclude unlinked_includes)
        src_reqs_u <- mapM convertReq src_reqs
        -- Mix-in link everything!  mixLink is the real workhorse.
        shape_u <- foldM mixLink emptyModuleScopeU (shapes_u ++ src_reqs_u)
        -- Read out all the final results by converting back
        -- into a pure representation.
        let convertIncludeU (uid_u, pid, rns) = do
                uid <- convertUnitIdU uid_u
                return ((uid, rns), (uid, pid))
        shape <- convertModuleScopeU shape_u
        includes_deps <- mapM convertIncludeU includes_u
        let (incls, deps) = unzip includes_deps
        return (shape, deps, incls)

    -- linked_shape0 is almost complete, but it doesn't contain
    -- the actual modules we export ourselves.  Add them!
    let reqs = modScopeRequires linked_shape0
        -- check that there aren't pre-filled requirements...
        insts = [ (req, OpenModuleVar req)
                | req <- Set.toList reqs ]
        this_uid = IndefFullUnitId this_cid . Map.fromList $ insts

        -- add the local exports to the scope
        local_exports = Map.fromListWith (++) $
          [ (mod_name, [ModuleSource (packageName this_pid)
                                     defaultIncludeRenaming
                                     (OpenModule this_uid mod_name)])
          | mod_name <- src_provs ]
          -- NB: do NOT include hidden modules here: GHC 7.10's ghc-pkg
          -- won't allow it (since someone could directly synthesize
          -- an 'InstalledPackageInfo' that violates abstraction.)
          -- Though, maybe it should be relaxed?
        linked_shape = linked_shape0 {
                          modScopeProvides =
                            Map.unionWith (++)
                              local_exports
                              (modScopeProvides linked_shape0)
                        }

    -- OK, compute the reexports
    -- TODO: This code reports the errors for reexports one reexport at
    -- a time.  Better to collect them all up and report them all at
    -- once.
    reexports_list <- for src_reexports $ \reex@(ModuleReexport mb_pn from to) -> do
      let err :: Doc -> LogProgress a
          err s = failProgress
            $ hang (text "Problem with module re-export" <> quotes (disp reex)
                        <+> colon) 2 s
      case Map.lookup from (modScopeProvides linked_shape) of
        Just cands@(x0:xs0) -> do
          -- Make sure there is at least one candidate
          (x, xs) <-
            case mb_pn of
              Just pn ->
                case filter ((pn==) . msrc_pkgname) cands of
                  (x1:xs1) -> return (x1, xs1)
                  _ -> err (brokenReexportMsg reex)
              Nothing -> return (x0, xs0)
          -- Test that all the candidates are consistent
          case filter (\x' -> msrc_module x /= msrc_module x') xs of
            [] -> return ()
            _ -> err $ ambiguousReexportMsg reex (x:xs)
          return (to, msrc_module x)
        _ ->
          err (brokenReexportMsg reex)

    -- TODO: maybe check this earlier; it's syntactically obvious.
    let build_reexports m (k, v)
            | Map.member k m =
                failProgress $ hsep
                    [ text "Module name ", disp k, text " is exported multiple times." ]
            | otherwise = return (Map.insert k v m)
    provs <- foldM build_reexports Map.empty $
                -- TODO: doublecheck we have checked for
                -- src_provs duplicates already!
                [ (mod_name, OpenModule this_uid mod_name) | mod_name <- src_provs ] ++
                reexports_list

    let final_linked_shape = ModuleShape provs (modScopeRequires linked_shape)

    return $ LinkedComponent {
                lc_uid = this_uid,
                lc_cid = this_cid,
                lc_insts = insts,
                lc_pkgid = pkgid,
                lc_component = component,
                lc_public = is_public,
                -- These must be executables
                lc_internal_build_tools = map (\cid -> IndefFullUnitId cid Map.empty) btools,
                lc_shape = final_linked_shape,
                lc_includes = linked_includes,
                lc_depends = linked_deps
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
    lc <- toLinkedComponent verbosity db this_pid lc_map cc
    return (extendLinkedComponentMap lc lc_map, lc)

type LinkedComponentMap = Map ComponentId (OpenUnitId, ModuleShape)

extendLinkedComponentMap :: LinkedComponent
                         -> LinkedComponentMap
                         -> LinkedComponentMap
extendLinkedComponentMap lc m =
    Map.insert (lc_cid lc) (lc_uid lc, lc_shape lc) m

brokenReexportMsg :: ModuleReexport -> Doc
brokenReexportMsg (ModuleReexport (Just pn) from _to) =
    text "The package" <+> disp pn <+>
    text "does not export a module" <+> disp from
brokenReexportMsg (ModuleReexport Nothing from _to) =
    text "The module" <+> disp from <+>
    text "is not exported by any suitable package." <+>
    text "It occurs in neither the 'exposed-modules' of this package," <+>
    text "nor any of its 'build-depends' dependencies."

ambiguousReexportMsg :: ModuleReexport -> [ModuleSource] -> Doc
ambiguousReexportMsg (ModuleReexport mb_pn from _to) ys =
    text "The module" <+> disp from <+>
    text "is (differently) exported by more than one package" <+>
    parens (hsep (punctuate comma [displaySource y | y <- ys])) <+>
    text "making the re-export ambiguous." <+> help_msg mb_pn
  where
    help_msg Nothing =
        text "The ambiguity can be resolved by qualifying the" <+>
        text "re-export with a package name." <+>
        text "The syntax is 'packagename:ModuleName [as NewName]'."
    -- Qualifying won't help that much.
    help_msg (Just _) =
        text "The ambiguity can be resolved by introducing a" <+>
        text "backpack-include field to rename one of the module" <+>
        text "names differently."
    displaySource y
      | not (isDefaultIncludeRenaming (msrc_renaming y))
          = disp (msrc_pkgname y) <+> text "with renaming" <+>
            disp (includeProvidesRn (msrc_renaming y))
      | otherwise = disp (msrc_pkgname y)
