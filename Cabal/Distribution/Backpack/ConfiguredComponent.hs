{-# LANGUAGE PatternGuards #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ConfiguredComponent (
    ConfiguredComponent(..),
    toConfiguredComponent,
    toConfiguredComponents,
    dispConfiguredComponent,

    ConfiguredComponentMap,
    extendConfiguredComponentMap,

    -- TODO: Should go somewhere else
    newPackageDepsBehaviour
) where

import Prelude ()
import Distribution.Compat.Prelude hiding ((<>))

import Distribution.Backpack.Id

import Distribution.Types.IncludeRenaming
import Distribution.Types.Mixin
import Distribution.Package
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.Setup as Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Version

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Traversable
    ( mapAccumL )
import Distribution.Text
import Text.PrettyPrint

-- | A configured component, we know exactly what its 'ComponentId' is,
-- and the 'ComponentId's of the things it depends on.
data ConfiguredComponent
    = ConfiguredComponent {
        cc_cid :: ComponentId,
        -- The package this component came from.
        cc_pkgid :: PackageId,
        cc_component :: Component,
        cc_public :: Bool,
        -- ^ Is this the public library component of the package?
        -- (THIS is what the hole instantiation applies to.)
        -- Note that in one-component configure mode, this is
        -- always True, because any component is the "public" one.)
        cc_internal_build_tools :: [ComponentId],
        -- Not resolved yet; component configuration only looks at ComponentIds.
        cc_includes :: [(ComponentId, PackageId, IncludeRenaming)]
      }

cc_name :: ConfiguredComponent -> ComponentName
cc_name = componentName . cc_component

dispConfiguredComponent :: ConfiguredComponent -> Doc
dispConfiguredComponent cc =
    hang (text "component" <+> disp (cc_cid cc)) 4
         (vcat [ hsep $ [ text "include", disp cid, disp incl_rn ]
               | (cid, _, incl_rn) <- cc_includes cc
               ])


-- | Construct a 'ConfiguredComponent', given that the 'ComponentId'
-- and library/executable dependencies are known.  The primary
-- work this does is handling implicit @backpack-include@ fields.
mkConfiguredComponent
    :: PackageId
    -> ComponentId
    -> [(PackageName, (ComponentId, PackageId))]
    -> [ComponentId]
    -> Component
    -> ConfiguredComponent
mkConfiguredComponent this_pid this_cid lib_deps exe_deps component =
    ConfiguredComponent {
        cc_cid = this_cid,
        cc_pkgid = this_pid,
        cc_component = component,
        cc_public = is_public,
        cc_internal_build_tools = exe_deps,
        cc_includes = explicit_includes ++ implicit_includes
    }
  where
    bi = componentBuildInfo component
    deps = map snd lib_deps
    deps_map = Map.fromList lib_deps

    -- Resolve each @backpack-include@ into the actual dependency
    -- from @lib_deps@.
    explicit_includes
        = [ (cid, pid { pkgName = name }, rns)
        | Mixin name rns <- mixins bi
        , Just (cid, pid) <- [Map.lookup name deps_map] ]

    -- Any @build-depends@ which is not explicitly mentioned in
    -- @backpack-include@ is converted into an "implicit" include.
    used_explicitly = Set.fromList (map (\(cid,_,_) -> cid) explicit_includes)
    implicit_includes
        = map (\(cid, pid) -> (cid, pid, defaultIncludeRenaming))
        $ filter (flip Set.notMember used_explicitly . fst) deps

    is_public = componentName component == CLibName

type ConfiguredComponentMap =
        (Map PackageName (ComponentId, PackageId), -- libraries
         Map String ComponentId)                   -- executables

-- Executable map must be different because an executable can
-- have the same name as a library. Ew.

-- | Given some ambient environment of package names that
-- are "in scope", looks at the 'BuildInfo' to decide
-- what the packages actually resolve to, and then builds
-- a 'ConfiguredComponent'.
toConfiguredComponent
    :: PackageDescription
    -> ComponentId
    -> Map PackageName (ComponentId, PackageId) -- external
    -> ConfiguredComponentMap
    -> Component
    -> ConfiguredComponent
toConfiguredComponent pkg_descr this_cid
       external_lib_map (lib_map, exe_map) component =
    mkConfiguredComponent
       (package pkg_descr) this_cid
       lib_deps exe_deps component
  where
    bi = componentBuildInfo component
    find_it :: PackageName -> (ComponentId, PackageId)
    find_it name =
        fromMaybe (error ("toConfiguredComponent: " ++ display (packageName pkg_descr) ++
                            " " ++ display name)) $
            Map.lookup name lib_map <|>
            Map.lookup name external_lib_map
    lib_deps
        | newPackageDepsBehaviour pkg_descr
        = [ (name, find_it name)
          | Dependency name _ <- targetBuildDepends bi ]
        | otherwise
        = Map.toList external_lib_map
    exe_deps = [ cid
               | Dependency pkgname _ <- buildTools bi
               , let name = unPackageName pkgname
               , Just cid <- [ Map.lookup name exe_map ] ]

-- | Also computes the 'ComponentId', and sets cc_public if necessary.
-- This is Cabal-only; cabal-install won't use this.
toConfiguredComponent'
    :: Bool -- use_external_internal_deps
    -> FlagAssignment
    -> PackageDescription
    -> Flag String      -- configIPID (todo: remove me)
    -> Flag ComponentId -- configCID
    -> Map PackageName (ComponentId, PackageId) -- external
    -> ConfiguredComponentMap
    -> Component
    -> ConfiguredComponent
toConfiguredComponent' use_external_internal_deps flags
                pkg_descr ipid_flag cid_flag
                external_lib_map (lib_map, exe_map) component =
    let cc = toConfiguredComponent
                pkg_descr this_cid
                external_lib_map (lib_map, exe_map) component
    in if use_external_internal_deps
        then cc { cc_public = True }
        else cc
  where
    this_cid = computeComponentId ipid_flag cid_flag (package pkg_descr)
                (componentName component) (Just (deps, flags))
    deps = [ cid | (cid, _) <- Map.elems external_lib_map ]

extendConfiguredComponentMap
    :: ConfiguredComponent
    -> ConfiguredComponentMap
    -> ConfiguredComponentMap
extendConfiguredComponentMap cc (lib_map, exe_map) =
    (lib_map', exe_map')
  where
    lib_map'
      = case cc_name cc of
          CLibName ->
            Map.insert (pkgName (cc_pkgid cc))
                       (cc_cid cc, cc_pkgid cc) lib_map
          CSubLibName str ->
            Map.insert (mkPackageName str)
                       (cc_cid cc, cc_pkgid cc) lib_map
          _ -> lib_map
    exe_map'
      = case cc_name cc of
          CExeName str ->
            Map.insert str (cc_cid cc) exe_map
          _ -> exe_map

-- Compute the 'ComponentId's for a graph of 'Component's.  The
-- list of internal components must be topologically sorted
-- based on internal package dependencies, so that any internal
-- dependency points to an entry earlier in the list.
toConfiguredComponents
    :: Bool -- use_external_internal_deps
    -> FlagAssignment
    -> Flag String -- configIPID
    -> Flag ComponentId -- configCID
    -> PackageDescription
    -> Map PackageName (ComponentId, PackageId)
    -> [Component]
    -> [ConfiguredComponent]
toConfiguredComponents
    use_external_internal_deps flags ipid_flag cid_flag pkg_descr
    external_lib_map comps
    = snd (mapAccumL go (Map.empty, Map.empty) comps)
  where
    go m component = (extendConfiguredComponentMap cc m, cc)
      where cc = toConfiguredComponent'
                        use_external_internal_deps flags pkg_descr ipid_flag cid_flag
                        external_lib_map m component


newPackageDepsBehaviourMinVersion :: Version
newPackageDepsBehaviourMinVersion = mkVersion [1,7,1]


-- In older cabal versions, there was only one set of package dependencies for
-- the whole package. In this version, we can have separate dependencies per
-- target, but we only enable this behaviour if the minimum cabal version
-- specified is >= a certain minimum. Otherwise, for compatibility we use the
-- old behaviour.
newPackageDepsBehaviour :: PackageDescription -> Bool
newPackageDepsBehaviour pkg =
   specVersion pkg >= newPackageDepsBehaviourMinVersion
