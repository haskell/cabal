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

import Distribution.Types.Dependency
import Distribution.Types.LegacyExeDependency
import Distribution.Types.IncludeRenaming
import Distribution.Types.Mixin
import Distribution.Types.UnqualComponentName
import Distribution.Types.ComponentInclude
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
        -- | Uniquely identifies a configured component.
        cc_cid :: ComponentId,
        -- | The package this component came from.
        cc_pkgid :: PackageId,
        -- | The fragment of syntax from the Cabal file describing this
        -- component.
        cc_component :: Component,
        -- | Is this the public library component of the package?
        -- (If we invoke Setup with an instantiation, this is the
        -- component the instantiation applies to.)
        -- Note that in one-component configure mode, this is
        -- always True, because any component is the "public" one.)
        cc_public :: Bool,
        -- | Dependencies on internal executables from @build-tools@.
        cc_internal_build_tools :: [ComponentId],
        -- | The mixins of this package, including both explicit (from
        -- the @mixins@ field) and implicit (from @build-depends@).  Not
        -- mix-in linked yet; component configuration only looks at
        -- 'ComponentId's.
        cc_includes :: [ComponentInclude ComponentId IncludeRenaming]
      }

-- | The 'ComponentName' of a component; this uniquely identifies
-- a fragment of syntax within a specified Cabal file describing the
-- component.
cc_name :: ConfiguredComponent -> ComponentName
cc_name = componentName . cc_component

-- | Pretty-print a 'ConfiguredComponent'.
dispConfiguredComponent :: ConfiguredComponent -> Doc
dispConfiguredComponent cc =
    hang (text "component" <+> disp (cc_cid cc)) 4
         (vcat [ hsep $ [ text "include", disp (ci_id incl), disp (ci_renaming incl) ]
               | incl <- cc_includes cc
               ])

-- | This is a mapping that keeps track of package-internal libraries
-- and executables.  Although a component of the key is a general
-- 'ComponentName', actually only 'CLib', 'CSubLib' and 'CExe' will ever
-- be here.
type ConfiguredComponentMap =
        Map (PackageName, ComponentName) (ComponentId, PackageId)

-- Executable map must be different because an executable can
-- have the same name as a library. Ew.

-- | Given some ambient environment of package names that
-- are "in scope", looks at the 'BuildInfo' to decide
-- what the packages actually resolve to, and then builds
-- a 'ConfiguredComponent'.
toConfiguredComponent
    :: PackageDescription
    -> ComponentId
    -> ConfiguredComponentMap
    -> Component
    -> ConfiguredComponent
toConfiguredComponent pkg_descr this_cid deps_map component =
    ConfiguredComponent {
        cc_cid = this_cid,
        cc_pkgid = package pkg_descr,
        cc_component = component,
        cc_public = is_public,
        cc_internal_build_tools = exe_deps,
        cc_includes = explicit_includes ++ implicit_includes
    }
  where
    pn = packageName pkg_descr
    bi = componentBuildInfo component

    -- Resolve each @backpack-include@ into the actual dependency
    -- from @lib_deps@.
    explicit_includes
        = [ let cname = maybe CLibName CSubLibName mb_lib_name
                (cid, pid) = case Map.lookup (name, cname) deps_map of
                    -- TODO: give a better error message here if the
                    -- *package* exists, but doesn't have this
                    -- component.
                    Nothing ->
                        error $ "Mix-in refers to non-existent component " ++ display cname ++
                                " in " ++ display name ++
                                " (did you forget to add the package to build-depends?)"
                    Just r -> r
            in ComponentInclude {
                ci_id       = cid,
                ci_pkgid    = pid,
                ci_renaming = rns
               }
          | Mixin name mb_lib_name rns <- mixins bi ]

    -- Any @build-depends@ which is not explicitly mentioned in
    -- @backpack-include@ is converted into an "implicit" include.
    -- NB: This INCLUDES if you depend pkg:sublib (because other way
    -- there's no way to depend on a sublib without depending on the
    -- main library as well).
    used_explicitly = Set.fromList (map (packageName . ci_pkgid) explicit_includes)
    lib_deps
        | newPackageDepsBehaviour pkg_descr
        = [ case Map.lookup (name, CLibName) deps_map of
                Nothing ->
                    error ("toConfiguredComponent: " ++ display (packageName pkg_descr) ++
                            " " ++ display name)
                Just r -> r
          | Dependency name _ <- targetBuildDepends bi
          , Set.notMember name used_explicitly ]
        | otherwise
        -- deps_map contains a mix of internal and external deps.
        -- We want all the public libraries (dep_cn == CLibName)
        -- of all external deps (dep /= pn).  Note that this
        -- excludes the public library of the current package:
        -- this is not supported by old-style deps behavior
        -- because it would imply a cyclic dependency for the
        -- library itself.
        = [ r
          | ((dep_pn,dep_cn), r) <- Map.toList deps_map
          , dep_pn /= pn
          , dep_cn == CLibName
          , Set.notMember dep_pn used_explicitly ]
    implicit_includes
        = map (\(cid, pid) ->
                    ComponentInclude {
                        ci_id       = cid,
                        ci_pkgid    = pid,
                        ci_renaming = defaultIncludeRenaming
                    }) lib_deps

    exe_deps = [ cid
               | LegacyExeDependency name _ <- buildTools bi
               , let cn = CExeName (mkUnqualComponentName name)
               -- NB: we silently swallow non-existent build-tools,
               -- because historically they did not have to correspond
               -- to Haskell executables.
               , Just (cid, _) <- [ Map.lookup (pn, cn) deps_map ] ]

    is_public = componentName component == CLibName

-- | Also computes the 'ComponentId', and sets cc_public if necessary.
-- This is Cabal-only; cabal-install won't use this.
toConfiguredComponent'
    :: Bool -- use_external_internal_deps
    -> FlagAssignment
    -> PackageDescription
    -> Flag String      -- configIPID (todo: remove me)
    -> Flag ComponentId -- configCID
    -> ConfiguredComponentMap
    -> Component
    -> ConfiguredComponent
toConfiguredComponent' use_external_internal_deps flags
                pkg_descr ipid_flag cid_flag
                deps_map component =
    let cc = toConfiguredComponent
                pkg_descr this_cid
                deps_map component
    in if use_external_internal_deps
        then cc { cc_public = True }
        else cc
  where
    this_cid = computeComponentId ipid_flag cid_flag (package pkg_descr)
                (componentName component) (Just (deps, flags))
    deps = [ cid | ((dep_pn, _), (cid, _)) <- Map.toList deps_map
                 , dep_pn /= packageName pkg_descr ]

extendConfiguredComponentMap
    :: ConfiguredComponent
    -> ConfiguredComponentMap
    -> ConfiguredComponentMap
extendConfiguredComponentMap cc deps_map =
    Map.insert (pkgName (cc_pkgid cc), cc_name cc) (cc_cid cc, cc_pkgid cc) deps_map

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
    -> ConfiguredComponentMap
    -> [Component]
    -> [ConfiguredComponent]
toConfiguredComponents
    use_external_internal_deps flags ipid_flag cid_flag pkg_descr
    deps_map comps
    = snd (mapAccumL go deps_map comps)
  where
    go m component = (extendConfiguredComponentMap cc m, cc)
      where cc = toConfiguredComponent'
                        use_external_internal_deps flags pkg_descr ipid_flag cid_flag
                        m component


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
