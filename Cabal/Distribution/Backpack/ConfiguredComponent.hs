{-# LANGUAGE PatternGuards #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ConfiguredComponent (
    ConfiguredComponent(..),
    cc_name,
    cc_cid,
    cc_pkgid,
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

import Distribution.Types.AnnotatedId
import Distribution.Types.LibDependency
import Distribution.Types.ExeDependency
import Distribution.Types.IncludeRenaming
import Distribution.Types.ComponentId
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Mixin
import Distribution.Types.ComponentName
import Distribution.Types.ComponentInclude
import Distribution.Package
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.Setup as Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Version
import Distribution.Utils.LogProgress
import Distribution.Utils.MapAccum
import Distribution.Utils.Generic

import Control.Monad
import qualified Data.Map as Map
import Distribution.Text
import Text.PrettyPrint

-- | A configured component, we know exactly what its 'ComponentId' is,
-- and the 'ComponentId's of the things it depends on.
data ConfiguredComponent
    = ConfiguredComponent {
        -- | Unique identifier of component, plus extra useful info.
        cc_ann_id :: AnnotatedId ComponentId,
        -- | The fragment of syntax from the Cabal file describing this
        -- component.
        cc_component :: Component,
        -- | Is this the public library component of the package?
        -- (If we invoke Setup with an instantiation, this is the
        -- component the instantiation applies to.)
        -- Note that in one-component configure mode, this is
        -- always True, because any component is the "public" one.)
        cc_public :: Bool,
        -- | Dependencies on executables from @build-tools@ and
        -- @build-tool-depends@.
        cc_exe_deps :: [AnnotatedId ComponentId],
        -- | The mixins of this package, including both explicit (from
        -- the @mixins@ field) and implicit (from @build-depends@).  Not
        -- mix-in linked yet; component configuration only looks at
        -- 'ComponentId's.
        cc_includes :: [ComponentInclude ComponentId IncludeRenaming]
      }


-- | Uniquely identifies a configured component.
cc_cid :: ConfiguredComponent -> ComponentId
cc_cid = ann_id . cc_ann_id

-- | The package this component came from.
cc_pkgid :: ConfiguredComponent -> PackageId
cc_pkgid = ann_pid . cc_ann_id

-- | The 'ComponentName' of a component; this uniquely identifies
-- a fragment of syntax within a specified Cabal file describing the
-- component.
cc_name :: ConfiguredComponent -> ComponentName
cc_name = ann_cname . cc_ann_id

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
        Map PackageName (Map ComponentName (AnnotatedId ComponentId))

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
    -> LogProgress ConfiguredComponent
toConfiguredComponent pkg_descr this_cid dep_map component = do
    let reg_lib_deps =
            if newPackageDepsBehaviour pkg_descr
            then
                [ (pn, cn)
                | LibDependency pn mb_ln _ <- targetBuildDepends bi
                , let cn = libraryComponentName mb_ln ]
            else
                -- dep_map contains a mix of internal and external deps.
                -- We want all the public libraries (dep_cn == CLibName)
                -- of all external deps (dep /= pn).  Note that this
                -- excludes the public library of the current package:
                -- this is not supported by old-style deps behavior
                -- because it would imply a cyclic dependency for the
                -- library itself.
                [ (pn, cn)
                | (pn, comp_map) <- Map.toList dep_map
                , pn /= packageName pkg_descr
                , (cn, _) <- Map.toList comp_map
                , cn == CLibName ]

        reg_lib_map, mixin_map :: Map (PackageName, ComponentName) (IncludeRenaming, Bool)

        reg_lib_map = Map.fromList $
            reg_lib_deps `zip` repeat (defaultIncludeRenaming, True)

        mixin_map = Map.fromList
            [ ((pn, cn), (rns, False))
            | Mixin pn mb_ln rns <- mixins bi
            , let cn = libraryComponentName mb_ln ]

        lib_deps = Map.toList $ reg_lib_map `Map.union` mixin_map

    mixin_includes <- forM lib_deps $ \((pname, cname), (rns, implicit)) -> do
        aid <- case Map.lookup cname =<< Map.lookup pname dep_map of
            Nothing -> dieProgress $
                text "Dependency on unbuildable" <+>
                text (showComponentName cname) <+>
                text "from" <+> disp pname
            Just r  -> return r
        return ComponentInclude {
                ci_ann_id   = aid,
                ci_renaming = rns,
                ci_implicit = implicit
            }

    return ConfiguredComponent {
            cc_ann_id = AnnotatedId {
                    ann_id = this_cid,
                    ann_pid = package pkg_descr,
                    ann_cname = componentName component
                },
            cc_component = component,
            cc_public = componentName component == CLibName,
            cc_exe_deps = exe_deps,
            cc_includes = mixin_includes
        }
  where
    bi = componentBuildInfo component
    -- We have to nub here, because 'getAllToolDependencies' may return
    -- duplicates (see #4986).  (NB: This is not needed for lib_deps,
    -- since those elaborate into includes, for which there explicitly
    -- may be multiple instances of a package)
    exe_deps = ordNub $
        [ exe
        | ExeDependency pn cn _ <- getAllToolDependencies pkg_descr bi
        -- The error suppression here is important, because in general
        -- we won't know about external dependencies (e.g., 'happy')
        -- which the package is attempting to use (those deps are only
        -- fed in when cabal-install uses this codepath.)
        -- TODO: Let cabal-install request errors here
        , Just exe <- [Map.lookup (CExeName cn) =<< Map.lookup pn dep_map]
        ]

-- | Also computes the 'ComponentId', and sets cc_public if necessary.
-- This is Cabal-only; cabal-install won't use this.
toConfiguredComponent'
    :: Bool -- use_external_internal_deps
    -> FlagAssignment
    -> PackageDescription
    -> Bool -- deterministic
    -> Flag String      -- configIPID (todo: remove me)
    -> Flag ComponentId -- configCID
    -> ConfiguredComponentMap
    -> Component
    -> LogProgress ConfiguredComponent
toConfiguredComponent' use_external_internal_deps flags
                pkg_descr deterministic ipid_flag cid_flag
                dep_map component = do
    cc <- toConfiguredComponent
                pkg_descr this_cid
                dep_map component
    return $ if use_external_internal_deps
                then cc { cc_public = True }
                else cc
  where
    -- TODO: pass component names to it too!
    this_cid = computeComponentId deterministic ipid_flag cid_flag (package pkg_descr)
                (componentName component) (Just (deps, flags))
    deps = [ ann_id aid | m <- Map.elems dep_map
                        , aid <- Map.elems m ]

extendConfiguredComponentMap
    :: ConfiguredComponent
    -> ConfiguredComponentMap
    -> ConfiguredComponentMap
extendConfiguredComponentMap cc =
    Map.insertWith Map.union
        (pkgName (cc_pkgid cc))
        (Map.singleton (cc_name cc) (cc_ann_id cc))

-- Compute the 'ComponentId's for a graph of 'Component's.  The
-- list of internal components must be topologically sorted
-- based on internal package dependencies, so that any internal
-- dependency points to an entry earlier in the list.
toConfiguredComponents
    :: Bool -- use_external_internal_deps
    -> FlagAssignment
    -> Bool -- deterministic
    -> Flag String -- configIPID
    -> Flag ComponentId -- configCID
    -> PackageDescription
    -> ConfiguredComponentMap
    -> [Component]
    -> LogProgress [ConfiguredComponent]
toConfiguredComponents
    use_external_internal_deps flags deterministic ipid_flag cid_flag pkg_descr
    dep_map comps
    = fmap snd (mapAccumM go dep_map comps)
  where
    go m component = do
        cc <- toConfiguredComponent'
                        use_external_internal_deps flags pkg_descr
                        deterministic ipid_flag cid_flag
                        m component
        return (extendConfiguredComponentMap cc m, cc)

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
