{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}

-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
--
-- WARNING: The contents of this module are HIGHLY experimental.
-- We may refactor it under you.
module Distribution.Backpack.Configure
  ( configureComponentLocalBuildInfos
  ) where

import Distribution.Compat.Prelude hiding ((<>))
import Prelude ()

import Distribution.Backpack
import Distribution.Backpack.ComponentsGraph
import Distribution.Backpack.ConfiguredComponent
import Distribution.Backpack.FullUnitId
import Distribution.Backpack.Id
import Distribution.Backpack.LinkedComponent
import Distribution.Backpack.PreExistingComponent
import Distribution.Backpack.ReadyComponent

import Distribution.Backpack.ModuleShape
import Distribution.Compat.Graph (Graph, IsNode (..))
import qualified Distribution.Compat.Graph as Graph
import Distribution.InstalledPackageInfo
  ( InstalledPackageInfo
  , emptyInstalledPackageInfo
  )
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Types.AnnotatedId
import Distribution.Types.ComponentInclude
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.MungedPackageName
import Distribution.Utils.LogProgress
import Distribution.Verbosity

import Data.Either
  ( lefts
  )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Pretty
import Text.PrettyPrint

------------------------------------------------------------------------------
-- Pipeline
------------------------------------------------------------------------------

configureComponentLocalBuildInfos
  :: Verbosity
  -> Bool -- use_external_internal_deps
  -> ComponentRequestedSpec
  -> Bool -- deterministic
  -> Flag String -- configIPID
  -> Flag ComponentId -- configCID
  -> PackageDescription
  -> ([PreExistingComponent], [PromisedComponent])
  -> FlagAssignment -- configConfigurationsFlags
  -> [(ModuleName, Module)] -- configInstantiateWith
  -> InstalledPackageIndex
  -> Compiler
  -> LogProgress ([ComponentLocalBuildInfo], InstalledPackageIndex)
configureComponentLocalBuildInfos
  verbosity
  use_external_internal_deps
  enabled
  deterministic
  ipid_flag
  cid_flag
  pkg_descr
  (prePkgDeps, promisedPkgDeps)
  flagAssignment
  instantiate_with
  installedPackageSet
  comp = do
    -- NB: In single component mode, this returns a *single* component.
    -- In this graph, the graph is NOT closed.
    graph0 <- case mkComponentsGraph enabled pkg_descr of
      Left ccycle -> dieProgress (componentCycleMsg (package pkg_descr) ccycle)
      Right g -> return (componentsGraphToList g)
    infoProgress $
      hang
        (text "Source component graph:")
        4
        (dispComponentsWithDeps graph0)

    let conf_pkg_map =
          Map.fromListWith
            Map.union
            [ ( pc_pkgname pkg
              , Map.singleton
                  (pc_compname pkg)
                  ( AnnotatedId
                      { ann_id = pc_cid pkg
                      , ann_pid = packageId pkg
                      , ann_cname = pc_compname pkg
                      }
                  )
              )
            | pkg <- prePkgDeps
            ]
            `Map.union` Map.fromListWith
              Map.union
              [ (pkg, Map.singleton (ann_cname aid) aid)
              | PromisedComponent pkg aid <- promisedPkgDeps
              ]
    graph1 <-
      toConfiguredComponents
        use_external_internal_deps
        flagAssignment
        deterministic
        ipid_flag
        cid_flag
        pkg_descr
        conf_pkg_map
        (map fst graph0)
    infoProgress $
      hang
        (text "Configured component graph:")
        4
        (vcat (map dispConfiguredComponent graph1))

    let shape_pkg_map =
          Map.fromList
            [ (pc_cid pkg, (pc_open_uid pkg, pc_shape pkg))
            | pkg <- prePkgDeps
            ]
            `Map.union` Map.fromList
              [ ( ann_id aid
                ,
                  ( DefiniteUnitId
                      ( unsafeMkDefUnitId
                          (mkUnitId (unComponentId (ann_id aid)))
                      )
                  , emptyModuleShape
                  )
                )
              | PromisedComponent _ aid <- promisedPkgDeps
              ]
        uid_lookup def_uid
          | Just pkg <- PackageIndex.lookupUnitId installedPackageSet uid =
              FullUnitId
                (Installed.installedComponentId pkg)
                (Map.fromList (Installed.instantiatedWith pkg))
          | otherwise = error ("uid_lookup: " ++ prettyShow uid)
          where
            uid = unDefUnitId def_uid
    graph2 <-
      toLinkedComponents
        verbosity
        (not (null promisedPkgDeps))
        uid_lookup
        (package pkg_descr)
        shape_pkg_map
        graph1

    infoProgress $
      hang
        (text "Linked component graph:")
        4
        (vcat (map dispLinkedComponent graph2))

    let pid_map =
          Map.fromList $
            [ (pc_uid pkg, pc_munged_id pkg)
            | pkg <- prePkgDeps
            ]
              ++ [ (Installed.installedUnitId pkg, mungedId pkg)
                 | (_, Module uid _) <- instantiate_with
                 , Just pkg <-
                    [ PackageIndex.lookupUnitId
                        installedPackageSet
                        (unDefUnitId uid)
                    ]
                 ]
        subst = Map.fromList instantiate_with
        graph3 = toReadyComponents pid_map subst graph2
        graph4 = Graph.revTopSort (Graph.fromDistinctList graph3)

    infoProgress $
      hang
        (text "Ready component graph:")
        4
        (vcat (map dispReadyComponent graph4))

    toComponentLocalBuildInfos comp installedPackageSet promisedPkgDeps pkg_descr prePkgDeps graph4

------------------------------------------------------------------------------
-- ComponentLocalBuildInfo
------------------------------------------------------------------------------

toComponentLocalBuildInfos
  :: Compiler
  -> InstalledPackageIndex -- FULL set
  -> [PromisedComponent]
  -> PackageDescription
  -> [PreExistingComponent] -- external package deps
  -> [ReadyComponent]
  -> LogProgress
      ( [ComponentLocalBuildInfo]
      , InstalledPackageIndex -- only relevant packages
      )
toComponentLocalBuildInfos
  comp
  installedPackageSet
  promisedPkgDeps
  pkg_descr
  externalPkgDeps
  graph = do
    -- Check and make sure that every instantiated component exists.
    -- We have to do this now, because prior to linking/instantiating
    -- we don't actually know what the full set of 'UnitId's we need
    -- are.
    let
      -- TODO: This is actually a bit questionable performance-wise,
      -- since we will pay for the ALL installed packages even if
      -- they are not related to what we are building.  This was true
      -- in the old configure code.
      external_graph :: Graph (Either InstalledPackageInfo ReadyComponent)
      external_graph =
        Graph.fromDistinctList
          . map Left
          $ PackageIndex.allPackages installedPackageSet
      internal_graph :: Graph (Either InstalledPackageInfo ReadyComponent)
      internal_graph =
        Graph.fromDistinctList
          . map Right
          $ graph
      combined_graph = Graph.unionRight external_graph internal_graph
      local_graph =
        fromMaybe (error "toComponentLocalBuildInfos: closure returned Nothing") $
          Graph.closure combined_graph (map nodeKey graph)
      -- The database of transitively reachable installed packages that the
      -- external components the package (as a whole) depends on.  This will be
      -- used in several ways:
      --
      --      * We'll use it to do a consistency check so we're not depending
      --        on multiple versions of the same package (TODO: someday relax
      --        this for private dependencies.)  See right below.
      --
      --      * We'll pass it on in the LocalBuildInfo, where preprocessors
      --        and other things will incorrectly use it to determine what
      --        the include paths and everything should be.
      --
      packageDependsIndex = PackageIndex.fromList (lefts local_graph)
      fullIndex = Graph.fromDistinctList local_graph

    case Graph.broken fullIndex of
      [] -> return ()
      -- If there are promised dependencies, we don't know what the dependencies
      -- of these are and that can easily lead to a broken graph. So assume that
      -- any promised package is not broken (ie all its dependencies, transitively,
      -- will be there). That's a promise.
      broken
        | not (null promisedPkgDeps) -> return ()
        | otherwise ->
            -- TODO: ppr this
            dieProgress . text $
              "The following packages are broken because other"
                ++ " packages they depend on are missing. These broken "
                ++ "packages must be rebuilt before they can be used.\n"
                -- TODO: Undupe.
                ++ unlines
                  [ "installed package "
                    ++ prettyShow (packageId pkg)
                    ++ " is broken due to missing package "
                    ++ intercalate ", " (map prettyShow deps)
                  | (Left pkg, deps) <- broken
                  ]
                ++ unlines
                  [ "planned package "
                    ++ prettyShow (packageId pkg)
                    ++ " is broken due to missing package "
                    ++ intercalate ", " (map prettyShow deps)
                  | (Right pkg, deps) <- broken
                  ]

    -- In this section, we'd like to look at the 'packageDependsIndex'
    -- and see if we've picked multiple versions of the same
    -- installed package (this is bad, because it means you might
    -- get an error could not match foo-0.1:Type with foo-0.2:Type).
    --
    -- What is pseudoTopPkg for? I have no idea.  It was used
    -- in the very original commit which introduced checking for
    -- inconsistencies 5115bb2be4e13841ea07dc9166b9d9afa5f0d012,
    -- and then moved out of PackageIndex and put here later.
    -- TODO: Try this code without it...
    --
    -- TODO: Move this into a helper function
    --
    -- TODO: This is probably wrong for Backpack
    let pseudoTopPkg :: InstalledPackageInfo
        pseudoTopPkg =
          emptyInstalledPackageInfo
            { Installed.installedUnitId = mkLegacyUnitId (packageId pkg_descr)
            , Installed.sourcePackageId = packageId pkg_descr
            , Installed.depends = map pc_uid externalPkgDeps
            }
    case PackageIndex.dependencyInconsistencies
      . PackageIndex.insert pseudoTopPkg
      $ packageDependsIndex of
      [] -> return ()
      inconsistencies ->
        warnProgress $
          hang
            ( text "This package indirectly depends on multiple versions of the same"
                <+> text "package. This is very likely to cause a compile failure."
            )
            2
            ( vcat
                [ text "package"
                  <+> pretty (packageName user)
                  <+> parens (pretty (installedUnitId user))
                  <+> text "requires"
                  <+> pretty inst
                | (_dep_key, insts) <- inconsistencies
                , (inst, users) <- insts
                , user <- users
                ]
            )
    let clbis = mkLinkedComponentsLocalBuildInfo comp graph
    -- forM clbis $ \(clbi,deps) -> info verbosity $ "UNIT" ++ hashUnitId (componentUnitId clbi) ++ "\n" ++ intercalate "\n" (map hashUnitId deps)
    return (clbis, packageDependsIndex)

-- Build ComponentLocalBuildInfo for each component we are going
-- to build.
--
-- This conversion is lossy; we lose some invariants from ReadyComponent
mkLinkedComponentsLocalBuildInfo
  :: Compiler
  -> [ReadyComponent]
  -> [ComponentLocalBuildInfo]
mkLinkedComponentsLocalBuildInfo comp rcs = map go rcs
  where
    internalUnits = Set.fromList (map rc_uid rcs)
    isInternal x = Set.member x internalUnits
    go rc =
      case rc_component rc of
        CLib lib ->
          let convModuleExport (modname', (Module uid modname))
                | this_uid == unDefUnitId uid
                , modname' == modname =
                    Installed.ExposedModule modname' Nothing
                | otherwise =
                    Installed.ExposedModule
                      modname'
                      (Just (OpenModule (DefiniteUnitId uid) modname))
              convOpenModuleExport (modname', modu@(OpenModule uid modname))
                | uid == this_open_uid
                , modname' == modname =
                    Installed.ExposedModule modname' Nothing
                | otherwise =
                    Installed.ExposedModule modname' (Just modu)
              convOpenModuleExport (_, OpenModuleVar _) =
                error "convOpenModuleExport: top-level modvar"
              exports =
                -- Loses invariants
                case rc_i rc of
                  Left indefc ->
                    map convOpenModuleExport $
                      Map.toList (indefc_provides indefc)
                  Right instc ->
                    map convModuleExport $
                      Map.toList (instc_provides instc)
              insts =
                case rc_i rc of
                  Left indefc -> [(m, OpenModuleVar m) | m <- indefc_requires indefc]
                  Right instc ->
                    [ (m, OpenModule (DefiniteUnitId uid') m')
                    | (m, Module uid' m') <- instc_insts instc
                    ]

              compat_name = MungedPackageName (packageName rc) (libName lib)
              compat_key = computeCompatPackageKey comp compat_name (packageVersion rc) this_uid
           in LibComponentLocalBuildInfo
                { componentPackageDeps = cpds
                , componentUnitId = this_uid
                , componentComponentId = this_cid
                , componentInstantiatedWith = insts
                , componentIsIndefinite_ = is_indefinite
                , componentLocalName = cname
                , componentInternalDeps = internal_deps
                , componentExeDeps = exe_deps
                , componentIncludes = includes
                , componentExposedModules = exports
                , componentIsPublic = rc_public rc
                , componentCompatPackageKey = compat_key
                , componentCompatPackageName = compat_name
                }
        CFLib _ ->
          FLibComponentLocalBuildInfo
            { componentUnitId = this_uid
            , componentComponentId = this_cid
            , componentLocalName = cname
            , componentPackageDeps = cpds
            , componentExeDeps = exe_deps
            , componentInternalDeps = internal_deps
            , componentIncludes = includes
            }
        CExe _ ->
          ExeComponentLocalBuildInfo
            { componentUnitId = this_uid
            , componentComponentId = this_cid
            , componentLocalName = cname
            , componentPackageDeps = cpds
            , componentExeDeps = exe_deps
            , componentInternalDeps = internal_deps
            , componentIncludes = includes
            }
        CTest _ ->
          TestComponentLocalBuildInfo
            { componentUnitId = this_uid
            , componentComponentId = this_cid
            , componentLocalName = cname
            , componentPackageDeps = cpds
            , componentExeDeps = exe_deps
            , componentInternalDeps = internal_deps
            , componentIncludes = includes
            }
        CBench _ ->
          BenchComponentLocalBuildInfo
            { componentUnitId = this_uid
            , componentComponentId = this_cid
            , componentLocalName = cname
            , componentPackageDeps = cpds
            , componentExeDeps = exe_deps
            , componentInternalDeps = internal_deps
            , componentIncludes = includes
            }
      where
        this_uid = rc_uid rc
        this_open_uid = rc_open_uid rc
        this_cid = rc_cid rc
        cname = componentName (rc_component rc)
        cpds = rc_depends rc
        exe_deps = map ann_id $ rc_exe_deps rc
        is_indefinite =
          case rc_i rc of
            Left _ -> True
            Right _ -> False
        includes =
          map (\ci -> (ci_id ci, ci_renaming ci)) $
            case rc_i rc of
              Left indefc ->
                indefc_includes indefc
              Right instc ->
                map
                  (\ci -> ci{ci_ann_id = fmap DefiniteUnitId (ci_ann_id ci)})
                  (instc_includes instc)
        internal_deps = filter isInternal (nodeNeighbors rc)
