{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

module Distribution.Client.ProjectPlanOutput (
    writePlanExternalRepresentation,
    writePlanGhcEnvironment,
  ) where

import           Distribution.Client.ProjectPlanning.Types
import           Distribution.Client.DistDirLayout
import           Distribution.Client.Types

import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.Utils.Json as J
import qualified Distribution.Simple.InstallDirs as InstallDirs

import qualified Distribution.Solver.Types.ComponentDeps as ComponentDeps

import           Distribution.Package
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.PackageDescription as PD
import           Distribution.Compiler (CompilerFlavor(GHC))
import           Distribution.Simple.Compiler
                   ( PackageDBStack, PackageDB(..)
                   , compilerVersion, compilerFlavor )
import           Distribution.Simple.GHC
                   ( getImplInfo, GhcImplInfo(supportsPkgEnvFiles)
                   , GhcEnvironmentFileEntry(..), simpleGhcEnvironmentFile
                   , writeGhcEnvironmentFile )
import           Distribution.Text
import qualified Distribution.Compat.Graph as Graph
import           Distribution.Simple.Utils
import qualified Paths_cabal_install as Our (version)

import           Data.Maybe (maybeToList)
import           Data.Monoid
import qualified Data.ByteString.Builder as BB
import           System.FilePath


-----------------------------------------------------------------------------
-- Writing plan.json files
--

-- | Write out a representation of the elaborated install plan.
--
-- This is for the benefit of debugging and external tools like editors.
--
writePlanExternalRepresentation :: DistDirLayout
                                -> ElaboratedInstallPlan
                                -> ElaboratedSharedConfig
                                -> IO ()
writePlanExternalRepresentation distDirLayout elaboratedInstallPlan
                                elaboratedSharedConfig =
    writeFileAtomic (distProjectCacheFile distDirLayout "plan.json") $
        BB.toLazyByteString
      . J.encodeToBuilder
      $ encodePlanAsJson distDirLayout elaboratedInstallPlan elaboratedSharedConfig

-- | Renders a subset of the elaborated install plan in a semi-stable JSON
-- format.
--
encodePlanAsJson :: DistDirLayout -> ElaboratedInstallPlan -> ElaboratedSharedConfig -> J.Value
encodePlanAsJson distDirLayout elaboratedInstallPlan elaboratedSharedConfig =
    --TODO: [nice to have] include all of the sharedPackageConfig and all of
    --      the parts of the elaboratedInstallPlan
    J.object [ "cabal-version"     J..= jdisplay Our.version
             , "cabal-lib-version" J..= jdisplay cabalVersion
             , "install-plan"      J..= installPlanToJ elaboratedInstallPlan
             ]
  where
    installPlanToJ :: ElaboratedInstallPlan -> [J.Value]
    installPlanToJ = map planPackageToJ . InstallPlan.toList

    planPackageToJ :: ElaboratedPlanPackage -> J.Value
    planPackageToJ pkg =
      case pkg of
        InstallPlan.PreExisting ipi -> installedPackageInfoToJ ipi
        InstallPlan.Configured elab -> elaboratedPackageToJ False elab
        InstallPlan.Installed  elab -> elaboratedPackageToJ True  elab

    installedPackageInfoToJ :: InstalledPackageInfo -> J.Value
    installedPackageInfoToJ ipi =
      -- Pre-existing packages lack configuration information such as their flag
      -- settings or non-lib components. We only get pre-existing packages for
      -- the global/core packages however, so this isn't generally a problem.
      -- So these packages are never local to the project.
      --
      J.object
        [ "type"       J..= J.String "pre-existing"
        , "id"         J..= jdisplay (installedUnitId ipi)
        , "depends" J..= map jdisplay (installedDepends ipi)
        ]

    elaboratedPackageToJ :: Bool -> ElaboratedConfiguredPackage -> J.Value
    elaboratedPackageToJ isInstalled elab =
      J.object $
        [ "type"       J..= J.String (if isInstalled then "installed"
                                                     else "configured")
        , "id"         J..= (jdisplay . installedUnitId) elab
        , "flags"      J..= J.object [ fn J..= v
                                     | (PD.FlagName fn,v) <-
                                            elabFlagAssignment elab ]
        , "style"      J..= J.String (style2str (elabLocalToProject elab) (elabBuildStyle elab))
        ] ++
        (case elabBuildStyle elab of
            BuildInplaceOnly ->
                ["dist-dir"   J..= J.String dist_dir]
            BuildAndInstall ->
                -- TODO: install dirs?
                []
            ) ++
        case elabPkgOrComp elab of
          ElabPackage pkg ->
            let components = J.object $
                  [ comp2str c J..= (J.object $
                    [ "depends"     J..= map (jdisplay . confInstId) ldeps
                    , "exe-depends" J..= map (jdisplay . confInstId) edeps ] ++
                    bin_file c)
                  | (c,(ldeps,edeps))
                      <- ComponentDeps.toList $
                         ComponentDeps.zip (pkgLibDependencies pkg)
                                           (pkgExeDependencies pkg) ]
            in ["components" J..= components]
          ElabComponent comp ->
            ["depends"     J..= map (jdisplay . confInstId) (elabLibDependencies elab)
            ,"exe-depends" J..= map jdisplay (elabExeDependencies elab)
            ,"component-name" J..= J.String (comp2str (compSolverName comp))
            ] ++
            bin_file (compSolverName comp)
     where
      dist_dir = distBuildDirectory distDirLayout
                    (elabDistDirParams elaboratedSharedConfig elab)

      bin_file c = case c of
        ComponentDeps.ComponentExe s   -> bin_file' s
        ComponentDeps.ComponentTest s  -> bin_file' s
        ComponentDeps.ComponentBench s -> bin_file' s
        _ -> []
      bin_file' s =
        ["bin-file" J..= J.String bin]
       where
        bin = if elabBuildStyle elab == BuildInplaceOnly
               then dist_dir </> "build" </> s </> s
               else InstallDirs.bindir (elabInstallDirs elab) </> s

    -- TODO: maybe move this helper to "ComponentDeps" module?
    --       Or maybe define a 'Text' instance?
    comp2str :: ComponentDeps.Component -> String
    comp2str c = case c of
        ComponentDeps.ComponentLib     -> "lib"
        ComponentDeps.ComponentSubLib s -> "lib:"   <> s
        ComponentDeps.ComponentExe s   -> "exe:"   <> s
        ComponentDeps.ComponentTest s  -> "test:"  <> s
        ComponentDeps.ComponentBench s -> "bench:" <> s
        ComponentDeps.ComponentSetup   -> "setup"

    style2str :: Bool -> BuildStyle -> String
    style2str True  _                = "local"
    style2str False BuildInplaceOnly = "inplace"
    style2str False BuildAndInstall  = "global"

    jdisplay :: Text a => a -> J.Value
    jdisplay = J.String . display


-----------------------------------------------------------------------------
-- Writing .ghc.environment files
--

writePlanGhcEnvironment :: FilePath
                        -> ElaboratedInstallPlan
                        -> ElaboratedSharedConfig
                        -> IO ()
writePlanGhcEnvironment projectRootDir
                        elaboratedInstallPlan
                        ElaboratedSharedConfig {
                          pkgConfigCompiler = compiler,
                          pkgConfigPlatform = platform
                        }
  | compilerFlavor compiler == GHC
  , supportsPkgEnvFiles (getImplInfo compiler)
  --TODO: check ghcjs compat
  = writeGhcEnvironmentFile
      projectRootDir
      platform (compilerVersion compiler)
      (renderGhcEnviromentFile projectRootDir elaboratedInstallPlan)
    --TODO: [required eventually] support for writing user-wide package
    -- environments, e.g. like a global project, but we would not put the
    -- env file in the home dir, rather it lives under ~/.ghc/

writePlanGhcEnvironment _ _ _ = return ()

renderGhcEnviromentFile :: FilePath
                        -> ElaboratedInstallPlan
                        -> [GhcEnvironmentFileEntry]
renderGhcEnviromentFile projectRootDir elaboratedInstallPlan =
    headerComment
  : simpleGhcEnvironmentFile packageDBs unitIds
  where
    headerComment =
        GhcEnvFileComment
      $ "This is a GHC environment file written by cabal. This means you can\n"
     ++ "run ghc or ghci and get the environment of the project as a whole.\n"
     ++ "But you still need to use cabal repl $target to get the environment\n"
     ++ "of specific components (libs, exes, tests etc) because each one can\n"
     ++ "have its own source dirs, cpp flags etc.\n\n"
    packageDBs =
        relativePackageDBPaths projectRootDir $
        -- If we have any inplace packages then their package db stack is the
        -- one we should use since it'll include the store + the local db but
        -- it's certainly possible to have no local inplace packages
        -- e.g. just "extra" packages coming from the store.
        case (inplacePackages, configuredPackages) of
          ([], pkgs) -> checkSamePackageDBs pkgs
          (pkgs, _)  -> checkSamePackageDBs pkgs
      where
        checkSamePackageDBs pkgs =
          case ordNub (map elabBuildPackageDBStack pkgs) of
            [packageDbs] -> packageDbs
            []           -> []
            _            -> error $ "renderGhcEnviromentFile: packages with "
                                 ++ "different package db stacks"
            -- This should not happen at the moment but will happen as soon
            -- as we support projects where we build packages with different
            -- compilers, at which point we have to consider how to adapt
            -- this feature, e.g. write out multiple env files, one for each
            -- compiler / project profile.

        inplacePackages =
          [ srcpkg
          | srcpkg <- configuredPackages
          , elabBuildStyle srcpkg == BuildInplaceOnly ]
        configuredPackages =
          [ srcpkg
          | pkg <- InstallPlan.toList elaboratedInstallPlan
          , srcpkg <- maybeToList $ case pkg of
                        InstallPlan.Configured srcpkg -> Just srcpkg
                        InstallPlan.Installed  srcpkg -> Just srcpkg
                        InstallPlan.PreExisting _     -> Nothing
          ]

    -- We're producing an environment for users to use in ghci, so of course
    -- that means libraries only (can't put exes into the ghc package env!).
    -- The library environment should be /consistent/ with the environment
    -- that each of the packages in the project use (ie same lib versions).
    -- So that means all the normal library dependencies of all the things
    -- in the project (including deps of exes that are local to the project).
    -- We do not however want to include the depencencies of Setup.hs scripts,
    -- since these are generally uninteresting but also they need not in
    -- general be consistent with the library versions that packages local to
    -- the project use (recall that Setup.hs script's deps can be picked
    -- independently of other packages in the project).
    --
    -- So, our strategy is as follows:
    --
    -- produce a dependency graph of all the packages in the install plan,
    -- but only consider normal library deps as edges in the graph. Thus we
    -- exclude the depencencies on Setup.hs scripts (in the case of
    -- per-component granularity) or of Setup.hs scripts (in the case of
    -- per-package granularity). Then take a dependency closure, using as
    -- roots all the packages/components local to the project. This will
    -- exclude Setup scripts and their depencencies.
    --
    -- Note: this algorithm will have to be adapted if/when the install plan
    -- is extended to cover multiple compilers at once, and may also have to
    -- change if we start to treat unshared deps of test suites in a similar
    -- way to how we treat Setup.hs script deps (ie being able to pick them
    -- independently).
    --
    libdepgraph :: Graph.Graph (Graph.Node UnitId ElaboratedPlanPackage)
    libdepgraph =
      Graph.fromList
        [ Graph.N pkg (installedUnitId pkg) libdeps
        | pkg <- InstallPlan.toList elaboratedInstallPlan
        , let libdeps = case pkg of
                InstallPlan.PreExisting ipkg  -> installedDepends ipkg
                InstallPlan.Configured srcpkg -> map (SimpleUnitId . confInstId)
                                                 (elabLibDependencies srcpkg)
                InstallPlan.Installed  srcpkg -> map (SimpleUnitId . confInstId)
                                                 (elabLibDependencies srcpkg)
        ]
    localpkgs =
      [ installedUnitId pkg
      | pkg <- InstallPlan.toList elaboratedInstallPlan
      , case pkg of
          InstallPlan.PreExisting _ -> False
          InstallPlan.Configured srcpkg -> elabLocalToProject srcpkg
          InstallPlan.Installed  srcpkg -> elabLocalToProject srcpkg
      ]

    -- Since we had to use all the local packages, including exes, (as roots
    -- to find the libs) then those exes still end up in our list so we have
    -- to filter them out at the end.
    unitIds =
      case Graph.closure libdepgraph localpkgs of
        Nothing    -> error "renderGhcEnviromentFile: broken dep closure"
        Just nodes ->
          [ pkgid
          | Graph.N pkg pkgid _ <- nodes
          , case pkg of
              InstallPlan.PreExisting _    -> True
              -- drop packages are not libs:
              InstallPlan.Installed srcpkg -> elabRequiresRegistration srcpkg
              -- and packages not yet built are not be included:
              InstallPlan.Configured  _    -> False
          ]

relativePackageDBPaths :: FilePath -> PackageDBStack -> PackageDBStack
relativePackageDBPaths relroot = map (relativePackageDBPath relroot)

relativePackageDBPath :: FilePath -> PackageDB -> PackageDB
relativePackageDBPath relroot pkgdb =
    case pkgdb of
      GlobalPackageDB        -> GlobalPackageDB
      UserPackageDB          -> UserPackageDB
      SpecificPackageDB path -> SpecificPackageDB relpath
        where relpath = makeRelative relroot path

