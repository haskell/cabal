{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Planning how to build everything in a project.
--
module Distribution.Client.ProjectPlanning (
    -- * elaborated install plan types
    ElaboratedInstallPlan,
    ElaboratedConfiguredPackage(..),
    ElaboratedPlanPackage,
    ElaboratedSharedConfig(..),
    ElaboratedReadyPackage,
    BuildStyle(..),
    CabalFileText,

    -- * Producing the elaborated install plan
    rebuildInstallPlan,

    -- * Build targets
    PackageTarget(..),
    ComponentTarget(..),
    SubComponentTarget(..),
    showComponentTarget,

    -- * Selecting a plan subset
    pruneInstallPlanToTargets,
    pruneInstallPlanToDependencies,

    -- * Utils required for building
    pkgHasEphemeralBuildTargets,
    elabBuildTargetWholeComponents,

    -- * Setup.hs CLI flags for building
    setupHsScriptOptions,
    setupHsConfigureFlags,
    setupHsConfigureArgs,
    setupHsBuildFlags,
    setupHsBuildArgs,
    setupHsReplFlags,
    setupHsReplArgs,
    setupHsTestFlags,
    setupHsTestArgs,
    setupHsCopyFlags,
    setupHsRegisterFlags,
    setupHsHaddockFlags,

    packageHashInputs,

    -- TODO: [code cleanup] utils that should live in some shared place?
    createPackageDBIfMissing
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import           Distribution.Client.ProjectPlanning.Types as Ty
import           Distribution.Client.PackageHash
import           Distribution.Client.RebuildMonad
import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanOutput

import           Distribution.Client.Types
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import           Distribution.Client.Dependency
import           Distribution.Client.Dependency.Types
import qualified Distribution.Client.IndexUtils as IndexUtils
import           Distribution.Client.Targets (userToPackageConstraint)
import           Distribution.Client.DistDirLayout
import           Distribution.Client.SetupWrapper
import           Distribution.Client.JobControl
import           Distribution.Client.FetchUtils
import qualified Hackage.Security.Client as Sec
import           Distribution.Client.Setup hiding (packageName, cabalVersion)
import           Distribution.Utils.NubList
import           Distribution.Utils.LogProgress
import           Distribution.Utils.MapAccum

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import           Distribution.Solver.Types.ConstraintSource
import           Distribution.Solver.Types.LabeledPackageConstraint
import           Distribution.Solver.Types.OptionalStanza
import           Distribution.Solver.Types.PkgConfigDb
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.InstSolverPackage
import           Distribution.Solver.Types.SourcePackage
import           Distribution.Solver.Types.Settings

import           Distribution.ModuleName
import           Distribution.Package hiding
  (InstalledPackageId, installedPackageId)
import           Distribution.Types.ComponentName
import           Distribution.Types.Dependency
import           Distribution.Types.ExeDependency
import           Distribution.Types.PkgconfigDependency
import           Distribution.Types.UnqualComponentName
import           Distribution.System
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import           Distribution.Simple.BuildToolDepends
import           Distribution.Simple.PackageIndex (InstalledPackageIndex)
import           Distribution.Simple.Compiler hiding (Flag)
import qualified Distribution.Simple.GHC   as GHC   --TODO: [code cleanup] eliminate
import qualified Distribution.Simple.GHCJS as GHCJS --TODO: [code cleanup] eliminate
import           Distribution.Simple.Program
import           Distribution.Simple.Program.Db
import           Distribution.Simple.Program.Find
import qualified Distribution.Simple.Setup as Cabal
import           Distribution.Simple.Setup
  (Flag, toFlag, flagToMaybe, flagToList, fromFlagOrDefault)
import qualified Distribution.Simple.Configure as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.InstalledPackageInfo as IPI

import           Distribution.Backpack.ConfiguredComponent
import           Distribution.Backpack.LinkedComponent
import           Distribution.Backpack.ComponentsGraph
import           Distribution.Backpack.ModuleShape
import           Distribution.Backpack.FullUnitId
import           Distribution.Backpack
import           Distribution.Types.ComponentInclude

import           Distribution.Simple.Utils hiding (matchFileGlob)
import           Distribution.Version
import           Distribution.Verbosity
import           Distribution.Text

import qualified Distribution.Compat.Graph as Graph
import           Distribution.Compat.Graph(IsNode(..))

import           Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as Disp
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad
import qualified Data.Traversable as T
import           Control.Monad.State as State
import           Control.Exception
import           Data.List (groupBy)
import           Data.Either
import           Data.Function
import           System.FilePath

------------------------------------------------------------------------------
-- * Elaborated install plan
------------------------------------------------------------------------------

-- "Elaborated" -- worked out with great care and nicety of detail;
--                 executed with great minuteness: elaborate preparations;
--                 elaborate care.
--
-- So here's the idea:
--
-- Rather than a miscellaneous collection of 'ConfigFlags', 'InstallFlags' etc
-- all passed in as separate args and which are then further selected,
-- transformed etc during the execution of the build. Instead we construct
-- an elaborated install plan that includes everything we will need, and then
-- during the execution of the plan we do as little transformation of this
-- info as possible.
--
-- So we're trying to split the work into two phases: construction of the
-- elaborated install plan (which as far as possible should be pure) and
-- then simple execution of that plan without any smarts, just doing what the
-- plan says to do.
--
-- So that means we need a representation of this fully elaborated install
-- plan. The representation consists of two parts:
--
-- * A 'ElaboratedInstallPlan'. This is a 'GenericInstallPlan' with a
--   representation of source packages that includes a lot more detail about
--   that package's individual configuration
--
-- * A 'ElaboratedSharedConfig'. Some package configuration is the same for
--   every package in a plan. Rather than duplicate that info every entry in
--   the 'GenericInstallPlan' we keep that separately.
--
-- The division between the shared and per-package config is /not set in stone
-- for all time/. For example if we wanted to generalise the install plan to
-- describe a situation where we want to build some packages with GHC and some
-- with GHCJS then the platform and compiler would no longer be shared between
-- all packages but would have to be per-package (probably with some sanity
-- condition on the graph structure).
--

-- Refer to ProjectPlanning.Types for details of these important types:

-- type ElaboratedInstallPlan = ...
-- type ElaboratedPlanPackage = ...
-- data ElaboratedSharedConfig = ...
-- data ElaboratedConfiguredPackage = ...
-- data BuildStyle =


-- | Check that an 'ElaboratedConfiguredPackage' actually makes
-- sense under some 'ElaboratedSharedConfig'.
sanityCheckElaboratedConfiguredPackage
    :: ElaboratedSharedConfig
    -> ElaboratedConfiguredPackage
    -> a
    -> a
sanityCheckElaboratedConfiguredPackage sharedConfig
                             elab@ElaboratedConfiguredPackage{..} =
    (case elabPkgOrComp of
        ElabPackage pkg -> sanityCheckElaboratedPackage elab pkg
        ElabComponent comp -> sanityCheckElaboratedComponent elab comp)

    -- either a package is being built inplace, or the
    -- 'installedPackageId' we assigned is consistent with
    -- the 'hashedInstalledPackageId' we would compute from
    -- the elaborated configured package
  . assert (elabBuildStyle == BuildInplaceOnly ||
     elabComponentId == hashedInstalledPackageId
                            (packageHashInputs sharedConfig elab))

    -- the stanzas explicitly disabled should not be available
  . assert (Set.null (Map.keysSet (Map.filter not elabStanzasRequested)
                `Set.intersection` elabStanzasAvailable))

    -- either a package is built inplace, or we are not attempting to
    -- build any test suites or benchmarks (we never build these
    -- for remote packages!)
  . assert (elabBuildStyle == BuildInplaceOnly ||
     Set.null elabStanzasAvailable)

sanityCheckElaboratedComponent
    :: ElaboratedConfiguredPackage
    -> ElaboratedComponent
    -> a
    -> a
sanityCheckElaboratedComponent ElaboratedConfiguredPackage{..}
                               ElaboratedComponent{..} =

    -- Should not be building bench or test if not inplace.
    assert (elabBuildStyle == BuildInplaceOnly ||
     case compComponentName of
        Nothing              -> True
        Just CLibName        -> True
        Just (CSubLibName _) -> True
        Just (CExeName _)    -> True
        -- This is interesting: there's no way to declare a dependency
        -- on a foreign library at the moment, but you may still want
        -- to install these to the store
        Just (CFLibName _)   -> True
        Just (CBenchName _)  -> False
        Just (CTestName _)   -> False)


sanityCheckElaboratedPackage
    :: ElaboratedConfiguredPackage
    -> ElaboratedPackage
    -> a
    -> a
sanityCheckElaboratedPackage ElaboratedConfiguredPackage{..}
                             ElaboratedPackage{..} =
    -- we should only have enabled stanzas that actually can be built
    -- (according to the solver)
    assert (pkgStanzasEnabled `Set.isSubsetOf` elabStanzasAvailable)

    -- the stanzas that the user explicitly requested should be
    -- enabled (by the previous test, they are also available)
  . assert (Map.keysSet (Map.filter id elabStanzasRequested)
                `Set.isSubsetOf` pkgStanzasEnabled)

------------------------------------------------------------------------------
-- * Deciding what to do: making an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------

-- | Return an up-to-date elaborated install plan and associated config.
--
-- Two variants of the install plan are returned: with and without packages
-- from the store. That is, the \"improved\" plan where source packages are
-- replaced by pre-existing installed packages from the store (when their ids
-- match), and also the original elaborated plan which uses primarily source
-- packages.

-- The improved plan is what we use for building, but the original elaborated
-- plan is useful for reporting and configuration. For example the @freeze@
-- command needs the source package info to know about flag choices and
-- dependencies of executables and setup scripts.
--
rebuildInstallPlan :: Verbosity
                   -> InstallFlags
                   -> FilePath -> DistDirLayout -> CabalDirLayout
                   -> ProjectConfig
                   -> IO ( ElaboratedInstallPlan  -- with store packages
                         , ElaboratedInstallPlan  -- with source packages
                         , ElaboratedSharedConfig
                         , ProjectConfig )
                      -- ^ @(improvedPlan, elaboratedPlan, _, _)@
rebuildInstallPlan verbosity
                   installFlags
                   projectRootDir
                   distDirLayout@DistDirLayout {
                     distDirectory,
                     distProjectCacheFile,
                     distProjectCacheDirectory
                   }
                   cabalDirLayout@CabalDirLayout {
                     cabalStoreDirectory,
                     cabalStorePackageDB
                   }
                   cliConfig =
    runRebuild projectRootDir $ do
    progsearchpath <- liftIO $ getSystemSearchPath
    let cliConfigPersistent = cliConfig { projectConfigBuildOnly = mempty }

    -- The overall improved plan is cached
    rerunIfChanged verbosity fileMonitorImprovedPlan
                   -- react to changes in command line args and the path
                   (cliConfigPersistent, progsearchpath) $ do

      -- And so is the elaborated plan that the improved plan based on
      (elaboratedPlan, elaboratedShared,
       projectConfig) <-
        rerunIfChanged verbosity fileMonitorElaboratedPlan
                       (cliConfigPersistent, progsearchpath) $ do

          (projectConfig, projectConfigTransient) <- phaseReadProjectConfig
          localPackages <- phaseReadLocalPackages projectConfig
          compilerEtc   <- phaseConfigureCompiler projectConfig
          _             <- phaseConfigurePrograms projectConfig compilerEtc
          (solverPlan, pkgConfigDB)
                        <- phaseRunSolver         projectConfigTransient
                                                  compilerEtc
                                                  localPackages
          (elaboratedPlan,
           elaboratedShared) <- phaseElaboratePlan projectConfigTransient
                                                   compilerEtc pkgConfigDB
                                                   solverPlan
                                                   localPackages

          phaseMaintainPlanOutputs elaboratedPlan elaboratedShared
          return (elaboratedPlan, elaboratedShared, projectConfig)

      -- The improved plan changes each time we install something, whereas
      -- the underlying elaborated plan only changes when input config
      -- changes, so it's worth caching them separately.
      improvedPlan <- phaseImprovePlan elaboratedPlan elaboratedShared

      return (improvedPlan, elaboratedPlan, elaboratedShared, projectConfig)

  where
    fileMonitorCompiler       = newFileMonitorInCacheDir "compiler"
    fileMonitorSolverPlan     = newFileMonitorInCacheDir "solver-plan"
    fileMonitorSourceHashes   = newFileMonitorInCacheDir "source-hashes"
    fileMonitorElaboratedPlan = newFileMonitorInCacheDir "elaborated-plan"
    fileMonitorImprovedPlan   = newFileMonitorInCacheDir "improved-plan"

    newFileMonitorInCacheDir :: Eq a => FilePath -> FileMonitor a b
    newFileMonitorInCacheDir  = newFileMonitor . distProjectCacheFile

    -- Read the cabal.project (or implicit config) and combine it with
    -- arguments from the command line
    --
    phaseReadProjectConfig :: Rebuild (ProjectConfig, ProjectConfig)
    phaseReadProjectConfig = do
      liftIO $ do
        info verbosity "Project settings changed, reconfiguring..."
        createDirectoryIfMissingVerbose verbosity True distDirectory
        createDirectoryIfMissingVerbose verbosity True distProjectCacheDirectory

      projectConfig <- readProjectConfig verbosity installFlags projectRootDir

      -- The project config comming from the command line includes "build only"
      -- flags that we don't cache persistently (because like all "build only"
      -- flags they do not affect the value of the outcome) but that we do
      -- sometimes using during planning (in particular the http transport)
      let projectConfigTransient  = projectConfig <> cliConfig
          projectConfigPersistent = projectConfig
                                 <> cliConfig {
                                      projectConfigBuildOnly = mempty
                                    }
      liftIO $ writeProjectConfigFile (distProjectCacheFile "config")
                                      projectConfigPersistent
      return (projectConfigPersistent, projectConfigTransient)

    -- Look for all the cabal packages in the project
    -- some of which may be local src dirs, tarballs etc
    --
    phaseReadLocalPackages :: ProjectConfig
                           -> Rebuild [UnresolvedSourcePackage]
    phaseReadLocalPackages projectConfig = do

      localCabalFiles <- findProjectPackages projectRootDir projectConfig
      mapM (readSourcePackage verbosity) localCabalFiles


    -- Configure the compiler we're using.
    --
    -- This is moderately expensive and doesn't change that often so we cache
    -- it independently.
    --
    phaseConfigureCompiler :: ProjectConfig
                           -> Rebuild (Compiler, Platform, ProgramDb)
    phaseConfigureCompiler ProjectConfig {
                             projectConfigShared = ProjectConfigShared {
                               projectConfigHcFlavor,
                               projectConfigHcPath,
                               projectConfigHcPkg
                             },
                             projectConfigLocalPackages = PackageConfig {
                               packageConfigProgramPaths,
                               packageConfigProgramArgs,
                               packageConfigProgramPathExtra
                             }
                           } = do
        progsearchpath <- liftIO $ getSystemSearchPath
        rerunIfChanged verbosity fileMonitorCompiler
                       (hcFlavor, hcPath, hcPkg, progsearchpath,
                        packageConfigProgramPaths,
                        packageConfigProgramArgs,
                        packageConfigProgramPathExtra) $ do

          liftIO $ info verbosity "Compiler settings changed, reconfiguring..."
          result@(_, _, progdb') <- liftIO $
            Cabal.configCompilerEx
              hcFlavor hcPath hcPkg
              progdb verbosity

        -- Note that we added the user-supplied program locations and args
        -- for /all/ programs, not just those for the compiler prog and
        -- compiler-related utils. In principle we don't know which programs
        -- the compiler will configure (and it does vary between compilers).
        -- We do know however that the compiler will only configure the
        -- programs it cares about, and those are the ones we monitor here.
          monitorFiles (programsMonitorFiles progdb')

          return result
      where
        hcFlavor = flagToMaybe projectConfigHcFlavor
        hcPath   = flagToMaybe projectConfigHcPath
        hcPkg    = flagToMaybe projectConfigHcPkg
        progdb   =
            userSpecifyPaths (Map.toList (getMapLast packageConfigProgramPaths))
          . userSpecifyArgss (Map.toList (getMapMappend packageConfigProgramArgs))
          . modifyProgramSearchPath
              (++ [ ProgramSearchPathDir dir
                  | dir <- fromNubList packageConfigProgramPathExtra ])
          $ defaultProgramDb


    -- Configuring other programs.
    --
    -- Having configred the compiler, now we configure all the remaining
    -- programs. This is to check we can find them, and to monitor them for
    -- changes.
    --
    -- TODO: [required eventually] we don't actually do this yet.
    --
    -- We rely on the fact that the previous phase added the program config for
    -- all local packages, but that all the programs configured so far are the
    -- compiler program or related util programs.
    --
    phaseConfigurePrograms :: ProjectConfig
                           -> (Compiler, Platform, ProgramDb)
                           -> Rebuild ()
    phaseConfigurePrograms projectConfig (_, _, compilerprogdb) = do
        -- Users are allowed to specify program locations independently for
        -- each package (e.g. to use a particular version of a pre-processor
        -- for some packages). However they cannot do this for the compiler
        -- itself as that's just not going to work. So we check for this.
        liftIO $ checkBadPerPackageCompilerPaths
          (configuredPrograms compilerprogdb)
          (getMapMappend (projectConfigSpecificPackage projectConfig))

        --TODO: [required eventually] find/configure other programs that the
        -- user specifies.

        --TODO: [required eventually] find/configure all build-tools
        -- but note that some of them may be built as part of the plan.


    -- Run the solver to get the initial install plan.
    -- This is expensive so we cache it independently.
    --
    phaseRunSolver :: ProjectConfig
                   -> (Compiler, Platform, ProgramDb)
                   -> [UnresolvedSourcePackage]
                   -> Rebuild (SolverInstallPlan, PkgConfigDb)
    phaseRunSolver projectConfig@ProjectConfig {
                     projectConfigShared,
                     projectConfigBuildOnly
                   }
                   (compiler, platform, progdb)
                   localPackages =
        rerunIfChanged verbosity fileMonitorSolverPlan
                       (solverSettings,
                        localPackages, localPackagesEnabledStanzas,
                        compiler, platform, programDbSignature progdb) $ do

          installedPkgIndex <- getInstalledPackages verbosity
                                                    compiler progdb platform
                                                    corePackageDbs
          sourcePkgDb       <- getSourcePackages verbosity withRepoCtx
                                 (solverSettingIndexState solverSettings)
          pkgConfigDB       <- getPkgConfigDb verbosity progdb

          --TODO: [code cleanup] it'd be better if the Compiler contained the
          -- ConfiguredPrograms that it needs, rather than relying on the progdb
          -- since we don't need to depend on all the programs here, just the
          -- ones relevant for the compiler.

          liftIO $ do
            solver <- chooseSolver verbosity
                                   (solverSettingSolver solverSettings)
                                   (compilerInfo compiler)

            notice verbosity "Resolving dependencies..."
            plan <- foldProgress logMsg die return $
              planPackages verbosity compiler platform solver solverSettings
                           installedPkgIndex sourcePkgDb pkgConfigDB
                           localPackages localPackagesEnabledStanzas
            return (plan, pkgConfigDB)
      where
        corePackageDbs = [GlobalPackageDB]
        withRepoCtx    = projectConfigWithSolverRepoContext verbosity
                           projectConfigShared
                           projectConfigBuildOnly
        solverSettings = resolveSolverSettings projectConfig
        logMsg message rest = debugNoWrap verbosity message >> rest

        localPackagesEnabledStanzas =
          Map.fromList
            [ (pkgname, stanzas)
            | pkg <- localPackages
            , let pkgname            = packageName pkg
                  testsEnabled       = lookupLocalPackageConfig
                                         packageConfigTests
                                         projectConfig pkgname
                  benchmarksEnabled  = lookupLocalPackageConfig
                                         packageConfigBenchmarks
                                         projectConfig pkgname
                  stanzas =
                    Map.fromList $
                      [ (TestStanzas, enabled)
                      | enabled <- flagToList testsEnabled ]
                   ++ [ (BenchStanzas , enabled)
                      | enabled <- flagToList benchmarksEnabled ]
            ]

    -- Elaborate the solver's install plan to get a fully detailed plan. This
    -- version of the plan has the final nix-style hashed ids.
    --
    phaseElaboratePlan :: ProjectConfig
                       -> (Compiler, Platform, ProgramDb)
                       -> PkgConfigDb
                       -> SolverInstallPlan
                       -> [SourcePackage loc]
                       -> Rebuild ( ElaboratedInstallPlan
                                  , ElaboratedSharedConfig )
    phaseElaboratePlan ProjectConfig {
                         projectConfigShared,
                         projectConfigLocalPackages,
                         projectConfigSpecificPackage,
                         projectConfigBuildOnly
                       }
                       (compiler, platform, progdb) pkgConfigDB
                       solverPlan localPackages = do

        liftIO $ debug verbosity "Elaborating the install plan..."

        sourcePackageHashes <-
          rerunIfChanged verbosity fileMonitorSourceHashes
                         (packageLocationsSignature solverPlan) $
            getPackageSourceHashes verbosity withRepoCtx solverPlan

        defaultInstallDirs <- liftIO $ userInstallDirTemplates compiler
        (elaboratedPlan, elaboratedShared)
          <- liftIO . runLogProgress verbosity $
              elaborateInstallPlan
                verbosity
                platform compiler progdb pkgConfigDB
                distDirLayout
                cabalDirLayout
                solverPlan
                localPackages
                sourcePackageHashes
                defaultInstallDirs
                projectConfigShared
                projectConfigLocalPackages
                (getMapMappend projectConfigSpecificPackage)
        let instantiatedPlan = instantiateInstallPlan elaboratedPlan
        liftIO $ debugNoWrap verbosity (InstallPlan.showInstallPlan instantiatedPlan)
        return (instantiatedPlan, elaboratedShared)
      where
        withRepoCtx = projectConfigWithSolverRepoContext verbosity
                        projectConfigShared
                        projectConfigBuildOnly

    -- Update the files we maintain that reflect our current build environment.
    -- In particular we maintain a JSON representation of the elaborated
    -- install plan (but not the improved plan since that reflects the state
    -- of the build rather than just the input environment).
    --
    phaseMaintainPlanOutputs :: ElaboratedInstallPlan
                             -> ElaboratedSharedConfig
                             -> Rebuild ()
    phaseMaintainPlanOutputs elaboratedPlan elaboratedShared = liftIO $ do
        debug verbosity "Updating plan.json"
        writePlanExternalRepresentation
          distDirLayout
          elaboratedPlan
          elaboratedShared


    -- Improve the elaborated install plan. The elaborated plan consists
    -- mostly of source packages (with full nix-style hashed ids). Where
    -- corresponding installed packages already exist in the store, replace
    -- them in the plan.
    --
    -- Note that we do monitor the store's package db here, so we will redo
    -- this improvement phase when the db changes -- including as a result of
    -- executing a plan and installing things.
    --
    phaseImprovePlan :: ElaboratedInstallPlan
                     -> ElaboratedSharedConfig
                     -> Rebuild ElaboratedInstallPlan
    phaseImprovePlan elaboratedPlan elaboratedShared = do

        liftIO $ debug verbosity "Improving the install plan..."
        createDirectoryMonitored True storeDirectory
        liftIO $ createPackageDBIfMissing verbosity
                                          compiler progdb
                                          storePackageDb
        storePkgIdSet <- getInstalledStorePackages storeDirectory
        let improvedPlan = improveInstallPlanWithInstalledPackages
                             storePkgIdSet
                             elaboratedPlan
        liftIO $ debugNoWrap verbosity (InstallPlan.showInstallPlan improvedPlan)
        -- TODO: [nice to have] having checked which packages from the store
        -- we're using, it may be sensible to sanity check those packages
        -- by loading up the compiler package db and checking everything
        -- matches up as expected, e.g. no dangling deps, files deleted.
        return improvedPlan
      where
        storeDirectory  = cabalStoreDirectory (compilerId compiler)
        storePackageDb  = cabalStorePackageDB (compilerId compiler)
        ElaboratedSharedConfig {
          pkgConfigCompiler      = compiler,
          pkgConfigCompilerProgs = progdb
        } = elaboratedShared


programsMonitorFiles :: ProgramDb -> [MonitorFilePath]
programsMonitorFiles progdb =
    [ monitor
    | prog    <- configuredPrograms progdb
    , monitor <- monitorFileSearchPath (programMonitorFiles prog)
                                       (programPath prog)
    ]

-- | Select the bits of a 'ProgramDb' to monitor for value changes.
-- Use 'programsMonitorFiles' for the files to monitor.
--
programDbSignature :: ProgramDb -> [ConfiguredProgram]
programDbSignature progdb =
    [ prog { programMonitorFiles = []
           , programOverrideEnv  = filter ((/="PATH") . fst)
                                          (programOverrideEnv prog) }
    | prog <- configuredPrograms progdb ]

getInstalledPackages :: Verbosity
                     -> Compiler -> ProgramDb -> Platform
                     -> PackageDBStack
                     -> Rebuild InstalledPackageIndex
getInstalledPackages verbosity compiler progdb platform packagedbs = do
    monitorFiles . map monitorFileOrDirectory
      =<< liftIO (IndexUtils.getInstalledPackagesMonitorFiles
                    verbosity compiler
                    packagedbs progdb platform)
    liftIO $ IndexUtils.getInstalledPackages
               verbosity compiler
               packagedbs progdb

{-
--TODO: [nice to have] use this but for sanity / consistency checking
getPackageDBContents :: Verbosity
                     -> Compiler -> ProgramDb -> Platform
                     -> PackageDB
                     -> Rebuild InstalledPackageIndex
getPackageDBContents verbosity compiler progdb platform packagedb = do
    monitorFiles . map monitorFileOrDirectory
      =<< liftIO (IndexUtils.getInstalledPackagesMonitorFiles
                    verbosity compiler
                    [packagedb] progdb platform)
    liftIO $ do
      createPackageDBIfMissing verbosity compiler progdb packagedb
      Cabal.getPackageDBContents verbosity compiler
                                 packagedb progdb
-}

-- | Return the 'UnitId's of all packages\/components already installed in the
-- store.
--
getInstalledStorePackages :: FilePath -- ^ store directory
                          -> Rebuild (Set UnitId)
getInstalledStorePackages storeDirectory = do
    paths <- getDirectoryContentsMonitored storeDirectory
    return $ Set.fromList [ newSimpleUnitId (mkComponentId path)
                          | path <- paths, valid path ]
  where
    valid ('.':_)      = False
    valid "package.db" = False
    valid _            = True

getSourcePackages :: Verbosity -> (forall a. (RepoContext -> IO a) -> IO a)
                  -> IndexUtils.IndexState -> Rebuild SourcePackageDb
getSourcePackages verbosity withRepoCtx idxState = do
    (sourcePkgDb, repos) <-
      liftIO $
        withRepoCtx $ \repoctx -> do
          sourcePkgDb <- IndexUtils.getSourcePackagesAtIndexState verbosity
                                                                  repoctx idxState
          return (sourcePkgDb, repoContextRepos repoctx)

    monitorFiles . map monitorFile
                 . IndexUtils.getSourcePackagesMonitorFiles
                 $ repos
    return sourcePkgDb


-- | Create a package DB if it does not currently exist. Note that this action
-- is /not/ safe to run concurrently.
--
createPackageDBIfMissing :: Verbosity -> Compiler -> ProgramDb
                         -> PackageDB -> IO ()
createPackageDBIfMissing verbosity compiler progdb
                         (SpecificPackageDB dbPath) = do
    exists <- liftIO $ Cabal.doesPackageDBExist dbPath
    unless exists $ do
      createDirectoryIfMissingVerbose verbosity True (takeDirectory dbPath)
      Cabal.createPackageDB verbosity compiler progdb False dbPath
createPackageDBIfMissing _ _ _ _ = return ()


getPkgConfigDb :: Verbosity -> ProgramDb -> Rebuild PkgConfigDb
getPkgConfigDb verbosity progdb = do
    dirs <- liftIO $ getPkgConfigDbDirs verbosity progdb
    -- Just monitor the dirs so we'll notice new .pc files.
    -- Alternatively we could monitor all the .pc files too.
    mapM_ monitorDirectoryStatus dirs
    liftIO $ readPkgConfigDb verbosity progdb


-- | Select the config values to monitor for changes package source hashes.
packageLocationsSignature :: SolverInstallPlan
                          -> [(PackageId, PackageLocation (Maybe FilePath))]
packageLocationsSignature solverPlan =
    [ (packageId pkg, packageSource pkg)
    | SolverInstallPlan.Configured (SolverPackage { solverPkgSource = pkg})
        <- SolverInstallPlan.toList solverPlan
    ]


-- | Get the 'HashValue' for all the source packages where we use hashes,
-- and download any packages required to do so.
--
-- Note that we don't get hashes for local unpacked packages.
--
getPackageSourceHashes :: Verbosity
                       -> (forall a. (RepoContext -> IO a) -> IO a)
                       -> SolverInstallPlan
                       -> Rebuild (Map PackageId PackageSourceHash)
getPackageSourceHashes verbosity withRepoCtx solverPlan = do

    -- Determine if and where to get the package's source hash from.
    --
    let allPkgLocations :: [(PackageId, PackageLocation (Maybe FilePath))]
        allPkgLocations =
          [ (packageId pkg, packageSource pkg)
          | SolverInstallPlan.Configured (SolverPackage { solverPkgSource = pkg})
              <- SolverInstallPlan.toList solverPlan ]

        -- Tarballs that were local in the first place.
        -- We'll hash these tarball files directly.
        localTarballPkgs :: [(PackageId, FilePath)]
        localTarballPkgs =
          [ (pkgid, tarball)
          | (pkgid, LocalTarballPackage tarball) <- allPkgLocations ]

        -- Tarballs from remote URLs. We must have downloaded these already
        -- (since we extracted the .cabal file earlier)
        --TODO: [required eventually] finish remote tarball functionality
--        allRemoteTarballPkgs =
--          [ (pkgid, )
--          | (pkgid, RemoteTarballPackage ) <- allPkgLocations ]

        -- Tarballs from repositories, either where the repository provides
        -- hashes as part of the repo metadata, or where we will have to
        -- download and hash the tarball.
        repoTarballPkgsWithMetadata    :: [(PackageId, Repo)]
        repoTarballPkgsWithoutMetadata :: [(PackageId, Repo)]
        (repoTarballPkgsWithMetadata,
         repoTarballPkgsWithoutMetadata) =
          partitionEithers
          [ case repo of
              RepoSecure{} -> Left  (pkgid, repo)
              _            -> Right (pkgid, repo)
          | (pkgid, RepoTarballPackage repo _ _) <- allPkgLocations ]

    -- For tarballs from repos that do not have hashes available we now have
    -- to check if the packages were downloaded already.
    --
    (repoTarballPkgsToDownload,
     repoTarballPkgsDownloaded)
      <- fmap partitionEithers $
         liftIO $ sequence
           [ do mtarball <- checkRepoTarballFetched repo pkgid
                case mtarball of
                  Nothing      -> return (Left  (pkgid, repo))
                  Just tarball -> return (Right (pkgid, tarball))
           | (pkgid, repo) <- repoTarballPkgsWithoutMetadata ]

    (hashesFromRepoMetadata,
     repoTarballPkgsNewlyDownloaded) <-
      -- Avoid having to initialise the repository (ie 'withRepoCtx') if we
      -- don't have to. (The main cost is configuring the http client.)
      if null repoTarballPkgsToDownload && null repoTarballPkgsWithMetadata
      then return (Map.empty, [])
      else liftIO $ withRepoCtx $ \repoctx -> do

      -- For tarballs from repos that do have hashes available as part of the
      -- repo metadata we now load up the index for each repo and retrieve
      -- the hashes for the packages
      --
      hashesFromRepoMetadata <-
        Sec.uncheckClientErrors $ --TODO: [code cleanup] wrap in our own exceptions
        fmap (Map.fromList . concat) $
        sequence
          -- Reading the repo index is expensive so we group the packages by repo
          [ repoContextWithSecureRepo repoctx repo $ \secureRepo ->
              Sec.withIndex secureRepo $ \repoIndex ->
                sequence
                  [ do hash <- Sec.trusted <$> -- strip off Trusted tag
                               Sec.indexLookupHash repoIndex pkgid
                       -- Note that hackage-security currently uses SHA256
                       -- but this API could in principle give us some other
                       -- choice in future.
                       return (pkgid, hashFromTUF hash)
                  | pkgid <- pkgids ]
          | (repo, pkgids) <-
                map (\grp@((_,repo):_) -> (repo, map fst grp))
              . groupBy ((==)    `on` (remoteRepoName . repoRemote . snd))
              . sortBy  (compare `on` (remoteRepoName . repoRemote . snd))
              $ repoTarballPkgsWithMetadata
          ]

      -- For tarballs from repos that do not have hashes available, download
      -- the ones we previously determined we need.
      --
      repoTarballPkgsNewlyDownloaded <-
        sequence
          [ do tarball <- fetchRepoTarball verbosity repoctx repo pkgid
               return (pkgid, tarball)
          | (pkgid, repo) <- repoTarballPkgsToDownload ]

      return (hashesFromRepoMetadata,
              repoTarballPkgsNewlyDownloaded)

    -- Hash tarball files for packages where we have to do that. This includes
    -- tarballs that were local in the first place, plus tarballs from repos,
    -- either previously cached or freshly downloaded.
    --
    let allTarballFilePkgs :: [(PackageId, FilePath)]
        allTarballFilePkgs = localTarballPkgs
                          ++ repoTarballPkgsDownloaded
                          ++ repoTarballPkgsNewlyDownloaded
    hashesFromTarballFiles <- liftIO $
      fmap Map.fromList $
      sequence
        [ do srchash <- readFileHashValue tarball
             return (pkgid, srchash)
        | (pkgid, tarball) <- allTarballFilePkgs
        ]
    monitorFiles [ monitorFile tarball
                 | (_pkgid, tarball) <- allTarballFilePkgs ]

    -- Return the combination
    return $! hashesFromRepoMetadata
           <> hashesFromTarballFiles


-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

planPackages :: Verbosity
             -> Compiler
             -> Platform
             -> Solver -> SolverSettings
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> PkgConfigDb
             -> [UnresolvedSourcePackage]
             -> Map PackageName (Map OptionalStanza Bool)
             -> Progress String String SolverInstallPlan
planPackages verbosity comp platform solver SolverSettings{..}
             installedPkgIndex sourcePkgDb pkgConfigDB
             localPackages pkgStanzasEnable =

    resolveDependencies
      platform (compilerInfo comp)
      pkgConfigDB solver
      resolverParams

  where

    --TODO: [nice to have] disable multiple instances restriction in the solver, but then
    -- make sure we can cope with that in the output.
    resolverParams =

        setMaxBackjumps solverSettingMaxBackjumps

        --TODO: [required eventually] should only be configurable for custom installs
   -- . setIndependentGoals solverSettingIndependentGoals

      . setReorderGoals solverSettingReorderGoals

      . setCountConflicts solverSettingCountConflicts

        --TODO: [required eventually] should only be configurable for custom installs
   -- . setAvoidReinstalls solverSettingAvoidReinstalls

        --TODO: [required eventually] should only be configurable for custom installs
   -- . setShadowPkgs solverSettingShadowPkgs

      . setStrongFlags solverSettingStrongFlags

      . setAllowBootLibInstalls solverSettingAllowBootLibInstalls

      . setSolverVerbosity verbosity

        --TODO: [required eventually] decide if we need to prefer installed for
        -- global packages, or prefer latest even for global packages. Perhaps
        -- should be configurable but with a different name than "upgrade-dependencies".
      . setPreferenceDefault PreferLatestForSelected
                           {-(if solverSettingUpgradeDeps
                                then PreferAllLatest
                                else PreferLatestForSelected)-}

      . removeLowerBounds solverSettingAllowOlder
      . removeUpperBounds solverSettingAllowNewer

      . addDefaultSetupDependencies (defaultSetupDeps comp platform
                                   . PD.packageDescription
                                   . packageDescription)

      . addSetupCabalMinVersionConstraint (mkVersion [1,20])
          -- While we can talk to older Cabal versions (we need to be able to
          -- do so for custom Setup scripts that require older Cabal lib
          -- versions), we have problems talking to some older versions that
          -- don't support certain features.
          --
          -- For example, Cabal-1.16 and older do not know about build targets.
          -- Even worse, 1.18 and older only supported the --constraint flag
          -- with source package ids, not --dependency with installed package
          -- ids. That is bad because we cannot reliably select the right
          -- dependencies in the presence of multiple instances (i.e. the
          -- store). See issue #3932. So we require Cabal 1.20 as a minimum.

      . addPreferences
          -- preferences from the config file or command line
          [ PackageVersionPreference name ver
          | Dependency name ver <- solverSettingPreferences ]

      . addConstraints
          -- version constraints from the config file or command line
            [ LabeledPackageConstraint (userToPackageConstraint pc) src
            | (pc, src) <- solverSettingConstraints ]

      . addPreferences
          -- enable stanza preference where the user did not specify
          [ PackageStanzasPreference pkgname stanzas
          | pkg <- localPackages
          , let pkgname = packageName pkg
                stanzaM = Map.findWithDefault Map.empty pkgname pkgStanzasEnable
                stanzas = [ stanza | stanza <- [minBound..maxBound]
                          , Map.lookup stanza stanzaM == Nothing ]
          , not (null stanzas)
          ]

      . addConstraints
          -- enable stanza constraints where the user asked to enable
          [ LabeledPackageConstraint
              (PackageConstraint (scopeToplevel pkgname)
                                 (PackagePropertyStanzas stanzas))
              ConstraintSourceConfigFlagOrTarget
          | pkg <- localPackages
          , let pkgname = packageName pkg
                stanzaM = Map.findWithDefault Map.empty pkgname pkgStanzasEnable
                stanzas = [ stanza | stanza <- [minBound..maxBound]
                          , Map.lookup stanza stanzaM == Just True ]
          , not (null stanzas)
          ]

      . addConstraints
          --TODO: [nice to have] should have checked at some point that the
          -- package in question actually has these flags.
          [ LabeledPackageConstraint
              (PackageConstraint (scopeToplevel pkgname)
                                 (PackagePropertyFlags flags))
              ConstraintSourceConfigFlagOrTarget
          | (pkgname, flags) <- Map.toList solverSettingFlagAssignments ]

      . addConstraints
          --TODO: [nice to have] we have user-supplied flags for unspecified
          -- local packages (as well as specific per-package flags). For the
          -- former we just apply all these flags to all local targets which
          -- is silly. We should check if the flags are appropriate.
          [ LabeledPackageConstraint
              (PackageConstraint (scopeToplevel pkgname)
                                 (PackagePropertyFlags flags))
              ConstraintSourceConfigFlagOrTarget
          | let flags = solverSettingFlagAssignment
          , not (null flags)
          , pkg <- localPackages
          , let pkgname = packageName pkg ]

      $ stdResolverParams

    stdResolverParams =
      -- Note: we don't use the standardInstallPolicy here, since that uses
      -- its own addDefaultSetupDependencies that is not appropriate for us.
      basicInstallPolicy
        installedPkgIndex sourcePkgDb
        (map SpecificSourcePackage localPackages)


------------------------------------------------------------------------------
-- * Install plan post-processing
------------------------------------------------------------------------------

-- This phase goes from the InstallPlan we get from the solver and has to
-- make an elaborated install plan.
--
-- We go in two steps:
--
--  1. elaborate all the source packages that the solver has chosen.
--  2. swap source packages for pre-existing installed packages wherever
--     possible.
--
-- We do it in this order, elaborating and then replacing, because the easiest
-- way to calculate the installed package ids used for the replacement step is
-- from the elaborated configuration for each package.




------------------------------------------------------------------------------
-- * Install plan elaboration
------------------------------------------------------------------------------

-- | Produce an elaborated install plan using the policy for local builds with
-- a nix-style shared store.
--
-- In theory should be able to make an elaborated install plan with a policy
-- matching that of the classic @cabal install --user@ or @--global@
--
elaborateInstallPlan
  :: Verbosity -> Platform -> Compiler -> ProgramDb -> PkgConfigDb
  -> DistDirLayout
  -> CabalDirLayout
  -> SolverInstallPlan
  -> [SourcePackage loc]
  -> Map PackageId PackageSourceHash
  -> InstallDirs.InstallDirTemplates
  -> ProjectConfigShared
  -> PackageConfig
  -> Map PackageName PackageConfig
  -> LogProgress (ElaboratedInstallPlan, ElaboratedSharedConfig)
elaborateInstallPlan verbosity platform compiler compilerprogdb pkgConfigDB
                     DistDirLayout{..}
                     cabalDirLayout@CabalDirLayout{cabalStorePackageDB}
                     solverPlan localPackages
                     sourcePackageHashes
                     defaultInstallDirs
                     _sharedPackageConfig
                     localPackagesConfig
                     perPackageConfig = do
    x <- elaboratedInstallPlan
    return (x, elaboratedSharedConfig)
  where
    elaboratedSharedConfig =
      ElaboratedSharedConfig {
        pkgConfigPlatform      = platform,
        pkgConfigCompiler      = compiler,
        pkgConfigCompilerProgs = compilerprogdb
      }

    preexistingInstantiatedPkgs =
        Map.fromList (mapMaybe f (SolverInstallPlan.toList solverPlan))
      where
        f (SolverInstallPlan.PreExisting inst)
            | not (IPI.indefinite ipkg)
            = Just (IPI.installedUnitId ipkg,
                     (FullUnitId (IPI.installedComponentId ipkg)
                                 (Map.fromList (IPI.instantiatedWith ipkg))))
         where ipkg = instSolverPkgIPI inst
        f _ = Nothing

    elaboratedInstallPlan =
      flip InstallPlan.fromSolverInstallPlanWithProgress solverPlan $ \mapDep planpkg ->
        case planpkg of
          SolverInstallPlan.PreExisting pkg ->
            return [InstallPlan.PreExisting (instSolverPkgIPI pkg)]

          SolverInstallPlan.Configured  pkg ->
            let inplace_doc | shouldBuildInplaceOnly pkg = text "inplace"
                            | otherwise                  = Disp.empty
            in addProgressCtx (text "In the" <+> inplace_doc <+> text "package" <+>
                             quotes (disp (packageId pkg))) $
               map InstallPlan.Configured <$> elaborateSolverToComponents mapDep pkg

    -- NB: We don't INSTANTIATE packages at this point.  That's
    -- a post-pass.  This makes it simpler to compute dependencies.
    elaborateSolverToComponents
        :: (SolverId -> [ElaboratedPlanPackage])
        -> SolverPackage UnresolvedPkgLoc
        -> LogProgress [ElaboratedConfiguredPackage]
    elaborateSolverToComponents mapDep spkg@(SolverPackage _ _ _ deps0 exe_deps0)
        = case toComponentsGraph (elabEnabledSpec elab0) pd of
           Right g -> do
            infoProgress $ hang (text "Component graph for" <+> disp pkgid <<>> colon)
                            4 (dispComponentsGraph g)
            (_, comps) <- mapAccumM buildComponent
                            ((Map.empty, Map.empty), Map.empty, Map.empty)
                            (map fst g)
            let is_public_lib ElaboratedConfiguredPackage{..} =
                    case elabPkgOrComp of
                        ElabComponent comp -> compSolverName comp == CD.ComponentLib
                        _ -> False
                modShape = case find is_public_lib comps of
                            Nothing -> emptyModuleShape
                            Just ElaboratedConfiguredPackage{..} -> elabModuleShape
            return $ if eligible
                then comps
                else [(elaborateSolverToPackage mapDep spkg) {
                        elabModuleShape = modShape
                     }]
           Left cns ->
            dieProgress $
                hang (text "Dependency cycle between the following components:") 4
                     (vcat (map (text . componentNameStanza) cns))
      where
        eligible
            -- At this point in time, only non-Custom setup scripts
            -- are supported.  Implementing per-component builds with
            -- Custom would require us to create a new 'ElabSetup'
            -- type, and teach all of the code paths how to handle it.
            -- Once you've implemented this, swap it for the code below.
            = fromMaybe PD.Custom (PD.buildType (elabPkgDescription elab0)) /= PD.Custom
            -- cabal-format versions prior to 1.8 have different build-depends semantics
            -- for now it's easier to just fallback to legacy-mode when specVersion < 1.8
            -- see, https://github.com/haskell/cabal/issues/4121
              && PD.specVersion pd >= mkVersion [1,8]

            {-
            -- Only non-Custom or sufficiently recent Custom
            -- scripts can be build per-component.
            = (fromMaybe PD.Custom (PD.buildType pd) /= PD.Custom)
                || PD.specVersion pd >= mkVersion [1,25,0]
            -}

        elab0 = elaborateSolverToCommon mapDep spkg
        pkgid = elabPkgSourceId    elab0
        pd    = elabPkgDescription elab0

        buildComponent
            :: (ConfiguredComponentMap,
                LinkedComponentMap,
                Map ComponentId FilePath)
            -> Cabal.Component
            -> LogProgress
                ((ConfiguredComponentMap,
                  LinkedComponentMap,
                  Map ComponentId FilePath),
                ElaboratedConfiguredPackage)
        buildComponent (cc_map, lc_map, exe_map) comp =
          addProgressCtx (text "In the stanza" <+>
                          quotes (text (componentNameStanza cname))) $ do
            -- Before we get too far, check if we depended on something
            -- unbuildable.  If we did, give a good error.  (If we don't
            -- check, the 'toConfiguredComponent' will assert fail, see #3978).
            case unbuildable_external_lib_deps of
                [] -> return ()
                deps -> dieProgress $
                            text "Dependency on unbuildable libraries:" <+>
                            hsep (punctuate comma (map (disp.solverSrcId) deps))
            case unbuildable_external_exe_deps of
                [] -> return ()
                deps -> dieProgress $
                            text "Dependency on unbuildable executables:" <+>
                            hsep (punctuate comma (map (disp.solverSrcId) deps))
            infoProgress $ dispConfiguredComponent cc
            let -- Use of invariant: DefUnitId indicates that if
                -- there is no hash, it must have an empty
                -- instantiation.
                lookup_uid def_uid =
                    case Map.lookup (unDefUnitId def_uid) preexistingInstantiatedPkgs of
                        Just full -> full
                        Nothing -> error ("lookup_uid: " ++ display def_uid)
            lc <- toLinkedComponent verbosity lookup_uid (elabPkgSourceId elab0)
                        (Map.union external_lc_map lc_map) cc
            let lc_map' = extendLinkedComponentMap lc lc_map
            infoProgress $ dispLinkedComponent lc
            -- NB: For inplace NOT InstallPaths.bindir installDirs; for an
            -- inplace build those values are utter nonsense.  So we
            -- have to guess where the directory is going to be.
            -- Fortunately this is "stable" part of Cabal API.
            -- But the way we get the build directory is A HORRIBLE
            -- HACK.
            -- NB: elab is setup to be the correct form for an
            -- indefinite library, or a definite library with no holes.
            -- We will modify it in 'instantiateInstallPlan' to handle
            -- instantiated packages.
            let elab = elab1 {
                    elabModuleShape = lc_shape lc,
                    elabUnitId      = abstractUnitId (lc_uid lc),
                    elabComponentId = lc_cid lc,
                    elabLinkedInstantiatedWith = Map.fromList (lc_insts lc),
                    elabPkgOrComp = ElabComponent $ elab_comp {
                        compLinkedLibDependencies = ordNub (map ci_id (lc_includes lc)),
                        compNonSetupDependencies =
                          ordNub (map (abstractUnitId . ci_id) (lc_includes lc ++ lc_sig_includes lc))
                      }
                   }
                inplace_bin_dir
                  | shouldBuildInplaceOnly spkg
                  = distBuildDirectory
                        (elabDistDirParams elaboratedSharedConfig elab) </>
                        "build" </> case Cabal.componentNameString cname of
                                        Just n -> display n
                                        Nothing -> ""
                  | otherwise
                  = InstallDirs.bindir install_dirs
                exe_map' = Map.insert cid inplace_bin_dir exe_map
            return ((cc_map', lc_map', exe_map'), elab)
          where
            elab1 = elab0 {
                    elabInstallDirs = install_dirs,
                    elabRequiresRegistration = requires_reg,
                    elabPkgOrComp = ElabComponent $ elab_comp
                 }
            elab_comp = ElaboratedComponent {..}
            compLinkedLibDependencies = error "buildComponent: compLinkedLibDependencies"
            compNonSetupDependencies = error "buildComponent: compNonSetupDependencies"

            cc = toConfiguredComponent pd cid external_cc_map cc_map comp
            cc_map' = extendConfiguredComponentMap cc cc_map

            cid :: ComponentId
            cid = case elabBuildStyle elab0 of
                    BuildInplaceOnly ->
                      mkComponentId $
                        display pkgid ++ "-inplace" ++
                          (case Cabal.componentNameString cname of
                              Nothing -> ""
                              Just s -> "-" ++ display s)
                    BuildAndInstall ->
                      hashedInstalledPackageId
                        (packageHashInputs
                            elaboratedSharedConfig
                            elab1) -- knot tied

            cname = Cabal.componentName comp
            requires_reg = case cname of
                CLibName -> True
                CSubLibName _ -> True
                _ -> False
            compComponentName = Just cname
            compSolverName = CD.componentNameToComponent cname
            -- NB: compLinkedLibDependencies and
            -- compNonSetupDependencies are defined when we define
            -- 'elab'.
            compLibDependencies =
                -- concatMap (elaborateLibSolverId mapDep) external_lib_dep_sids
                ordNub (map (\ci -> ConfiguredId (ci_pkgid ci) (ci_id ci)) (cc_includes cc))
            filterExeMapDepApp = filterExeMapDep mapDep pd [Cabal.componentBuildInfo comp]

            compExeDependencies =
                map confInstId
                    (concatMap (elaborateExeSolverId filterExeMapDepApp) external_exe_dep_sids) ++
                cc_internal_build_tools cc
            compExeDependencyPaths =
                concatMap (elaborateExePath filterExeMapDepApp) external_exe_dep_sids ++
                [ path
                | cid' <- cc_internal_build_tools cc
                , Just path <- [Map.lookup cid' exe_map]]

            bi = Cabal.componentBuildInfo comp
            compPkgConfigDependencies =
                [ (pn, fromMaybe (error $ "compPkgConfigDependencies: impossible! "
                                            ++ display pn ++ " from "
                                            ++ display (elabPkgSourceId elab1))
                                 (pkgConfigDbPkgVersion pkgConfigDB pn))
                | PkgconfigDependency pn _ <- PD.pkgconfigDepends bi ]

            compSetupDependencies = concatMap (elaborateLibSolverId mapDep) (CD.setupDeps deps0)

            install_dirs
              | shouldBuildInplaceOnly spkg
              -- use the ordinary default install dirs
              = (InstallDirs.absoluteInstallDirs
                   pkgid
                   (newSimpleUnitId cid)
                   (compilerInfo compiler)
                   InstallDirs.NoCopyDest
                   platform
                   defaultInstallDirs) {

                  InstallDirs.libsubdir  = "", -- absoluteInstallDirs sets these as
                  InstallDirs.datasubdir = ""  -- 'undefined' but we have to use
                }                              -- them as "Setup.hs configure" args

              | otherwise
              -- use special simplified install dirs
              = storePackageInstallDirs
                  cabalDirLayout
                  (compilerId compiler)
                  cid

            external_lib_dep_sids = CD.select (== compSolverName) deps0
            external_lib_dep_pkgs = concatMap (elaborateLibSolverId' mapDep) external_lib_dep_sids
            compInplaceDependencyBuildCacheFiles
                = concatMap (elaborateLibBuildCacheFile mapDep) external_lib_dep_sids
            external_exe_dep_sids = CD.select (== compSolverName) exe_deps0
            external_cc_map = Map.fromList (map mkPkgNameMapping external_lib_dep_pkgs)
            external_lc_map = Map.fromList (map mkShapeMapping external_lib_dep_pkgs)

            unbuildable_external_lib_deps =
                filter (null . elaborateLibSolverId mapDep) external_lib_dep_sids
            unbuildable_external_exe_deps =
                filter (null . elaborateExeSolverId mapDep) external_exe_dep_sids

            mkPkgNameMapping :: ElaboratedPlanPackage
                             -> (PackageName, (ComponentId, PackageId))
            mkPkgNameMapping dpkg =
                (packageName dpkg, (getComponentId dpkg, packageId dpkg))

            mkShapeMapping :: ElaboratedPlanPackage
                           -> (ComponentId, (OpenUnitId, ModuleShape))
            mkShapeMapping dpkg =
                (getComponentId dpkg, (indef_uid, shape))
              where
                (dcid, shape) = case dpkg of
                    InstallPlan.PreExisting dipkg ->
                        (IPI.installedComponentId dipkg, shapeInstalledPackage dipkg)
                    InstallPlan.Configured elab' ->
                        (elabComponentId elab', elabModuleShape elab')
                    InstallPlan.Installed elab' ->
                        (elabComponentId elab', elabModuleShape elab')
                indef_uid =
                    IndefFullUnitId dcid
                        (Map.fromList [ (req, OpenModuleVar req)
                                      | req <- Set.toList (modShapeRequires shape)])

    filterExeMapDep :: (SolverId -> [ElaboratedPlanPackage])
                    -> PD.PackageDescription -> [PD.BuildInfo]
                    -> SolverId -> [ElaboratedPlanPackage]
    filterExeMapDep mapDep pd bis = filter go . mapDep
      where
        toolDeps = getAllToolDependencies pd =<< bis
        exeKV :: [(PackageName, Set UnqualComponentName)]
        exeKV = map go' toolDeps where
          go' (ExeDependency p n _) = (p, Set.singleton n)

        -- Nothing means wildcard, the complete subset
        exeMap :: Map PackageName (Set UnqualComponentName)
        exeMap = Map.fromListWith mappend exeKV

        go (InstallPlan.Installed _) = unexpectedState
        go (InstallPlan.PreExisting _) = True
        go (InstallPlan.Configured (ElaboratedConfiguredPackage {
              elabPkgSourceId = PackageIdentifier { pkgName, .. },
              elabPkgOrComp,
              ..
            })) = case elabPkgOrComp of
          -- If we can only build the whole package or none of it, then we have
          -- no choice and must build it all.
          ElabPackage   _     -> True
          -- If we can build specific components, lets just build the ones we
          -- actually need.
          ElabComponent comp' ->
            case Ty.compSolverName comp' of
              CD.ComponentExe n -> case Map.lookup pkgName exeMap of
                Just set -> Set.member n set
                -- We may get unwanted components, but they should be from
                -- packages we at least depended on.
                Nothing  -> unexpectedState
              -- If it's not an exe component, it won't satisfy an exe dep
              _  -> False


    elaborateLibSolverId' :: (SolverId -> [ElaboratedPlanPackage])
                      -> SolverId -> [ElaboratedPlanPackage]
    elaborateLibSolverId' mapDep = filter is_lib . mapDep
      where is_lib (InstallPlan.PreExisting _) = True
            is_lib (InstallPlan.Configured elab) =
                case elabPkgOrComp elab of
                    ElabPackage _ -> True
                    ElabComponent comp -> compSolverName comp == CD.ComponentLib
            is_lib (InstallPlan.Installed _) = unexpectedState

    elaborateLibSolverId :: (SolverId -> [ElaboratedPlanPackage])
                      -> SolverId -> [ConfiguredId]
    elaborateLibSolverId mapDep = map configuredId . elaborateLibSolverId' mapDep

    elaborateLibBuildCacheFile :: (SolverId -> [ElaboratedPlanPackage])
                               -> SolverId -> [FilePath]
    elaborateLibBuildCacheFile mapDep = concatMap get_cache_file . mapDep
      where
        get_cache_file (InstallPlan.PreExisting _) = []
        get_cache_file (InstallPlan.Installed elab) = go elab
        get_cache_file (InstallPlan.Configured elab) = go elab

        go elab
            | elabBuildStyle elab == BuildInplaceOnly
            , case elabPkgOrComp elab of
                ElabPackage _ -> True
                ElabComponent comp -> compSolverName comp == CD.ComponentLib
            = [ distPackageCacheFile
                  (elabDistDirParams elaboratedSharedConfig elab)
                  "build" ]
            | otherwise = []

    elaborateExeSolverId :: (SolverId -> [ElaboratedPlanPackage])
                      -> SolverId -> [ConfiguredId]
    elaborateExeSolverId mapDep = map configuredId . filter is_exe . mapDep
      where is_exe (InstallPlan.PreExisting _) = False
            is_exe (InstallPlan.Configured elab) =
                case elabPkgOrComp elab of
                    ElabPackage _ -> True
                    ElabComponent comp ->
                        case compSolverName comp of
                            CD.ComponentExe _ -> True
                            _ -> False
            is_exe (InstallPlan.Installed _) = unexpectedState

    elaborateExePath :: (SolverId -> [ElaboratedPlanPackage])
                     -> SolverId -> [FilePath]
    elaborateExePath mapDep = concatMap get_exe_path . mapDep
      where
        -- Pre-existing executables are assumed to be in PATH
        -- already.  In fact, this should be impossible.
        -- Modest duplication with 'inplace_bin_dir'
        get_exe_path (InstallPlan.PreExisting _) = []
        get_exe_path (InstallPlan.Configured elab) =
            [if elabBuildStyle elab == BuildInplaceOnly
              then distBuildDirectory
                    (elabDistDirParams elaboratedSharedConfig elab) </>
                    "build" </>
                        case elabPkgOrComp elab of
                            ElabPackage _ -> ""
                            ElabComponent comp ->
                                case fmap Cabal.componentNameString
                                          (compComponentName comp) of
                                    Just (Just n) -> display n
                                    _ -> ""
              else InstallDirs.bindir (elabInstallDirs elab)]
        get_exe_path (InstallPlan.Installed _) = unexpectedState

    unexpectedState = error "elaborateInstallPlan: unexpected Installed state"

    elaborateSolverToPackage :: (SolverId -> [ElaboratedPlanPackage])
                             -> SolverPackage UnresolvedPkgLoc
                             -> ElaboratedConfiguredPackage
    elaborateSolverToPackage
        mapDep
        pkg@(SolverPackage (SourcePackage pkgid _gdesc _srcloc _descOverride)
                           _flags _stanzas deps0 exe_deps0) =
        -- Knot tying: the final elab includes the
        -- pkgInstalledId, which is calculated by hashing many
        -- of the other fields of the elaboratedPackage.
        elab
      where
        elab0@ElaboratedConfiguredPackage{..} = elaborateSolverToCommon mapDep pkg
        elab = elab0 {
                elabUnitId = newSimpleUnitId pkgInstalledId,
                elabComponentId = pkgInstalledId,
                elabLinkedInstantiatedWith = Map.empty,
                elabInstallDirs = install_dirs,
                elabRequiresRegistration = requires_reg,
                elabPkgOrComp = ElabPackage $ ElaboratedPackage {..}
            }

        deps = fmap (concatMap (elaborateLibSolverId mapDep)) deps0
        pkgInplaceDependencyBuildCacheFiles = fmap (concatMap (elaborateLibBuildCacheFile mapDep)) deps0

        requires_reg = PD.hasPublicLib elabPkgDescription
        pkgInstalledId
          | shouldBuildInplaceOnly pkg
          = mkComponentId (display pkgid ++ "-inplace")

          | otherwise
          = assert (isJust elabPkgSourceHash) $
            hashedInstalledPackageId
              (packageHashInputs
                elaboratedSharedConfig
                elab)  -- recursive use of elab

          | otherwise
          = error $ "elaborateInstallPlan: non-inplace package "
                 ++ " is missing a source hash: " ++ display pkgid

        buildInfos = PD.allBuildInfo elabPkgDescription
        filterExeMapDepApp = filterExeMapDep mapDep elabPkgDescription buildInfos

        pkgLibDependencies  = deps
        pkgExeDependencies  = fmap (concatMap (elaborateExeSolverId filterExeMapDepApp)) exe_deps0
        pkgExeDependencyPaths = fmap (concatMap (elaborateExePath filterExeMapDepApp)) exe_deps0
        pkgPkgConfigDependencies =
              ordNub
            $ [ (pn, fromMaybe (error $ "pkgPkgConfigDependencies: impossible! "
                                          ++ display pn ++ " from " ++ display pkgid)
                               (pkgConfigDbPkgVersion pkgConfigDB pn))
              | PkgconfigDependency pn _ <- concatMap PD.pkgconfigDepends buildInfos
              ]

        -- Filled in later
        pkgStanzasEnabled  = Set.empty

        install_dirs
          | shouldBuildInplaceOnly pkg
          -- use the ordinary default install dirs
          = (InstallDirs.absoluteInstallDirs
               pkgid
               (newSimpleUnitId pkgInstalledId)
               (compilerInfo compiler)
               InstallDirs.NoCopyDest
               platform
               defaultInstallDirs) {

              InstallDirs.libsubdir  = "", -- absoluteInstallDirs sets these as
              InstallDirs.datasubdir = ""  -- 'undefined' but we have to use
            }                              -- them as "Setup.hs configure" args

          | otherwise
          -- use special simplified install dirs
          = storePackageInstallDirs
              cabalDirLayout
              (compilerId compiler)
              pkgInstalledId

    elaborateSolverToCommon :: (SolverId -> [ElaboratedPlanPackage])
                            -> SolverPackage UnresolvedPkgLoc
                            -> ElaboratedConfiguredPackage
    elaborateSolverToCommon mapDep
        pkg@(SolverPackage (SourcePackage pkgid gdesc srcloc descOverride)
                           flags stanzas deps0 _exe_deps0) =
        elaboratedPackage
      where
        elaboratedPackage = ElaboratedConfiguredPackage {..}

        -- These get filled in later
        elabUnitId          = error "elaborateSolverToCommon: elabUnitId"
        elabComponentId     = error "elaborateSolverToCommon: elabComponentId"
        elabInstantiatedWith = Map.empty
        elabLinkedInstantiatedWith = error "elaborateSolverToCommon: elabLinkedInstantiatedWith"
        elabPkgOrComp       = error "elaborateSolverToCommon: elabPkgOrComp"
        elabInstallDirs     = error "elaborateSolverToCommon: elabInstallDirs"
        elabRequiresRegistration = error "elaborateSolverToCommon: elabRequiresRegistration"
        elabModuleShape     = error "elaborateSolverToCommon: elabModuleShape"

        elabPkgSourceId     = pkgid
        elabPkgDescription  = let Right (desc, _) =
                                    PD.finalizePD
                                    flags elabEnabledSpec (const True)
                                    platform (compilerInfo compiler)
                                    [] gdesc
                               in desc
        elabInternalPackages = Cabal.getInternalPackages gdesc
        elabFlagAssignment  = flags
        elabFlagDefaults    = [ (Cabal.flagName flag, Cabal.flagDefault flag)
                              | flag <- PD.genPackageFlags gdesc ]

        elabEnabledSpec      = enableStanzas stanzas
        elabStanzasAvailable = Set.fromList stanzas
        elabStanzasRequested =
            -- NB: even if a package stanza is requested, if the package
            -- doesn't actually have any of that stanza we omit it from
            -- the request, to ensure that we don't decide that this
            -- package needs to be rebuilt.  (It needs to be done here,
            -- because the ElaboratedConfiguredPackage is where we test
            -- whether or not there have been changes.)
            Map.fromList $ [ (TestStanzas,  v) | v <- maybeToList tests
                                               , _ <- PD.testSuites elabPkgDescription ]
                        ++ [ (BenchStanzas, v) | v <- maybeToList benchmarks
                                               , _ <- PD.benchmarks elabPkgDescription ]
          where
            tests, benchmarks :: Maybe Bool
            tests      = perPkgOptionMaybe pkgid packageConfigTests
            benchmarks = perPkgOptionMaybe pkgid packageConfigBenchmarks

        -- This is a placeholder which will get updated by 'pruneInstallPlanPass1'
        -- and 'pruneInstallPlanPass2'.  We can't populate it here
        -- because whether or not tests/benchmarks should be enabled
        -- is heuristically calculated based on whether or not the
        -- dependencies of the test suite have already been installed,
        -- but this function doesn't know what is installed (since
        -- we haven't improved the plan yet), so we do it in another pass.
        -- Check the comments of those functions for more details.
        elabBuildTargets    = []
        elabTestTargets     = []
        elabReplTarget      = Nothing
        elabBuildHaddocks   = False

        elabPkgSourceLocation = srcloc
        elabPkgSourceHash   = Map.lookup pkgid sourcePackageHashes
        elabLocalToProject  = isLocalToProject pkg
        elabBuildStyle      = if shouldBuildInplaceOnly pkg
                                then BuildInplaceOnly else BuildAndInstall
        elabBuildPackageDBStack    = buildAndRegisterDbs
        elabRegisterPackageDBStack = buildAndRegisterDbs

        elabSetupScriptStyle       = packageSetupScriptStyle elabPkgDescription
        -- Computing the deps here is a little awful
        deps = fmap (concatMap (elaborateLibSolverId mapDep)) deps0
        elabSetupScriptCliVersion  = packageSetupScriptSpecVersion
                                      elabSetupScriptStyle elabPkgDescription deps
        elabSetupPackageDBStack    = buildAndRegisterDbs

        buildAndRegisterDbs
          | shouldBuildInplaceOnly pkg = inplacePackageDbs
          | otherwise                  = storePackageDbs

        elabPkgDescriptionOverride = descOverride

        elabVanillaLib    = perPkgOptionFlag pkgid True packageConfigVanillaLib --TODO: [required feature]: also needs to be handled recursively
        elabSharedLib     = pkgid `Set.member` pkgsUseSharedLibrary
        elabDynExe        = perPkgOptionFlag pkgid False packageConfigDynExe
        elabGHCiLib       = perPkgOptionFlag pkgid False packageConfigGHCiLib --TODO: [required feature] needs to default to enabled on windows still

        elabProfExe       = perPkgOptionFlag pkgid False packageConfigProf
        elabProfLib       = pkgid `Set.member` pkgsUseProfilingLibrary

        (elabProfExeDetail,
         elabProfLibDetail) = perPkgOptionLibExeFlag pkgid ProfDetailDefault
                               packageConfigProfDetail
                               packageConfigProfLibDetail
        elabCoverage      = perPkgOptionFlag pkgid False packageConfigCoverage

        elabOptimization  = perPkgOptionFlag pkgid NormalOptimisation packageConfigOptimization
        elabSplitObjs     = perPkgOptionFlag pkgid False packageConfigSplitObjs
        elabStripLibs     = perPkgOptionFlag pkgid False packageConfigStripLibs
        elabStripExes     = perPkgOptionFlag pkgid False packageConfigStripExes
        elabDebugInfo     = perPkgOptionFlag pkgid NoDebugInfo packageConfigDebugInfo

        -- Combine the configured compiler prog settings with the user-supplied
        -- config. For the compiler progs any user-supplied config was taken
        -- into account earlier when configuring the compiler so its ok that
        -- our configured settings for the compiler override the user-supplied
        -- config here.
        elabProgramPaths  = Map.fromList
                             [ (programId prog, programPath prog)
                             | prog <- configuredPrograms compilerprogdb ]
                        <> perPkgOptionMapLast pkgid packageConfigProgramPaths
        elabProgramArgs   = Map.fromList
                             [ (programId prog, args)
                             | prog <- configuredPrograms compilerprogdb
                             , let args = programOverrideArgs prog
                             , not (null args)
                             ]
                        <> perPkgOptionMapMappend pkgid packageConfigProgramArgs
        elabProgramPathExtra    = perPkgOptionNubList pkgid packageConfigProgramPathExtra
        elabConfigureScriptArgs = perPkgOptionList pkgid packageConfigConfigureArgs
        elabExtraLibDirs        = perPkgOptionList pkgid packageConfigExtraLibDirs
        elabExtraFrameworkDirs  = perPkgOptionList pkgid packageConfigExtraFrameworkDirs
        elabExtraIncludeDirs    = perPkgOptionList pkgid packageConfigExtraIncludeDirs
        elabProgPrefix          = perPkgOptionMaybe pkgid packageConfigProgPrefix
        elabProgSuffix          = perPkgOptionMaybe pkgid packageConfigProgSuffix


        elabHaddockHoogle       = perPkgOptionFlag pkgid False packageConfigHaddockHoogle
        elabHaddockHtml         = perPkgOptionFlag pkgid False packageConfigHaddockHtml
        elabHaddockHtmlLocation = perPkgOptionMaybe pkgid packageConfigHaddockHtmlLocation
        elabHaddockForeignLibs  = perPkgOptionFlag pkgid False packageConfigHaddockForeignLibs
        elabHaddockExecutables  = perPkgOptionFlag pkgid False packageConfigHaddockExecutables
        elabHaddockTestSuites   = perPkgOptionFlag pkgid False packageConfigHaddockTestSuites
        elabHaddockBenchmarks   = perPkgOptionFlag pkgid False packageConfigHaddockBenchmarks
        elabHaddockInternal     = perPkgOptionFlag pkgid False packageConfigHaddockInternal
        elabHaddockCss          = perPkgOptionMaybe pkgid packageConfigHaddockCss
        elabHaddockHscolour     = perPkgOptionFlag pkgid False packageConfigHaddockHscolour
        elabHaddockHscolourCss  = perPkgOptionMaybe pkgid packageConfigHaddockHscolourCss
        elabHaddockContents     = perPkgOptionMaybe pkgid packageConfigHaddockContents

    perPkgOptionFlag  :: PackageId -> a ->  (PackageConfig -> Flag a) -> a
    perPkgOptionMaybe :: PackageId ->       (PackageConfig -> Flag a) -> Maybe a
    perPkgOptionList  :: PackageId ->       (PackageConfig -> [a])    -> [a]

    perPkgOptionFlag  pkgid def f = fromFlagOrDefault def (lookupPerPkgOption pkgid f)
    perPkgOptionMaybe pkgid     f = flagToMaybe (lookupPerPkgOption pkgid f)
    perPkgOptionList  pkgid     f = lookupPerPkgOption pkgid f
    perPkgOptionNubList    pkgid f = fromNubList   (lookupPerPkgOption pkgid f)
    perPkgOptionMapLast    pkgid f = getMapLast    (lookupPerPkgOption pkgid f)
    perPkgOptionMapMappend pkgid f = getMapMappend (lookupPerPkgOption pkgid f)

    perPkgOptionLibExeFlag pkgid def fboth flib = (exe, lib)
      where
        exe = fromFlagOrDefault def bothflag
        lib = fromFlagOrDefault def (bothflag <> libflag)

        bothflag = lookupPerPkgOption pkgid fboth
        libflag  = lookupPerPkgOption pkgid flib

    lookupPerPkgOption :: (Package pkg, Monoid m)
                       => pkg -> (PackageConfig -> m) -> m
    lookupPerPkgOption pkg f
      -- the project config specifies values that apply to packages local to
      -- but by default non-local packages get all default config values
      -- the project, and can specify per-package values for any package,
      | isLocalToProject pkg = local `mappend` perpkg
      | otherwise            =                 perpkg
      where
        local  = f localPackagesConfig
        perpkg = maybe mempty f (Map.lookup (packageName pkg) perPackageConfig)

    inplacePackageDbs = storePackageDbs
                     ++ [ distPackageDB (compilerId compiler) ]

    storePackageDbs   = [ GlobalPackageDB
                        , cabalStorePackageDB (compilerId compiler) ]

    -- For this local build policy, every package that lives in a local source
    -- dir (as opposed to a tarball), or depends on such a package, will be
    -- built inplace into a shared dist dir. Tarball packages that depend on
    -- source dir packages will also get unpacked locally.
    shouldBuildInplaceOnly :: SolverPackage loc -> Bool
    shouldBuildInplaceOnly pkg = Set.member (packageId pkg)
                                            pkgsToBuildInplaceOnly

    pkgsToBuildInplaceOnly :: Set PackageId
    pkgsToBuildInplaceOnly =
        Set.fromList
      $ map packageId
      $ SolverInstallPlan.reverseDependencyClosure
          solverPlan
          [ PlannedId (packageId pkg)
          | pkg <- localPackages ]

    isLocalToProject :: Package pkg => pkg -> Bool
    isLocalToProject pkg = Set.member (packageId pkg)
                                      pkgsLocalToProject

    pkgsLocalToProject :: Set PackageId
    pkgsLocalToProject = Set.fromList [ packageId pkg | pkg <- localPackages ]

    pkgsUseSharedLibrary :: Set PackageId
    pkgsUseSharedLibrary =
        packagesWithLibDepsDownwardClosedProperty needsSharedLib
      where
        needsSharedLib pkg =
            fromMaybe compilerShouldUseSharedLibByDefault
                      (liftM2 (||) pkgSharedLib pkgDynExe)
          where
            pkgid        = packageId pkg
            pkgSharedLib = perPkgOptionMaybe pkgid packageConfigSharedLib
            pkgDynExe    = perPkgOptionMaybe pkgid packageConfigDynExe

    --TODO: [code cleanup] move this into the Cabal lib. It's currently open
    -- coded in Distribution.Simple.Configure, but should be made a proper
    -- function of the Compiler or CompilerInfo.
    compilerShouldUseSharedLibByDefault =
      case compilerFlavor compiler of
        GHC   -> GHC.isDynamic compiler
        GHCJS -> GHCJS.isDynamic compiler
        _     -> False

    pkgsUseProfilingLibrary :: Set PackageId
    pkgsUseProfilingLibrary =
        packagesWithLibDepsDownwardClosedProperty needsProfilingLib
      where
        needsProfilingLib pkg =
            fromFlagOrDefault False (profBothFlag <> profLibFlag)
          where
            pkgid        = packageId pkg
            profBothFlag = lookupPerPkgOption pkgid packageConfigProf
            profLibFlag  = lookupPerPkgOption pkgid packageConfigProfLib
            --TODO: [code cleanup] unused: the old deprecated packageConfigProfExe

    libDepGraph = Graph.fromDistinctList $
                    map NonSetupLibDepSolverPlanPackage
                        (SolverInstallPlan.toList solverPlan)

    packagesWithLibDepsDownwardClosedProperty property =
        Set.fromList
      . map packageId
      . fromMaybe []
      $ Graph.closure
          libDepGraph
          [ Graph.nodeKey pkg
          | pkg <- SolverInstallPlan.toList solverPlan
          , property pkg ] -- just the packages that satisfy the propety
      --TODO: [nice to have] this does not check the config consistency,
      -- e.g. a package explicitly turning off profiling, but something
      -- depending on it that needs profiling. This really needs a separate
      -- package config validation/resolution pass.

      --TODO: [nice to have] config consistency checking:
      -- + profiling libs & exes, exe needs lib, recursive
      -- + shared libs & exes, exe needs lib, recursive
      -- + vanilla libs & exes, exe needs lib, recursive
      -- + ghci or shared lib needed by TH, recursive, ghc version dependent

-- | A newtype for 'SolverInstallPlan.SolverPlanPackage' for which the
-- dependency graph considers only dependencies on libraries which are
-- NOT from setup dependencies.  Used to compute the set
-- of packages needed for profiling and dynamic libraries.
newtype NonSetupLibDepSolverPlanPackage
    = NonSetupLibDepSolverPlanPackage
    { unNonSetupLibDepSolverPlanPackage :: SolverInstallPlan.SolverPlanPackage }

instance Package NonSetupLibDepSolverPlanPackage where
    packageId = packageId . unNonSetupLibDepSolverPlanPackage

instance IsNode NonSetupLibDepSolverPlanPackage where
    type Key NonSetupLibDepSolverPlanPackage = SolverId
    nodeKey = nodeKey . unNonSetupLibDepSolverPlanPackage
    nodeNeighbors (NonSetupLibDepSolverPlanPackage spkg)
        = ordNub $ CD.nonSetupDeps (resolverPackageLibDeps spkg)

type InstS = Map UnitId ElaboratedPlanPackage
type InstM a = State InstS a

getComponentId :: ElaboratedPlanPackage
               -> ComponentId
getComponentId (InstallPlan.PreExisting dipkg) = IPI.installedComponentId dipkg
getComponentId (InstallPlan.Configured elab) = elabComponentId elab
getComponentId (InstallPlan.Installed elab) = elabComponentId elab

instantiateInstallPlan :: ElaboratedInstallPlan -> ElaboratedInstallPlan
instantiateInstallPlan plan =
    InstallPlan.new (IndependentGoals False)
                    (Graph.fromDistinctList (Map.elems ready_map))
  where
    pkgs = InstallPlan.toList plan

    cmap = Map.fromList [ (getComponentId pkg, pkg) | pkg <- pkgs ]

    instantiateUnitId :: ComponentId -> Map ModuleName Module
                      -> InstM DefUnitId
    instantiateUnitId cid insts = state $ \s ->
        case Map.lookup uid s of
            Nothing ->
                -- Knot tied
                let (r, s') = runState (instantiateComponent uid cid insts)
                                       (Map.insert uid r s)
                in (def_uid, Map.insert uid r s')
            Just _ -> (def_uid, s)
      where
        def_uid = mkDefUnitId cid insts
        uid = unDefUnitId def_uid

    instantiateComponent
        :: UnitId -> ComponentId -> Map ModuleName Module
        -> InstM ElaboratedPlanPackage
    instantiateComponent uid cid insts
      | Just planpkg <- Map.lookup cid cmap
      = case planpkg of
          InstallPlan.Configured (elab@ElaboratedConfiguredPackage
                                    { elabPkgOrComp = ElabComponent comp }) -> do
            deps <- mapM (substUnitId insts)
                         (compLinkedLibDependencies comp)
            let getDep (Module dep_uid _) = [dep_uid]
            return $ InstallPlan.Configured elab {
                    elabUnitId = uid,
                    elabComponentId = cid,
                    elabInstantiatedWith = insts,
                    elabPkgOrComp = ElabComponent comp {
                        compNonSetupDependencies =
                            (if Map.null insts then [] else [newSimpleUnitId cid]) ++
                            ordNub (map unDefUnitId
                                (deps ++ concatMap getDep (Map.elems insts)))
                    }
                }
          _ -> return planpkg
      | otherwise = error ("instantiateComponent: " ++ display cid)

    substUnitId :: Map ModuleName Module -> OpenUnitId -> InstM DefUnitId
    substUnitId _ (DefiniteUnitId uid) =
        return uid
    substUnitId subst (IndefFullUnitId cid insts) = do
        insts' <- substSubst subst insts
        instantiateUnitId cid insts'

    -- NB: NOT composition
    substSubst :: Map ModuleName Module
               -> Map ModuleName OpenModule
               -> InstM (Map ModuleName Module)
    substSubst subst insts = T.mapM (substModule subst) insts

    substModule :: Map ModuleName Module -> OpenModule -> InstM Module
    substModule subst (OpenModuleVar mod_name)
        | Just m <- Map.lookup mod_name subst = return m
        | otherwise = error "substModule: non-closing substitution"
    substModule subst (OpenModule uid mod_name) = do
        uid' <- substUnitId subst uid
        return (Module uid' mod_name)

    indefiniteUnitId :: ComponentId -> InstM UnitId
    indefiniteUnitId cid = do
        let uid = newSimpleUnitId cid
        r <- indefiniteComponent uid cid
        state $ \s -> (uid, Map.insert uid r s)

    indefiniteComponent :: UnitId -> ComponentId -> InstM ElaboratedPlanPackage
    indefiniteComponent _uid cid
      | Just planpkg <- Map.lookup cid cmap
      = return planpkg
      | otherwise = error ("indefiniteComponent: " ++ display cid)

    ready_map = execState work Map.empty

    work = forM_ pkgs $ \pkg ->
            case pkg of
                InstallPlan.Configured elab
                    | not (Map.null (elabLinkedInstantiatedWith elab))
                    -> indefiniteUnitId (elabComponentId elab)
                        >> return ()
                _ -> instantiateUnitId (getComponentId pkg) Map.empty
                        >> return ()

---------------------------
-- Build targets
--

-- Refer to ProjectPlanning.Types for details of these important types:

-- data PackageTarget = ...
-- data ComponentTarget = ...
-- data SubComponentTarget = ...


--TODO: this needs to report some user target/config errors
elaboratePackageTargets :: ElaboratedConfiguredPackage -> [PackageTarget]
                        -> ([ComponentTarget], [ComponentTarget], Maybe ComponentTarget, Bool)
elaboratePackageTargets ElaboratedConfiguredPackage{..} targets =
    let buildTargets  = nubComponentTargets
                      . map compatSubComponentTargets
                      . concatMap elaborateBuildTarget
                      $ targets

        testTargets = nubComponentTargets
                    . filter isTestComponentTarget
                    . map compatSubComponentTargets
                    . concatMap elaborateTestTarget
                    $ targets

        --TODO: instead of listToMaybe we should be reporting an error here
        replTargets   = listToMaybe
                      . nubComponentTargets
                      . map compatSubComponentTargets
                      . concatMap elaborateReplTarget
                      $ targets
        buildHaddocks = HaddockDefaultComponents `elem` targets

     in (buildTargets, testTargets, replTargets, buildHaddocks)
  where
    --TODO: need to report an error here if defaultComponents is empty
    elaborateBuildTarget  BuildDefaultComponents    = pkgDefaultComponents
    elaborateBuildTarget (BuildSpecificComponent t) = [t]
    -- TODO: We need to build test components as well
    -- should this be configurable, i.e. to /just/ run, not try to build
    elaborateBuildTarget  TestDefaultComponents     = pkgDefaultComponents
    elaborateBuildTarget (TestSpecificComponent t)  = [t]
    elaborateBuildTarget  _                         = []

    elaborateTestTarget  TestDefaultComponents    = pkgDefaultComponents
    elaborateTestTarget (TestSpecificComponent t) = [t]
    elaborateTestTarget  _                        = []

    --TODO: need to report an error here if defaultComponents is empty
    elaborateReplTarget  ReplDefaultComponent     = take 1 pkgDefaultComponents
    elaborateReplTarget (ReplSpecificComponent t) = [t]
    elaborateReplTarget  _                        = []

    pkgDefaultComponents =
        [ ComponentTarget cname WholeComponent
        | c <- Cabal.pkgComponents elabPkgDescription
        , PD.buildable (Cabal.componentBuildInfo c)
        , let cname = Cabal.componentName c
        , enabledOptionalStanza cname
        ]
      where
        enabledOptionalStanza cname =
          case componentOptionalStanza cname of
            Nothing     -> True
            Just stanza -> Map.lookup stanza elabStanzasRequested
                        == Just True

    -- Not all Cabal Setup.hs versions support sub-component targets, so switch
    -- them over to the whole component
    compatSubComponentTargets :: ComponentTarget -> ComponentTarget
    compatSubComponentTargets target@(ComponentTarget cname _subtarget)
      | not setupHsSupportsSubComponentTargets
                  = ComponentTarget cname WholeComponent
      | otherwise = target

    -- Actually the reality is that no current version of Cabal's Setup.hs
    -- build command actually support building specific files or modules.
    setupHsSupportsSubComponentTargets = False
    -- TODO: when that changes, adjust this test, e.g.
    -- | pkgSetupScriptCliVersion >= Version [x,y] []

    nubComponentTargets :: [ComponentTarget] -> [ComponentTarget]
    nubComponentTargets =
        concatMap (wholeComponentOverrides . map snd)
      . groupBy ((==)    `on` fst)
      . sortBy  (compare `on` fst)
      . map (\t@(ComponentTarget cname _) -> (cname, t))

    -- If we're building the whole component then that the only target all we
    -- need, otherwise we can have several targets within the component.
    wholeComponentOverrides :: [ComponentTarget] -> [ComponentTarget]
    wholeComponentOverrides ts =
      case [ t | t@(ComponentTarget _ WholeComponent) <- ts ] of
        (t:_) -> [t]
        []    -> ts

pkgHasEphemeralBuildTargets :: ElaboratedConfiguredPackage -> Bool
pkgHasEphemeralBuildTargets elab =
    isJust (elabReplTarget elab)
 || (not . null) (elabTestTargets elab)
 || (not . null) [ () | ComponentTarget _ subtarget <- elabBuildTargets elab
                      , subtarget /= WholeComponent ]

-- | The components that we'll build all of, meaning that after they're built
-- we can skip building them again (unlike with building just some modules or
-- other files within a component).
--
elabBuildTargetWholeComponents :: ElaboratedConfiguredPackage
                              -> Set ComponentName
elabBuildTargetWholeComponents elab =
    Set.fromList
      [ cname | ComponentTarget cname WholeComponent <- elabBuildTargets elab ]



------------------------------------------------------------------------------
-- * Install plan pruning
------------------------------------------------------------------------------

-- | Given a set of package targets (and optionally component targets within
-- those packages), take the subset of the install plan needed to build those
-- targets. Also, update the package config to specify which optional stanzas
-- to enable, and which targets within each package to build.
--
pruneInstallPlanToTargets :: Map UnitId [PackageTarget]
                          -> ElaboratedInstallPlan -> ElaboratedInstallPlan
pruneInstallPlanToTargets perPkgTargetsMap elaboratedPlan =
    InstallPlan.new (InstallPlan.planIndepGoals elaboratedPlan)
  . Graph.fromDistinctList
    -- We have to do this in two passes
  . pruneInstallPlanPass2
  . pruneInstallPlanPass1 perPkgTargetsMap
  . InstallPlan.toList
  $ elaboratedPlan

-- | This is a temporary data type, where we temporarily
-- override the graph dependencies of an 'ElaboratedPackage',
-- so we can take a closure over them.  We'll throw out the
-- overriden dependencies when we're done so it's strictly temporary.
--
-- For 'ElaboratedComponent', this the cached unit IDs always
-- coincide with the real thing.
data PrunedPackage = PrunedPackage ElaboratedConfiguredPackage [UnitId]

instance Package PrunedPackage where
    packageId (PrunedPackage elab _) = packageId elab

instance HasUnitId PrunedPackage where
    installedUnitId = nodeKey

instance IsNode PrunedPackage where
    type Key PrunedPackage = UnitId
    nodeKey (PrunedPackage elab _)  = nodeKey elab
    nodeNeighbors (PrunedPackage _ deps) = deps

fromPrunedPackage :: PrunedPackage -> ElaboratedConfiguredPackage
fromPrunedPackage (PrunedPackage elab _) = elab

-- | The first pass does three things:
--
-- * Set the build targets based on the user targets (but not rev deps yet).
-- * A first go at determining which optional stanzas (testsuites, benchmarks)
--   are needed. We have a second go in the next pass.
-- * Take the dependency closure using pruned dependencies. We prune deps that
--   are used only by unneeded optional stanzas. These pruned deps are only
--   used for the dependency closure and are not persisted in this pass.
--
pruneInstallPlanPass1 :: Map UnitId [PackageTarget]
                      -> [ElaboratedPlanPackage]
                      -> [ElaboratedPlanPackage]
pruneInstallPlanPass1 perPkgTargetsMap pkgs =
    map (mapConfiguredPackage fromPrunedPackage)
        (fromMaybe [] $ Graph.closure g roots)
  where
    pkgs' = map (mapConfiguredPackage prune) pkgs
    g = Graph.fromDistinctList pkgs'

    prune elab =
        let elab' = (pruneOptionalStanzas . setElabBuildTargets) elab
        in PrunedPackage elab' (pruneOptionalDependencies elab')

    roots = mapMaybe find_root pkgs'
    find_root (InstallPlan.Configured (PrunedPackage elab _)) =
        if not (null (elabBuildTargets elab)
                    && null (elabTestTargets elab)
                    && isNothing (elabReplTarget elab)
                    && not (elabBuildHaddocks elab))
            then Just (installedUnitId elab)
            else Nothing
    find_root _ = Nothing

    -- Elaborate and set the targets we'll build for this package. This is just
    -- based on the targets from the user, not targets implied by reverse
    -- dependencies. Those comes in the second pass once we know the rev deps.
    --
    setElabBuildTargets elab =
        elab {
          elabBuildTargets   = mapMaybe targetForElab buildTargets,
          elabTestTargets    = mapMaybe targetForElab testTargets,
          elabReplTarget     = replTarget >>= targetForElab,
          elabBuildHaddocks  = buildHaddocks
        }
      where
        (buildTargets, testTargets, replTarget, buildHaddocks)
                = elaboratePackageTargets elab targets
        targets = fromMaybe []
                $ Map.lookup (installedUnitId elab) perPkgTargetsMap
        targetForElab tgt@(ComponentTarget cname _) =
            case elabPkgOrComp elab of
                ElabPackage _ -> Just tgt -- always valid
                ElabComponent comp
                    -- Only if the component name matches
                    | compComponentName comp == Just cname -> Just tgt
                    | otherwise -> Nothing

    -- Decide whether or not to enable testsuites and benchmarks
    --
    -- The testsuite and benchmark targets are somewhat special in that we need
    -- to configure the packages with them enabled, and we need to do that even
    -- if we only want to build one of several testsuites.
    --
    -- There are two cases in which we will enable the testsuites (or
    -- benchmarks): if one of the targets is a testsuite, or if all of the
    -- testsuite dependencies are already cached in the store. The rationale
    -- for the latter is to minimise how often we have to reconfigure due to
    -- the particular targets we choose to build. Otherwise choosing to build
    -- a testsuite target, and then later choosing to build an exe target
    -- would involve unnecessarily reconfiguring the package with testsuites
    -- disabled. Technically this introduces a little bit of stateful
    -- behaviour to make this "sticky", but it should be benign.
    --
    pruneOptionalStanzas :: ElaboratedConfiguredPackage -> ElaboratedConfiguredPackage
    pruneOptionalStanzas elab@ElaboratedConfiguredPackage{ elabPkgOrComp = ElabPackage pkg } =
        elab {
            elabPkgOrComp = ElabPackage (pkg { pkgStanzasEnabled = stanzas })
        }
      where
        stanzas :: Set OptionalStanza
        stanzas = optionalStanzasRequiredByTargets  elab
               <> optionalStanzasRequestedByDefault elab
               <> optionalStanzasWithDepsAvailable availablePkgs elab pkg
    pruneOptionalStanzas elab = elab

    -- Calculate package dependencies but cut out those needed only by
    -- optional stanzas that we've determined we will not enable.
    -- These pruned deps are not persisted in this pass since they're based on
    -- the optional stanzas and we'll make further tweaks to the optional
    -- stanzas in the next pass.
    --
    pruneOptionalDependencies :: ElaboratedConfiguredPackage -> [UnitId]
    pruneOptionalDependencies elab@ElaboratedConfiguredPackage{ elabPkgOrComp = ElabComponent _ }
        = InstallPlan.depends elab -- no pruning
    pruneOptionalDependencies ElaboratedConfiguredPackage{ elabPkgOrComp = ElabPackage pkg }
        = (CD.flatDeps . CD.filterDeps keepNeeded) (pkgOrderDependencies pkg)
      where
        keepNeeded (CD.ComponentTest  _) _ = TestStanzas  `Set.member` stanzas
        keepNeeded (CD.ComponentBench _) _ = BenchStanzas `Set.member` stanzas
        keepNeeded _                     _ = True
        stanzas = pkgStanzasEnabled pkg

    optionalStanzasRequiredByTargets :: ElaboratedConfiguredPackage
                                     -> Set OptionalStanza
    optionalStanzasRequiredByTargets pkg =
      Set.fromList
        [ stanza
        | ComponentTarget cname _ <- elabBuildTargets pkg
                                  ++ elabTestTargets pkg
                                  ++ maybeToList (elabReplTarget pkg)
        , stanza <- maybeToList (componentOptionalStanza cname)
        ]

    optionalStanzasRequestedByDefault :: ElaboratedConfiguredPackage
                                      -> Set OptionalStanza
    optionalStanzasRequestedByDefault =
        Map.keysSet
      . Map.filter (id :: Bool -> Bool)
      . elabStanzasRequested

    availablePkgs =
      Set.fromList
        [ installedUnitId pkg
        | InstallPlan.PreExisting pkg <- pkgs ]

-- | Given a set of already installed packages @availablePkgs@,
-- determine the set of available optional stanzas from @pkg@
-- which have all of their dependencies already installed.  This is used
-- to implement "sticky" testsuites, where once we have installed
-- all of the deps needed for the test suite, we go ahead and
-- enable it always.
optionalStanzasWithDepsAvailable :: Set UnitId
                                 -> ElaboratedConfiguredPackage
                                 -> ElaboratedPackage
                                 -> Set OptionalStanza
optionalStanzasWithDepsAvailable availablePkgs elab pkg =
    Set.fromList
      [ stanza
      | stanza <- Set.toList (elabStanzasAvailable elab)
      , let deps :: [UnitId]
            deps = CD.select (optionalStanzaDeps stanza)
                             -- TODO: probably need to select other
                             -- dep types too eventually
                             (pkgOrderDependencies pkg)
      , all (`Set.member` availablePkgs) deps
      ]
  where
    optionalStanzaDeps TestStanzas  (CD.ComponentTest  _) = True
    optionalStanzaDeps BenchStanzas (CD.ComponentBench _) = True
    optionalStanzaDeps _            _                     = False


-- The second pass does three things:
--
-- * A second go at deciding which optional stanzas to enable.
-- * Prune the dependencies based on the final choice of optional stanzas.
-- * Extend the targets within each package to build, now we know the reverse
--   dependencies, ie we know which libs are needed as deps by other packages.
--
-- Achieving sticky behaviour with enabling\/disabling optional stanzas is
-- tricky. The first approximation was handled by the first pass above, but
-- it's not quite enough. That pass will enable stanzas if all of the deps
-- of the optional stanza are already installed /in the store/. That's important
-- but it does not account for dependencies that get built inplace as part of
-- the project. We cannot take those inplace build deps into account in the
-- pruning pass however because we don't yet know which ones we're going to
-- build. Once we do know, we can have another go and enable stanzas that have
-- all their deps available. Now we can consider all packages in the pruned
-- plan to be available, including ones we already decided to build from
-- source.
--
-- Deciding which targets to build depends on knowing which packages have
-- reverse dependencies (ie are needed). This requires the result of first
-- pass, which is another reason we have to split it into two passes.
--
-- Note that just because we might enable testsuites or benchmarks (in the
-- first or second pass) doesn't mean that we build all (or even any) of them.
-- That depends on which targets we picked in the first pass.
--
pruneInstallPlanPass2 :: [ElaboratedPlanPackage]
                      -> [ElaboratedPlanPackage]
pruneInstallPlanPass2 pkgs =
    map (mapConfiguredPackage setStanzasDepsAndTargets) pkgs
  where
    setStanzasDepsAndTargets elab =
        elab {
          elabBuildTargets = ordNub
                           $ elabBuildTargets elab
                          ++ libTargetsRequiredForRevDeps
                          ++ exeTargetsRequiredForRevDeps,
          elabPkgOrComp =
            case elabPkgOrComp elab of
              ElabPackage pkg ->
                let stanzas = pkgStanzasEnabled pkg
                           <> optionalStanzasWithDepsAvailable availablePkgs elab pkg
                    keepNeeded (CD.ComponentTest  _) _ = TestStanzas  `Set.member` stanzas
                    keepNeeded (CD.ComponentBench _) _ = BenchStanzas `Set.member` stanzas
                    keepNeeded _                     _ = True
                in ElabPackage $ pkg {
                  pkgStanzasEnabled = stanzas,
                  pkgLibDependencies   = CD.filterDeps keepNeeded (pkgLibDependencies pkg),
                  pkgExeDependencies   = CD.filterDeps keepNeeded (pkgExeDependencies pkg),
                  pkgExeDependencyPaths = CD.filterDeps keepNeeded (pkgExeDependencyPaths pkg),
                  pkgInplaceDependencyBuildCacheFiles = CD.filterDeps keepNeeded (pkgInplaceDependencyBuildCacheFiles pkg)
                }
              r@(ElabComponent _) -> r
        }
      where
        libTargetsRequiredForRevDeps =
          [ ComponentTarget Cabal.defaultLibName WholeComponent
          | installedUnitId elab `Set.member` hasReverseLibDeps
          ]
        exeTargetsRequiredForRevDeps =
          -- TODO: allow requesting executable with different name
          -- than package name
          [ ComponentTarget (Cabal.CExeName
                             $ packageNameToUnqualComponentName
                             $ packageName $ elabPkgSourceId elab)
                            WholeComponent
          | installedUnitId elab `Set.member` hasReverseExeDeps
          ]


    availablePkgs :: Set UnitId
    availablePkgs = Set.fromList (map installedUnitId pkgs)

    hasReverseLibDeps :: Set UnitId
    hasReverseLibDeps =
      Set.fromList [ depid
                   | InstallPlan.Configured pkg <- pkgs
                   , depid <- elabOrderLibDependencies pkg ]

    hasReverseExeDeps :: Set UnitId
    hasReverseExeDeps =
      Set.fromList [ depid
                   | InstallPlan.Configured pkg <- pkgs
                   , depid <- elabOrderExeDependencies pkg ]

mapConfiguredPackage :: (srcpkg -> srcpkg')
                     -> InstallPlan.GenericPlanPackage ipkg srcpkg
                     -> InstallPlan.GenericPlanPackage ipkg srcpkg'
mapConfiguredPackage f (InstallPlan.Configured pkg) =
  InstallPlan.Configured (f pkg)
mapConfiguredPackage f (InstallPlan.Installed pkg) =
  InstallPlan.Installed (f pkg)
mapConfiguredPackage _ (InstallPlan.PreExisting pkg) =
  InstallPlan.PreExisting pkg

componentOptionalStanza :: Cabal.ComponentName -> Maybe OptionalStanza
componentOptionalStanza (Cabal.CTestName  _) = Just TestStanzas
componentOptionalStanza (Cabal.CBenchName _) = Just BenchStanzas
componentOptionalStanza _                    = Nothing

------------------------------------
-- Support for --only-dependencies
--

-- | Try to remove the given targets from the install plan.
--
-- This is not always possible.
--
pruneInstallPlanToDependencies :: Set UnitId
                               -> ElaboratedInstallPlan
                               -> Either CannotPruneDependencies
                                         ElaboratedInstallPlan
pruneInstallPlanToDependencies pkgTargets installPlan =
    assert (all (isJust . InstallPlan.lookup installPlan)
                (Set.toList pkgTargets)) $

    fmap (InstallPlan.new (InstallPlan.planIndepGoals installPlan))
  . checkBrokenDeps
  . Graph.fromDistinctList
  . filter (\pkg -> installedUnitId pkg `Set.notMember` pkgTargets)
  . InstallPlan.toList
  $ installPlan
    where
      -- Our strategy is to remove the packages we don't want and then check
      -- if the remaining graph is broken or not, ie any packages with dangling
      -- dependencies. If there are then we cannot prune the given targets.
      checkBrokenDeps :: Graph.Graph ElaboratedPlanPackage
                      -> Either CannotPruneDependencies
                                (Graph.Graph ElaboratedPlanPackage)
      checkBrokenDeps graph =
        case Graph.broken graph of
          []             -> Right graph
          brokenPackages ->
            Left $ CannotPruneDependencies
             [ (pkg, missingDeps)
             | (pkg, missingDepIds) <- brokenPackages
             , let missingDeps = catMaybes (map lookupDep missingDepIds)
             ]
            where
              -- lookup in the original unpruned graph
              lookupDep = InstallPlan.lookup installPlan

-- | It is not always possible to prune to only the dependencies of a set of
-- targets. It may be the case that removing a package leaves something else
-- that still needed the pruned package.
--
-- This lists all the packages that would be broken, and their dependencies
-- that would be missing if we did prune.
--
newtype CannotPruneDependencies =
        CannotPruneDependencies [(ElaboratedPlanPackage,
                                  [ElaboratedPlanPackage])]
#if MIN_VERSION_base(4,8,0)
  deriving (Show, Typeable)
#else
  deriving (Typeable)

instance Show CannotPruneDependencies where
  show = renderCannotPruneDependencies
#endif

instance Exception CannotPruneDependencies where
#if MIN_VERSION_base(4,8,0)
  displayException = renderCannotPruneDependencies
#endif

renderCannotPruneDependencies :: CannotPruneDependencies -> String
renderCannotPruneDependencies (CannotPruneDependencies brokenPackages) =
      "Cannot select only the dependencies (as requested by the "
   ++ "'--only-dependencies' flag), "
   ++ (case pkgids of
          [pkgid] -> "the package " ++ display pkgid ++ " is "
          _       -> "the packages "
                     ++ intercalate ", " (map display pkgids) ++ " are ")
   ++ "required by a dependency of one of the other targets."
  where
    -- throw away the details and just list the deps that are needed
    pkgids :: [PackageId]
    pkgids = nub . map packageId . concatMap snd $ brokenPackages


---------------------------
-- Setup.hs script policy
--

-- Handling for Setup.hs scripts is a bit tricky, part of it lives in the
-- solver phase, and part in the elaboration phase. We keep the helper
-- functions for both phases together here so at least you can see all of it
-- in one place.
--
-- There are four major cases for Setup.hs handling:
--
--  1. @build-type@ Custom with a @custom-setup@ section
--  2. @build-type@ Custom without a @custom-setup@ section
--  3. @build-type@ not Custom with @cabal-version >  $our-cabal-version@
--  4. @build-type@ not Custom with @cabal-version <= $our-cabal-version@
--
-- It's also worth noting that packages specifying @cabal-version: >= 1.23@
-- or later that have @build-type@ Custom will always have a @custom-setup@
-- section. Therefore in case 2, the specified @cabal-version@ will always be
-- less than 1.23.
--
-- In cases 1 and 2 we obviously have to build an external Setup.hs script,
-- while in case 4 we can use the internal library API. In case 3 we also have
-- to build an external Setup.hs script because the package needs a later
-- Cabal lib version than we can support internally.
--
-- data SetupScriptStyle = ...  -- see ProjectPlanning.Types

-- | Work out the 'SetupScriptStyle' given the package description.
--
packageSetupScriptStyle :: PD.PackageDescription -> SetupScriptStyle
packageSetupScriptStyle pkg
  | buildType == PD.Custom
  , Just setupbi <- PD.setupBuildInfo pkg -- does have a custom-setup stanza
  , not (PD.defaultSetupDepends setupbi)  -- but not one we added internally
  = SetupCustomExplicitDeps

  | buildType == PD.Custom
  , Just setupbi <- PD.setupBuildInfo pkg -- we get this case post-solver as
  , PD.defaultSetupDepends setupbi        -- the solver fills in the deps
  = SetupCustomImplicitDeps

  | buildType == PD.Custom
  , Nothing <- PD.setupBuildInfo pkg      -- we get this case pre-solver
  = SetupCustomImplicitDeps

  | PD.specVersion pkg > cabalVersion -- one cabal-install is built against
  = SetupNonCustomExternalLib

  | otherwise
  = SetupNonCustomInternalLib
  where
    buildType = fromMaybe PD.Custom (PD.buildType pkg)


-- | Part of our Setup.hs handling policy is implemented by getting the solver
-- to work out setup dependencies for packages. The solver already handles
-- packages that explicitly specify setup dependencies, but we can also tell
-- the solver to treat other packages as if they had setup dependencies.
-- That's what this function does, it gets called by the solver for all
-- packages that don't already have setup dependencies.
--
-- The dependencies we want to add is different for each 'SetupScriptStyle'.
--
-- Note that adding default deps means these deps are actually /added/ to the
-- packages that we get out of the solver in the 'SolverInstallPlan'. Making
-- implicit setup deps explicit is a problem in the post-solver stages because
-- we still need to distinguish the case of explicit and implict setup deps.
-- See 'rememberImplicitSetupDeps'.
--
-- Note in addition to adding default setup deps, we also use
-- 'addSetupCabalMinVersionConstraint' (in 'planPackages') to require
-- @Cabal >= 1.20@ for Setup scripts.
--
defaultSetupDeps :: Compiler -> Platform
                 -> PD.PackageDescription
                 -> Maybe [Dependency]
defaultSetupDeps compiler platform pkg =
    case packageSetupScriptStyle pkg of

      -- For packages with build type custom that do not specify explicit
      -- setup dependencies, we add a dependency on Cabal and a number
      -- of other packages.
      SetupCustomImplicitDeps ->
        Just $
        [ Dependency depPkgname anyVersion
        | depPkgname <- legacyCustomSetupPkgs compiler platform ] ++
        [ Dependency cabalPkgname cabalConstraint
        | packageName pkg /= cabalPkgname ]
        where
          -- The Cabal dep is slightly special:
          -- * We omit the dep for the Cabal lib itself, since it bootstraps.
          -- * We constrain it to be < 1.25
          --
          -- Note: we also add a global constraint to require Cabal >= 1.20
          -- for Setup scripts (see use addSetupCabalMinVersionConstraint).
          --
          cabalConstraint   = orLaterVersion (PD.specVersion pkg)
                                `intersectVersionRanges`
                              earlierVersion cabalCompatMaxVer
          -- The idea here is that at some point we will make significant
          -- breaking changes to the Cabal API that Setup.hs scripts use.
          -- So for old custom Setup scripts that do not specify explicit
          -- constraints, we constrain them to use a compatible Cabal version.
          cabalCompatMaxVer = mkVersion [1,25]

      -- For other build types (like Simple) if we still need to compile an
      -- external Setup.hs, it'll be one of the simple ones that only depends
      -- on Cabal and base.
      SetupNonCustomExternalLib ->
        Just [ Dependency cabalPkgname cabalConstraint
             , Dependency basePkgname  anyVersion ]
        where
          cabalConstraint = orLaterVersion (PD.specVersion pkg)

      -- The internal setup wrapper method has no deps at all.
      SetupNonCustomInternalLib -> Just []

      -- This case gets ruled out by the caller, planPackages, see the note
      -- above in the SetupCustomImplicitDeps case.
      SetupCustomExplicitDeps ->
        error $ "defaultSetupDeps: called for a package with explicit "
             ++ "setup deps: " ++ display (packageId pkg)


-- | Work out which version of the Cabal spec we will be using to talk to the
-- Setup.hs interface for this package.
--
-- This depends somewhat on the 'SetupScriptStyle' but most cases are a result
-- of what the solver picked for us, based on the explicit setup deps or the
-- ones added implicitly by 'defaultSetupDeps'.
--
packageSetupScriptSpecVersion :: Package pkg
                              => SetupScriptStyle
                              -> PD.PackageDescription
                              -> ComponentDeps [pkg]
                              -> Version

-- We're going to be using the internal Cabal library, so the spec version of
-- that is simply the version of the Cabal library that cabal-install has been
-- built with.
packageSetupScriptSpecVersion SetupNonCustomInternalLib _ _ =
    cabalVersion

-- If we happen to be building the Cabal lib itself then because that
-- bootstraps itself then we use the version of the lib we're building.
packageSetupScriptSpecVersion SetupCustomImplicitDeps pkg _
  | packageName pkg == cabalPkgname
  = packageVersion pkg

-- In all other cases we have a look at what version of the Cabal lib the
-- solver picked. Or if it didn't depend on Cabal at all (which is very rare)
-- then we look at the .cabal file to see what spec version it declares.
packageSetupScriptSpecVersion _ pkg deps =
    case find ((cabalPkgname ==) . packageName) (CD.setupDeps deps) of
      Just dep -> packageVersion dep
      Nothing  -> PD.specVersion pkg


cabalPkgname, basePkgname :: PackageName
cabalPkgname = mkPackageName "Cabal"
basePkgname  = mkPackageName "base"


legacyCustomSetupPkgs :: Compiler -> Platform -> [PackageName]
legacyCustomSetupPkgs compiler (Platform _ os) =
    map mkPackageName $
        [ "array", "base", "binary", "bytestring", "containers"
        , "deepseq", "directory", "filepath", "old-time", "pretty"
        , "process", "time", "transformers" ]
     ++ [ "Win32" | os == Windows ]
     ++ [ "unix"  | os /= Windows ]
     ++ [ "ghc-prim"         | isGHC ]
     ++ [ "template-haskell" | isGHC ]
  where
    isGHC = compilerCompatFlavor GHC compiler

-- The other aspects of our Setup.hs policy lives here where we decide on
-- the 'SetupScriptOptions'.
--
-- Our current policy for the 'SetupCustomImplicitDeps' case is that we
-- try to make the implicit deps cover everything, and we don't allow the
-- compiler to pick up other deps. This may or may not be sustainable, and
-- we might have to allow the deps to be non-exclusive, but that itself would
-- be tricky since we would have to allow the Setup access to all the packages
-- in the store and local dbs.

setupHsScriptOptions :: ElaboratedReadyPackage
                     -> ElaboratedSharedConfig
                     -> FilePath
                     -> FilePath
                     -> Bool
                     -> Lock
                     -> SetupScriptOptions
-- TODO: Fix this so custom is a separate component.  Custom can ALWAYS
-- be a separate component!!!
setupHsScriptOptions (ReadyPackage elab@ElaboratedConfiguredPackage{..})
                     ElaboratedSharedConfig{..} srcdir builddir
                     isParallelBuild cacheLock =
    SetupScriptOptions {
      useCabalVersion          = thisVersion elabSetupScriptCliVersion,
      useCabalSpecVersion      = Just elabSetupScriptCliVersion,
      useCompiler              = Just pkgConfigCompiler,
      usePlatform              = Just pkgConfigPlatform,
      usePackageDB             = elabSetupPackageDBStack,
      usePackageIndex          = Nothing,
      useDependencies          = [ (uid, srcid)
                                 | ConfiguredId srcid uid
                                 <- elabSetupDependencies elab ],
      useDependenciesExclusive = True,
      useVersionMacros         = elabSetupScriptStyle == SetupCustomExplicitDeps,
      useProgramDb             = pkgConfigCompilerProgs,
      useDistPref              = builddir,
      useLoggingHandle         = Nothing, -- this gets set later
      useWorkingDir            = Just srcdir,
      useExtraPathEnv          = elabExeDependencyPaths elab,
      useWin32CleanHack        = False,   --TODO: [required eventually]
      forceExternalSetupMethod = isParallelBuild,
      setupCacheLock           = Just cacheLock,
      isInteractive            = False
    }


-- | To be used for the input for elaborateInstallPlan.
--
-- TODO: [code cleanup] make InstallDirs.defaultInstallDirs pure.
--
userInstallDirTemplates :: Compiler
                        -> IO InstallDirs.InstallDirTemplates
userInstallDirTemplates compiler = do
    InstallDirs.defaultInstallDirs
                  (compilerFlavor compiler)
                  True  -- user install
                  False -- unused

storePackageInstallDirs :: CabalDirLayout
                        -> CompilerId
                        -> InstalledPackageId
                        -> InstallDirs.InstallDirs FilePath
storePackageInstallDirs CabalDirLayout{cabalStorePackageDirectory}
                        compid ipkgid =
    InstallDirs.InstallDirs {..}
  where
    prefix       = cabalStorePackageDirectory compid ipkgid
    bindir       = prefix </> "bin"
    libdir       = prefix </> "lib"
    libsubdir    = ""
    dynlibdir    = libdir
    flibdir      = libdir
    libexecdir   = prefix </> "libexec"
    includedir   = libdir </> "include"
    datadir      = prefix </> "share"
    datasubdir   = ""
    docdir       = datadir </> "doc"
    mandir       = datadir </> "man"
    htmldir      = docdir  </> "html"
    haddockdir   = htmldir
    sysconfdir   = prefix </> "etc"


--TODO: [code cleanup] perhaps reorder this code
-- based on the ElaboratedInstallPlan + ElaboratedSharedConfig,
-- make the various Setup.hs {configure,build,copy} flags


setupHsConfigureFlags :: ElaboratedReadyPackage
                      -> ElaboratedSharedConfig
                      -> Verbosity
                      -> FilePath
                      -> Cabal.ConfigFlags
setupHsConfigureFlags (ReadyPackage elab@ElaboratedConfiguredPackage{..})
                      sharedConfig@ElaboratedSharedConfig{..}
                      verbosity builddir =
    sanityCheckElaboratedConfiguredPackage sharedConfig elab
        (Cabal.ConfigFlags {..})
  where
    configArgs                = mempty -- unused, passed via args
    configDistPref            = toFlag builddir
    configCabalFilePath       = mempty
    configVerbosity           = toFlag verbosity

    configInstantiateWith     = Map.toList elabInstantiatedWith

    configDeterministic       = mempty -- doesn't matter, configIPID/configCID overridese
    configIPID                = case elabPkgOrComp of
                                  ElabPackage pkg -> toFlag (display (pkgInstalledId pkg))
                                  ElabComponent _ -> mempty
    configCID                 = case elabPkgOrComp of
                                  ElabPackage _ -> mempty
                                  ElabComponent _ -> toFlag elabComponentId

    configProgramPaths        = Map.toList elabProgramPaths
    configProgramArgs         = Map.toList elabProgramArgs
    configProgramPathExtra    = toNubList elabProgramPathExtra
    configHcFlavor            = toFlag (compilerFlavor pkgConfigCompiler)
    configHcPath              = mempty -- we use configProgramPaths instead
    configHcPkg               = mempty -- we use configProgramPaths instead

    configVanillaLib          = toFlag elabVanillaLib
    configSharedLib           = toFlag elabSharedLib
    configDynExe              = toFlag elabDynExe
    configGHCiLib             = toFlag elabGHCiLib
    configProfExe             = mempty
    configProfLib             = toFlag elabProfLib
    configProf                = toFlag elabProfExe

    -- configProfDetail is for exe+lib, but overridden by configProfLibDetail
    -- so we specify both so we can specify independently
    configProfDetail          = toFlag elabProfExeDetail
    configProfLibDetail       = toFlag elabProfLibDetail

    configCoverage            = toFlag elabCoverage
    configLibCoverage         = mempty

    configOptimization        = toFlag elabOptimization
    configSplitObjs           = toFlag elabSplitObjs
    configStripExes           = toFlag elabStripExes
    configStripLibs           = toFlag elabStripLibs
    configDebugInfo           = toFlag elabDebugInfo
    configAllowOlder          = mempty -- we use configExactConfiguration True
    configAllowNewer          = mempty -- we use configExactConfiguration True

    configConfigurationsFlags = elabFlagAssignment
    configConfigureArgs       = elabConfigureScriptArgs
    configExtraLibDirs        = elabExtraLibDirs
    configExtraFrameworkDirs  = elabExtraFrameworkDirs
    configExtraIncludeDirs    = elabExtraIncludeDirs
    configProgPrefix          = maybe mempty toFlag elabProgPrefix
    configProgSuffix          = maybe mempty toFlag elabProgSuffix

    configInstallDirs         = fmap (toFlag . InstallDirs.toPathTemplate)
                                     elabInstallDirs

    -- we only use configDependencies, unless we're talking to an old Cabal
    -- in which case we use configConstraints
    -- NB: This does NOT use InstallPlan.depends, which includes executable
    -- dependencies which should NOT be fed in here (also you don't have
    -- enough info anyway)
    configDependencies        = [ (packageName srcid, cid)
                                | ConfiguredId srcid cid <- elabLibDependencies elab ]
    configConstraints         =
        case elabPkgOrComp of
            ElabPackage _ ->
                [ thisPackageVersion srcid
                | ConfiguredId srcid _uid <- elabLibDependencies elab ]
            ElabComponent _ -> []


    -- explicitly clear, then our package db stack
    -- TODO: [required eventually] have to do this differently for older Cabal versions
    configPackageDBs          = Nothing : map Just elabBuildPackageDBStack

    configTests               = case elabPkgOrComp of
                                    ElabPackage pkg -> toFlag (TestStanzas  `Set.member` pkgStanzasEnabled pkg)
                                    ElabComponent _ -> mempty
    configBenchmarks          = case elabPkgOrComp of
                                    ElabPackage pkg -> toFlag (BenchStanzas `Set.member` pkgStanzasEnabled pkg)
                                    ElabComponent _ -> mempty

    configExactConfiguration  = toFlag True
    configFlagError           = mempty --TODO: [research required] appears not to be implemented
    configRelocatable         = mempty --TODO: [research required] ???
    configScratchDir          = mempty -- never use
    configUserInstall         = mempty -- don't rely on defaults
    configPrograms_           = mempty -- never use, shouldn't exist


setupHsConfigureArgs :: ElaboratedConfiguredPackage
                     -> [String]
setupHsConfigureArgs (ElaboratedConfiguredPackage { elabPkgOrComp = ElabPackage _ }) = []
setupHsConfigureArgs elab@(ElaboratedConfiguredPackage { elabPkgOrComp = ElabComponent comp }) =
    [showComponentTarget (packageId elab) (ComponentTarget cname WholeComponent)]
  where
    cname = fromMaybe (error "setupHsConfigureArgs: trying to configure setup")
                      (compComponentName comp)

setupHsBuildFlags :: ElaboratedConfiguredPackage
                  -> ElaboratedSharedConfig
                  -> Verbosity
                  -> FilePath
                  -> Cabal.BuildFlags
setupHsBuildFlags _ _ verbosity builddir =
    Cabal.BuildFlags {
      buildProgramPaths = mempty, --unused, set at configure time
      buildProgramArgs  = mempty, --unused, set at configure time
      buildVerbosity    = toFlag verbosity,
      buildDistPref     = toFlag builddir,
      buildNumJobs      = mempty, --TODO: [nice to have] sometimes want to use toFlag (Just numBuildJobs),
      buildArgs         = mempty  -- unused, passed via args not flags
    }


setupHsBuildArgs :: ElaboratedConfiguredPackage -> [String]
setupHsBuildArgs elab@(ElaboratedConfiguredPackage { elabPkgOrComp = ElabPackage _ })
    -- Fix for #3335, don't pass build arguments if it's not supported
    | elabSetupScriptCliVersion elab >= mkVersion [1,17]
    = map (showComponentTarget (packageId elab)) (elabBuildTargets elab)
    | otherwise
    = []
setupHsBuildArgs (ElaboratedConfiguredPackage { elabPkgOrComp = ElabComponent _ })
    = []


setupHsTestFlags :: ElaboratedConfiguredPackage
                 -> ElaboratedSharedConfig
                 -> Verbosity
                 -> FilePath
                 -> Cabal.TestFlags 
setupHsTestFlags _ _ verbosity builddir = Cabal.TestFlags
    { testDistPref    = toFlag builddir
    , testVerbosity   = toFlag verbosity
    , testMachineLog  = mempty
    , testHumanLog    = mempty
    , testShowDetails = toFlag Cabal.Always
    , testKeepTix     = mempty
    , testOptions     = mempty
    }

setupHsTestArgs :: ElaboratedConfiguredPackage -> [String]
-- TODO: Does the issue #3335 affects test as well
setupHsTestArgs elab =
    mapMaybe (showTestComponentTarget (packageId elab)) (elabTestTargets elab)

setupHsReplFlags :: ElaboratedConfiguredPackage
                 -> ElaboratedSharedConfig
                 -> Verbosity
                 -> FilePath
                 -> Cabal.ReplFlags
setupHsReplFlags _ _ verbosity builddir =
    Cabal.ReplFlags {
      replProgramPaths = mempty, --unused, set at configure time
      replProgramArgs  = mempty, --unused, set at configure time
      replVerbosity    = toFlag verbosity,
      replDistPref     = toFlag builddir,
      replReload       = mempty  --only used as callback from repl
    }


setupHsReplArgs :: ElaboratedConfiguredPackage -> [String]
setupHsReplArgs elab =
    maybe [] (\t -> [showComponentTarget (packageId elab) t]) (elabReplTarget elab)
    --TODO: should be able to give multiple modules in one component


setupHsCopyFlags :: ElaboratedConfiguredPackage
                 -> ElaboratedSharedConfig
                 -> Verbosity
                 -> FilePath
                 -> Cabal.CopyFlags
setupHsCopyFlags _ _ verbosity builddir =
    Cabal.CopyFlags {
      --TODO: [nice to have] we currently just rely on Setup.hs copy to always do the right
      -- thing, but perhaps we ought really to copy into an image dir and do
      -- some sanity checks and move into the final location ourselves
      copyArgs      = [], -- TODO: could use this to only copy what we enabled
      copyDest      = toFlag InstallDirs.NoCopyDest,
      copyDistPref  = toFlag builddir,
      copyVerbosity = toFlag verbosity
    }

setupHsRegisterFlags :: ElaboratedConfiguredPackage
                     -> ElaboratedSharedConfig
                     -> Verbosity
                     -> FilePath
                     -> FilePath
                     -> Cabal.RegisterFlags
setupHsRegisterFlags ElaboratedConfiguredPackage{..} _
                     verbosity builddir pkgConfFile =
    Cabal.RegisterFlags {
      regPackageDB   = mempty,  -- misfeature
      regGenScript   = mempty,  -- never use
      regGenPkgConf  = toFlag (Just pkgConfFile),
      regInPlace     = case elabBuildStyle of
                         BuildInplaceOnly -> toFlag True
                         _                -> toFlag False,
      regPrintId     = mempty,  -- never use
      regDistPref    = toFlag builddir,
      regArgs        = [],
      regVerbosity   = toFlag verbosity
    }

setupHsHaddockFlags :: ElaboratedConfiguredPackage
                    -> ElaboratedSharedConfig
                    -> Verbosity
                    -> FilePath
                    -> Cabal.HaddockFlags
-- TODO: reconsider whether or not Executables/TestSuites/...
-- needed for component
setupHsHaddockFlags (ElaboratedConfiguredPackage{..}) _ verbosity builddir =
    Cabal.HaddockFlags {
      haddockProgramPaths  = mempty, --unused, set at configure time
      haddockProgramArgs   = mempty, --unused, set at configure time
      haddockHoogle        = toFlag elabHaddockHoogle,
      haddockHtml          = toFlag elabHaddockHtml,
      haddockHtmlLocation  = maybe mempty toFlag elabHaddockHtmlLocation,
      haddockForHackage    = mempty, --TODO: new flag
      haddockForeignLibs   = toFlag elabHaddockForeignLibs,
      haddockExecutables   = toFlag elabHaddockExecutables,
      haddockTestSuites    = toFlag elabHaddockTestSuites,
      haddockBenchmarks    = toFlag elabHaddockBenchmarks,
      haddockInternal      = toFlag elabHaddockInternal,
      haddockCss           = maybe mempty toFlag elabHaddockCss,
      haddockHscolour      = toFlag elabHaddockHscolour,
      haddockHscolourCss   = maybe mempty toFlag elabHaddockHscolourCss,
      haddockContents      = maybe mempty toFlag elabHaddockContents,
      haddockDistPref      = toFlag builddir,
      haddockKeepTempFiles = mempty, --TODO: from build settings
      haddockVerbosity     = toFlag verbosity
    }

{-
setupHsTestFlags :: ElaboratedConfiguredPackage
                 -> ElaboratedSharedConfig
                 -> Verbosity
                 -> FilePath
                 -> Cabal.TestFlags
setupHsTestFlags _ _ verbosity builddir =
    Cabal.TestFlags {
    }
-}

------------------------------------------------------------------------------
-- * Sharing installed packages
------------------------------------------------------------------------------

--
-- Nix style store management for tarball packages
--
-- So here's our strategy:
--
-- We use a per-user nix-style hashed store, but /only/ for tarball packages.
-- So that includes packages from hackage repos (and other http and local
-- tarballs). For packages in local directories we do not register them into
-- the shared store by default, we just build them locally inplace.
--
-- The reason we do it like this is that it's easy to make stable hashes for
-- tarball packages, and these packages benefit most from sharing. By contrast
-- unpacked dir packages are harder to hash and they tend to change more
-- frequently so there's less benefit to sharing them.
--
-- When using the nix store approach we have to run the solver *without*
-- looking at the packages installed in the store, just at the source packages
-- (plus core\/global installed packages). Then we do a post-processing pass
-- to replace configured packages in the plan with pre-existing ones, where
-- possible. Where possible of course means where the nix-style package hash
-- equals one that's already in the store.
--
-- One extra wrinkle is that unless we know package tarball hashes upfront, we
-- will have to download the tarballs to find their hashes. So we have two
-- options: delay replacing source with pre-existing installed packages until
-- the point during the execution of the install plan where we have the
-- tarball, or try to do as much up-front as possible and then check again
-- during plan execution. The former isn't great because we would end up
-- telling users we're going to re-install loads of packages when in fact we
-- would just share them. It'd be better to give as accurate a prediction as
-- we can. The latter is better for users, but we do still have to check
-- during plan execution because it's important that we don't replace existing
-- installed packages even if they have the same package hash, because we
-- don't guarantee ABI stability.

-- TODO: [required eventually] for safety of concurrent installs, we must make sure we register but
-- not replace installed packages with ghc-pkg.

packageHashInputs :: ElaboratedSharedConfig
                  -> ElaboratedConfiguredPackage
                  -> PackageHashInputs
packageHashInputs
    pkgshared
    elab@(ElaboratedConfiguredPackage {
      elabPkgSourceHash = Just srchash
    }) =
    PackageHashInputs {
      pkgHashPkgId       = packageId elab,
      pkgHashComponent   =
        case elabPkgOrComp elab of
          ElabPackage _ -> Nothing
          ElabComponent comp -> Just (compSolverName comp),
      pkgHashSourceHash  = srchash,
      pkgHashPkgConfigDeps = Set.fromList (elabPkgConfigDependencies elab),
      pkgHashDirectDeps  =
        case elabPkgOrComp elab of
          ElabPackage (ElaboratedPackage{..}) ->
            Set.fromList $
             [ confInstId dep
             | dep <- CD.select relevantDeps pkgLibDependencies ] ++
             [ confInstId dep
             | dep <- CD.select relevantDeps pkgExeDependencies ]
          ElabComponent comp ->
            Set.fromList (map confInstId (compLibDependencies comp)
                                       ++ compExeDependencies comp),
      pkgHashOtherConfig = packageHashConfigInputs pkgshared elab
    }
  where
    -- Obviously the main deps are relevant
    relevantDeps CD.ComponentLib       = True
    relevantDeps (CD.ComponentSubLib _) = True
    relevantDeps (CD.ComponentFLib _)   = True
    relevantDeps (CD.ComponentExe _)   = True
    -- Setup deps can affect the Setup.hs behaviour and thus what is built
    relevantDeps  CD.ComponentSetup    = True
    -- However testsuites and benchmarks do not get installed and should not
    -- affect the result, so we do not include them.
    relevantDeps (CD.ComponentTest  _) = False
    relevantDeps (CD.ComponentBench _) = False

packageHashInputs _ pkg =
    error $ "packageHashInputs: only for packages with source hashes. "
         ++ display (packageId pkg)

packageHashConfigInputs :: ElaboratedSharedConfig
                        -> ElaboratedConfiguredPackage
                        -> PackageHashConfigInputs
packageHashConfigInputs
    ElaboratedSharedConfig{..}
    ElaboratedConfiguredPackage{..} =

    PackageHashConfigInputs {
      pkgHashCompilerId          = compilerId pkgConfigCompiler,
      pkgHashPlatform            = pkgConfigPlatform,
      pkgHashFlagAssignment      = elabFlagAssignment,
      pkgHashConfigureScriptArgs = elabConfigureScriptArgs,
      pkgHashVanillaLib          = elabVanillaLib,
      pkgHashSharedLib           = elabSharedLib,
      pkgHashDynExe              = elabDynExe,
      pkgHashGHCiLib             = elabGHCiLib,
      pkgHashProfLib             = elabProfLib,
      pkgHashProfExe             = elabProfExe,
      pkgHashProfLibDetail       = elabProfLibDetail,
      pkgHashProfExeDetail       = elabProfExeDetail,
      pkgHashCoverage            = elabCoverage,
      pkgHashOptimization        = elabOptimization,
      pkgHashSplitObjs           = elabSplitObjs,
      pkgHashStripLibs           = elabStripLibs,
      pkgHashStripExes           = elabStripExes,
      pkgHashDebugInfo           = elabDebugInfo,
      pkgHashProgramArgs         = elabProgramArgs,
      pkgHashExtraLibDirs        = elabExtraLibDirs,
      pkgHashExtraFrameworkDirs  = elabExtraFrameworkDirs,
      pkgHashExtraIncludeDirs    = elabExtraIncludeDirs,
      pkgHashProgPrefix          = elabProgPrefix,
      pkgHashProgSuffix          = elabProgSuffix
    }


-- | Given the 'InstalledPackageIndex' for a nix-style package store, and an
-- 'ElaboratedInstallPlan', replace configured source packages by installed
-- packages from the store whenever they exist.
--
improveInstallPlanWithInstalledPackages :: Set UnitId
                                        -> ElaboratedInstallPlan
                                        -> ElaboratedInstallPlan
improveInstallPlanWithInstalledPackages installedPkgIdSet =
    InstallPlan.installed canPackageBeImproved
  where
    canPackageBeImproved pkg =
      installedUnitId pkg `Set.member` installedPkgIdSet
    --TODO: sanity checks:
    -- * the installed package must have the expected deps etc
    -- * the installed package must not be broken, valid dep closure

    --TODO: decide what to do if we encounter broken installed packages,
    -- since overwriting is never safe.
