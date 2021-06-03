{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

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
    rebuildProjectConfig,
    rebuildInstallPlan,

    -- * Build targets
    availableTargets,
    AvailableTarget(..),
    AvailableTargetStatus(..),
    TargetRequested(..),
    ComponentTarget(..),
    SubComponentTarget(..),
    showComponentTarget,
    nubComponentTargets,

    -- * Selecting a plan subset
    pruneInstallPlanToTargets,
    TargetAction(..),
    pruneInstallPlanToDependencies,
    CannotPruneDependencies(..),

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
    setupHsBenchFlags,
    setupHsBenchArgs,
    setupHsCopyFlags,
    setupHsRegisterFlags,
    setupHsHaddockFlags,
    setupHsHaddockArgs,

    packageHashInputs,

    -- * Path construction
    binDirectoryFor,
    binDirectories,
    storePackageInstallDirs,
    storePackageInstallDirs'
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import           Distribution.Client.HashValue
import           Distribution.Client.ProjectPlanning.Types as Ty
import           Distribution.Client.PackageHash
import           Distribution.Client.RebuildMonad
import           Distribution.Client.Store
import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanOutput

import           Distribution.Client.Types
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import           Distribution.Client.Dependency
import           Distribution.Client.Dependency.Types
import qualified Distribution.Client.IndexUtils as IndexUtils
import           Distribution.Client.Utils (incVersion)
import           Distribution.Client.Targets (userToPackageConstraint)
import           Distribution.Client.DistDirLayout
import           Distribution.Client.SetupWrapper
import           Distribution.Client.JobControl
import           Distribution.Client.FetchUtils
import           Distribution.Client.Config
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

import           Distribution.CabalSpecVersion
import           Distribution.ModuleName
import           Distribution.Package
import           Distribution.Types.AnnotatedId
import           Distribution.Types.ComponentName
import           Distribution.Types.LibraryName
import           Distribution.Types.GivenComponent
  (GivenComponent(..))
import           Distribution.Types.PackageVersionConstraint
import           Distribution.Types.PkgconfigDependency
import           Distribution.Types.UnqualComponentName
import           Distribution.System
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import           Distribution.Simple.PackageIndex (InstalledPackageIndex)
import           Distribution.Simple.Compiler
import qualified Distribution.Simple.GHC   as GHC   --TODO: [code cleanup] eliminate
import qualified Distribution.Simple.GHCJS as GHCJS --TODO: [code cleanup] eliminate
import           Distribution.Simple.Program
import           Distribution.Simple.Program.Db
import           Distribution.Simple.Program.Find
import qualified Distribution.Simple.Setup as Cabal
import           Distribution.Simple.Setup
  (Flag(..), toFlag, flagToMaybe, flagToList, fromFlagOrDefault)
import qualified Distribution.Simple.Configure as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import           Distribution.Simple.LocalBuildInfo
                   ( Component(..), pkgComponents, componentBuildInfo
                   , componentName )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.InstalledPackageInfo as IPI

import           Distribution.Backpack.ConfiguredComponent
import           Distribution.Backpack.LinkedComponent
import           Distribution.Backpack.ComponentsGraph
import           Distribution.Backpack.ModuleShape
import           Distribution.Backpack.FullUnitId
import           Distribution.Backpack
import           Distribution.Types.ComponentInclude

import           Distribution.Simple.Utils
import           Distribution.Version

import qualified Distribution.Compat.Graph as Graph
import           Distribution.Compat.Graph(IsNode(..))

import           Data.Foldable (fold)
import           Text.PrettyPrint (text, hang, quotes, colon, vcat, ($$), fsep, punctuate, comma)
import qualified Text.PrettyPrint as Disp
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Control.Monad.State as State
import           Control.Exception (assert)
import           Data.List (groupBy, deleteBy)
import qualified Data.List.NonEmpty as NE
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
  . assert (optStanzaSetNull $
        optStanzaKeysFilteredByValue (maybe False not) elabStanzasRequested `optStanzaSetIntersection` elabStanzasAvailable)

    -- either a package is built inplace, or we are not attempting to
    -- build any test suites or benchmarks (we never build these
    -- for remote packages!)
  . assert (elabBuildStyle == BuildInplaceOnly ||
     optStanzaSetNull elabStanzasAvailable)

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
        Just (CLibName _)    -> True
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
    assert (pkgStanzasEnabled `optStanzaSetIsSubset` elabStanzasAvailable)

    -- the stanzas that the user explicitly requested should be
    -- enabled (by the previous test, they are also available)
  . assert (optStanzaKeysFilteredByValue (fromMaybe False) elabStanzasRequested
                `optStanzaSetIsSubset` pkgStanzasEnabled)

------------------------------------------------------------------------------
-- * Deciding what to do: making an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------

-- | Return the up-to-date project config and information about the local
-- packages within the project.
--
rebuildProjectConfig :: Verbosity
                     -> DistDirLayout
                     -> ProjectConfig
                     -> IO ( ProjectConfig
                           , [PackageSpecifier UnresolvedSourcePackage] )
rebuildProjectConfig verbosity
                     distDirLayout@DistDirLayout {
                       distProjectRootDirectory,
                       distDirectory,
                       distProjectCacheFile,
                       distProjectCacheDirectory,
                       distProjectFile
                     }
                     cliConfig = do

    fileMonitorProjectConfigKey <- do
      configPath <- getConfigFilePath projectConfigConfigFile
      return (configPath, distProjectFile "")

    (projectConfig, localPackages) <-
      runRebuild distProjectRootDirectory
      $ rerunIfChanged verbosity
                       fileMonitorProjectConfig
                       fileMonitorProjectConfigKey
      $ do
          liftIO $ info verbosity "Project settings changed, reconfiguring..."
          projectConfig <- phaseReadProjectConfig
          localPackages <- phaseReadLocalPackages projectConfig
          return (projectConfig, localPackages)

    info verbosity
      $ unlines
      $ ("this build was affected by the following (project) config files:" :)
      $ [ "- " ++ path
        | Explicit path <- Set.toList $ projectConfigProvenance projectConfig
        ]

    return (projectConfig <> cliConfig, localPackages)

  where

    ProjectConfigShared { projectConfigConfigFile } =
      projectConfigShared cliConfig

    fileMonitorProjectConfig =
      newFileMonitor (distProjectCacheFile "config") :: FileMonitor
          (FilePath, FilePath)
          (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage])

    -- Read the cabal.project (or implicit config) and combine it with
    -- arguments from the command line
    --
    phaseReadProjectConfig :: Rebuild ProjectConfig
    phaseReadProjectConfig = do
      readProjectConfig verbosity projectConfigConfigFile distDirLayout

    -- Look for all the cabal packages in the project
    -- some of which may be local src dirs, tarballs etc
    --
    phaseReadLocalPackages :: ProjectConfig
                           -> Rebuild [PackageSpecifier UnresolvedSourcePackage]
    phaseReadLocalPackages projectConfig@ProjectConfig {
                               projectConfigShared,
                               projectConfigBuildOnly
                             } = do
      pkgLocations <- findProjectPackages distDirLayout projectConfig

      -- Create folder only if findProjectPackages did not throw a
      -- BadPackageLocations exception.
      liftIO $ do
        createDirectoryIfMissingVerbose verbosity True distDirectory
        createDirectoryIfMissingVerbose verbosity True distProjectCacheDirectory

      fetchAndReadSourcePackages verbosity distDirLayout
                                 projectConfigShared
                                 projectConfigBuildOnly
                                 pkgLocations


-- | Return an up-to-date elaborated install plan.
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
                   -> DistDirLayout -> CabalDirLayout
                   -> ProjectConfig
                   -> [PackageSpecifier UnresolvedSourcePackage]
                   -> IO ( ElaboratedInstallPlan  -- with store packages
                         , ElaboratedInstallPlan  -- with source packages
                         , ElaboratedSharedConfig
                         , IndexUtils.TotalIndexState
                         , IndexUtils.ActiveRepos
                         )
                      -- ^ @(improvedPlan, elaboratedPlan, _, _, _)@
rebuildInstallPlan verbosity
                   distDirLayout@DistDirLayout {
                     distProjectRootDirectory,
                     distProjectCacheFile
                   }
                   CabalDirLayout {
                     cabalStoreDirLayout
                   } = \projectConfig localPackages ->
    runRebuild distProjectRootDirectory $ do
    progsearchpath <- liftIO $ getSystemSearchPath
    let projectConfigMonitored = projectConfig { projectConfigBuildOnly = mempty }

    -- The overall improved plan is cached
    rerunIfChanged verbosity fileMonitorImprovedPlan
                   -- react to changes in the project config,
                   -- the package .cabal files and the path
                   (projectConfigMonitored, localPackages, progsearchpath) $ do

      -- And so is the elaborated plan that the improved plan based on
      (elaboratedPlan, elaboratedShared, totalIndexState, activeRepos) <-
        rerunIfChanged verbosity fileMonitorElaboratedPlan
                       (projectConfigMonitored, localPackages,
                        progsearchpath) $ do

          compilerEtc   <- phaseConfigureCompiler projectConfig
          _             <- phaseConfigurePrograms projectConfig compilerEtc
          (solverPlan, pkgConfigDB, totalIndexState, activeRepos)
                        <- phaseRunSolver         projectConfig
                                                  compilerEtc
                                                  localPackages
          (elaboratedPlan,
           elaboratedShared) <- phaseElaboratePlan projectConfig
                                                   compilerEtc pkgConfigDB
                                                   solverPlan
                                                   localPackages

          phaseMaintainPlanOutputs elaboratedPlan elaboratedShared
          return (elaboratedPlan, elaboratedShared, totalIndexState, activeRepos)

      -- The improved plan changes each time we install something, whereas
      -- the underlying elaborated plan only changes when input config
      -- changes, so it's worth caching them separately.
      improvedPlan <- phaseImprovePlan elaboratedPlan elaboratedShared

      return (improvedPlan, elaboratedPlan, elaboratedShared, totalIndexState, activeRepos)

  where
    fileMonitorCompiler       = newFileMonitorInCacheDir "compiler"
    fileMonitorSolverPlan     = newFileMonitorInCacheDir "solver-plan"
    fileMonitorSourceHashes   = newFileMonitorInCacheDir "source-hashes"
    fileMonitorElaboratedPlan = newFileMonitorInCacheDir "elaborated-plan"
    fileMonitorImprovedPlan   = newFileMonitorInCacheDir "improved-plan"

    newFileMonitorInCacheDir :: Eq a => FilePath -> FileMonitor a b
    newFileMonitorInCacheDir  = newFileMonitor . distProjectCacheFile


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
    phaseRunSolver
        :: ProjectConfig
        -> (Compiler, Platform, ProgramDb)
        -> [PackageSpecifier UnresolvedSourcePackage]
        -> Rebuild (SolverInstallPlan, PkgConfigDb, IndexUtils.TotalIndexState, IndexUtils.ActiveRepos)
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
          (sourcePkgDb, tis, ar) <- getSourcePackages verbosity withRepoCtx
              (solverSettingIndexState solverSettings)
              (solverSettingActiveRepos solverSettings)
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
            plan <- foldProgress logMsg (die' verbosity) return $
              planPackages verbosity compiler platform solver solverSettings
                           installedPkgIndex sourcePkgDb pkgConfigDB
                           localPackages localPackagesEnabledStanzas
            return (plan, pkgConfigDB, tis, ar)
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
              -- TODO: misnormer: we should separate
              -- builtin/global/inplace/local packages
              -- and packages explicitly mentioned in the project
              --
            , let pkgname            = pkgSpecifierTarget pkg
                  testsEnabled       = lookupLocalPackageConfig
                                         packageConfigTests
                                         projectConfig pkgname
                  benchmarksEnabled  = lookupLocalPackageConfig
                                         packageConfigBenchmarks
                                         projectConfig pkgname
                  isLocal = isJust (shouldBeLocal pkg)
                  stanzas
                    | isLocal = Map.fromList $
                      [ (TestStanzas, enabled)
                      | enabled <- flagToList testsEnabled ] ++
                      [ (BenchStanzas , enabled)
                      | enabled <- flagToList benchmarksEnabled ]
                    | otherwise = Map.fromList [(TestStanzas, False), (BenchStanzas, False) ]
            ]

    -- Elaborate the solver's install plan to get a fully detailed plan. This
    -- version of the plan has the final nix-style hashed ids.
    --
    phaseElaboratePlan :: ProjectConfig
                       -> (Compiler, Platform, ProgramDb)
                       -> PkgConfigDb
                       -> SolverInstallPlan
                       -> [PackageSpecifier (SourcePackage (PackageLocation loc))]
                       -> Rebuild ( ElaboratedInstallPlan
                                  , ElaboratedSharedConfig )
    phaseElaboratePlan ProjectConfig {
                         projectConfigShared,
                         projectConfigAllPackages,
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
                cabalStoreDirLayout
                solverPlan
                localPackages
                sourcePackageHashes
                defaultInstallDirs
                projectConfigShared
                projectConfigAllPackages
                projectConfigLocalPackages
                (getMapMappend projectConfigSpecificPackage)
        let instantiatedPlan
              = instantiateInstallPlan
                  cabalStoreDirLayout
                  defaultInstallDirs
                  elaboratedShared
                  elaboratedPlan
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
        storePkgIdSet <- getStoreEntries cabalStoreDirLayout compid
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
        compid = compilerId (pkgConfigCompiler elaboratedShared)


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

getSourcePackages
    :: Verbosity
    -> (forall a. (RepoContext -> IO a) -> IO a)
    -> Maybe IndexUtils.TotalIndexState
    -> Maybe IndexUtils.ActiveRepos
    -> Rebuild (SourcePackageDb, IndexUtils.TotalIndexState, IndexUtils.ActiveRepos)
getSourcePackages verbosity withRepoCtx idxState activeRepos = do
    (sourcePkgDbWithTIS, repos) <-
      liftIO $
        withRepoCtx $ \repoctx -> do
          sourcePkgDbWithTIS <- IndexUtils.getSourcePackagesAtIndexState verbosity repoctx idxState activeRepos
          return (sourcePkgDbWithTIS, repoContextRepos repoctx)

    traverse_ needIfExists
        . IndexUtils.getSourcePackagesMonitorFiles
        $ repos
    return sourcePkgDbWithTIS


getPkgConfigDb :: Verbosity -> ProgramDb -> Rebuild PkgConfigDb
getPkgConfigDb verbosity progdb = do
    dirs <- liftIO $ getPkgConfigDbDirs verbosity progdb
    -- Just monitor the dirs so we'll notice new .pc files.
    -- Alternatively we could monitor all the .pc files too.
    traverse_ monitorDirectoryStatus dirs
    liftIO $ readPkgConfigDb verbosity progdb


-- | Select the config values to monitor for changes package source hashes.
packageLocationsSignature :: SolverInstallPlan
                          -> [(PackageId, PackageLocation (Maybe FilePath))]
packageLocationsSignature solverPlan =
    [ (packageId pkg, srcpkgSource pkg)
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
          [ (packageId pkg, srcpkgSource pkg)
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
        remoteTarballPkgs =
          [ (pkgid, tarball)
          | (pkgid, RemoteTarballPackage _ (Just tarball)) <- allPkgLocations ]

        -- tarballs from source-repository-package stanzas
        sourceRepoTarballPkgs =
          [ (pkgid, tarball)
          | (pkgid, RemoteSourceRepoPackage _ (Just tarball)) <- allPkgLocations ]

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
                map (\grp@((_,repo):|_) -> (repo, map fst (NE.toList grp)))
              . NE.groupBy ((==)    `on` (remoteRepoName . repoRemote . snd))
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
                          ++ remoteTarballPkgs
                          ++ sourceRepoTarballPkgs
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
             -> [PackageSpecifier UnresolvedSourcePackage]
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

    --TODO: [nice to have] disable multiple instances restriction in
    -- the solver, but then make sure we can cope with that in the
    -- output.
    resolverParams =

        setMaxBackjumps solverSettingMaxBackjumps

      . setIndependentGoals solverSettingIndependentGoals

      . setReorderGoals solverSettingReorderGoals

      . setCountConflicts solverSettingCountConflicts

      . setFineGrainedConflicts solverSettingFineGrainedConflicts

      . setMinimizeConflictSet solverSettingMinimizeConflictSet

        --TODO: [required eventually] should only be configurable for
        --custom installs
   -- . setAvoidReinstalls solverSettingAvoidReinstalls

        --TODO: [required eventually] should only be configurable for
        --custom installs
   -- . setShadowPkgs solverSettingShadowPkgs

      . setStrongFlags solverSettingStrongFlags

      . setAllowBootLibInstalls solverSettingAllowBootLibInstalls

      . setOnlyConstrained solverSettingOnlyConstrained

      . setSolverVerbosity verbosity

        --TODO: [required eventually] decide if we need to prefer
        -- installed for global packages, or prefer latest even for
        -- global packages. Perhaps should be configurable but with a
        -- different name than "upgrade-dependencies".
      . setPreferenceDefault PreferLatestForSelected
                           {-(if solverSettingUpgradeDeps
                                then PreferAllLatest
                                else PreferLatestForSelected)-}

      . removeLowerBounds solverSettingAllowOlder
      . removeUpperBounds solverSettingAllowNewer

      . addDefaultSetupDependencies (defaultSetupDeps comp platform
                                   . PD.packageDescription
                                   . srcpkgDescription)

      . addSetupCabalMinVersionConstraint setupMinCabalVersionConstraint
      . addSetupCabalMaxVersionConstraint setupMaxCabalVersionConstraint

      . addPreferences
          -- preferences from the config file or command line
          [ PackageVersionPreference name ver
          | PackageVersionConstraint name ver <- solverSettingPreferences ]

      . addConstraints
          -- version constraints from the config file or command line
            [ LabeledPackageConstraint (userToPackageConstraint pc) src
            | (pc, src) <- solverSettingConstraints ]

      . addPreferences
          -- enable stanza preference where the user did not specify
          [ PackageStanzasPreference pkgname stanzas
          | pkg <- localPackages
          , let pkgname = pkgSpecifierTarget pkg
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
          , let pkgname = pkgSpecifierTarget pkg
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
          , not (PD.nullFlagAssignment flags)
          , pkg <- localPackages
          , let pkgname = pkgSpecifierTarget pkg ]

      $ stdResolverParams

    stdResolverParams =
      -- Note: we don't use the standardInstallPolicy here, since that uses
      -- its own addDefaultSetupDependencies that is not appropriate for us.
      basicInstallPolicy
        installedPkgIndex sourcePkgDb
        localPackages

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
    --
    -- Moreover, lib:Cabal generally only supports the interface of
    -- current and past compilers; in fact recent lib:Cabal versions
    -- will warn when they encounter a too new or unknown GHC compiler
    -- version (c.f. #415). To avoid running into unsupported
    -- configurations we encode the compatibility matrix as lower
    -- bounds on lib:Cabal here (effectively corresponding to the
    -- respective major Cabal version bundled with the respective GHC
    -- release).
    --
    -- GHC 9.0   needs  Cabal >= 3.4
    -- GHC 8.10  needs  Cabal >= 3.2
    -- GHC 8.8   needs  Cabal >= 3.0
    -- GHC 8.6   needs  Cabal >= 2.4
    -- GHC 8.4   needs  Cabal >= 2.2
    -- GHC 8.2   needs  Cabal >= 2.0
    -- GHC 8.0   needs  Cabal >= 1.24
    -- GHC 7.10  needs  Cabal >= 1.22
    --
    -- (NB: we don't need to consider older GHCs as Cabal >= 1.20 is
    -- the absolute lower bound)
    --
    -- TODO: long-term, this compatibility matrix should be
    --       stored as a field inside 'Distribution.Compiler.Compiler'
    setupMinCabalVersionConstraint
      | isGHC, compVer >= mkVersion [9,0]  = mkVersion [3,4]
      | isGHC, compVer >= mkVersion [8,10] = mkVersion [3,2]
      | isGHC, compVer >= mkVersion [8,8]  = mkVersion [3,0]
      | isGHC, compVer >= mkVersion [8,6]  = mkVersion [2,4]
      | isGHC, compVer >= mkVersion [8,4]  = mkVersion [2,2]
      | isGHC, compVer >= mkVersion [8,2]  = mkVersion [2,0]
      | isGHC, compVer >= mkVersion [8,0]  = mkVersion [1,24]
      | isGHC, compVer >= mkVersion [7,10] = mkVersion [1,22]
      | otherwise                          = mkVersion [1,20]
      where
        isGHC    = compFlav `elem` [GHC,GHCJS]
        compFlav = compilerFlavor comp
        compVer  = compilerVersion comp

    -- As we can't predict the future, we also place a global upper
    -- bound on the lib:Cabal version we know how to interact with:
    --
    -- The upper bound is computed by incrementing the current major
    -- version twice in order to allow for the current version, as
    -- well as the next adjacent major version (one of which will not
    -- be released, as only "even major" versions of Cabal are
    -- released to Hackage or bundled with proper GHC releases).
    --
    -- For instance, if the current version of cabal-install is an odd
    -- development version, e.g.  Cabal-2.1.0.0, then we impose an
    -- upper bound `setup.Cabal < 2.3`; if `cabal-install` is on a
    -- stable/release even version, e.g. Cabal-2.2.1.0, the upper
    -- bound is `setup.Cabal < 2.4`. This gives us enough flexibility
    -- when dealing with development snapshots of Cabal and cabal-install.
    --
    setupMaxCabalVersionConstraint =
      alterVersion (take 2) $ incVersion 1 $ incVersion 1 cabalVersion

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

-- Note [SolverId to ConfiguredId]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Dependency solving is a per package affair, so after we're done, we
-- end up with 'SolverInstallPlan' that records in 'solverPkgLibDeps'
-- and 'solverPkgExeDeps' what packages provide the libraries and executables
-- needed by each component of the package (phew!)  For example, if I have
--
--      library
--          build-depends: lib
--          build-tool-depends: pkg:exe1
--          build-tools: alex
--
-- After dependency solving, I find out that this library component has
-- library dependencies on lib-0.2, and executable dependencies on pkg-0.1
-- and alex-0.3 (other components of the package may have different
-- dependencies).  Note that I've "lost" the knowledge that I depend
-- *specifically* on the exe1 executable from pkg.
--
-- So, we have a this graph of packages, and we need to transform it into
-- a graph of components which we are actually going to build.  In particular:
--
-- NODE changes from PACKAGE (SolverPackage) to COMPONENTS (ElaboratedConfiguredPackage)
-- EDGE changes from PACKAGE DEP (SolverId) to COMPONENT DEPS (ConfiguredId)
--
-- In both cases, what was previously a single node/edge may turn into multiple
-- nodes/edges.  Multiple components, because there may be multiple components
-- in a package; multiple component deps, because we may depend upon multiple
-- executables from the same package (and maybe, some day, multiple libraries
-- from the same package.)
--
-- Let's talk about how to do this transformation. Naively, we might consider
-- just processing each package, converting it into (zero or) one or more
-- components.  But we also have to update the edges; this leads to
-- two complications:
--
--      1. We don't know what the ConfiguredId of a component is until
--      we've configured it, but we cannot configure a component unless
--      we know the ConfiguredId of all its dependencies.  Thus, we must
--      process the 'SolverInstallPlan' in topological order.
--
--      2. When we process a package, we know the SolverIds of its
--      dependencies, but we have to do some work to turn these into
--      ConfiguredIds.  For example, in the case of build-tool-depends, the
--      SolverId isn't enough to uniquely determine the ConfiguredId we should
--      elaborate to: we have to look at the executable name attached to
--      the package name in the package description to figure it out.
--      At the same time, we NEED to use the SolverId, because there might
--      be multiple versions of the same package in the build plan
--      (due to setup dependencies); we can't just look up the package name
--      from the package description.
--
-- We can adopt the following strategy:
--
--      * When a package is transformed into components, record
--        a mapping from SolverId to ALL of the components
--        which were elaborated.
--
--      * When we look up an edge, we use our knowledge of the
--        component name to *filter* the list of components into
--        the ones we actually wanted to refer to.
--
-- By the way, we can tell that SolverInstallPlan is not the "right" type
-- because a SolverId cannot adequately represent all possible dependency
-- solver states: we may need to record foo-0.1 multiple times in
-- the solver install plan with different dependencies.  This imprecision in the
-- type currently doesn't cause any problems because the dependency solver
-- continues to enforce the single instance restriction regardless of compiler
-- version.  The right way to solve this is to come up with something very much
-- like a 'ConfiguredId', in that it incorporates the version choices of its
-- dependencies, but less fine grained.


-- | Produce an elaborated install plan using the policy for local builds with
-- a nix-style shared store.
--
-- In theory should be able to make an elaborated install plan with a policy
-- matching that of the classic @cabal install --user@ or @--global@
--
elaborateInstallPlan
  :: Verbosity -> Platform -> Compiler -> ProgramDb -> PkgConfigDb
  -> DistDirLayout
  -> StoreDirLayout
  -> SolverInstallPlan
  -> [PackageSpecifier (SourcePackage (PackageLocation loc))]
  -> Map PackageId PackageSourceHash
  -> InstallDirs.InstallDirTemplates
  -> ProjectConfigShared
  -> PackageConfig
  -> PackageConfig
  -> Map PackageName PackageConfig
  -> LogProgress (ElaboratedInstallPlan, ElaboratedSharedConfig)
elaborateInstallPlan verbosity platform compiler compilerprogdb pkgConfigDB
                     distDirLayout@DistDirLayout{..}
                     storeDirLayout@StoreDirLayout{storePackageDBStack}
                     solverPlan localPackages
                     sourcePackageHashes
                     defaultInstallDirs
                     sharedPackageConfig
                     allPackagesConfig
                     localPackagesConfig
                     perPackageConfig = do
    x <- elaboratedInstallPlan
    return (x, elaboratedSharedConfig)
  where
    elaboratedSharedConfig =
      ElaboratedSharedConfig {
        pkgConfigPlatform      = platform,
        pkgConfigCompiler      = compiler,
        pkgConfigCompilerProgs = compilerprogdb,
        pkgConfigReplOptions   = []
      }

    preexistingInstantiatedPkgs =
        Map.fromList (mapMaybe f (SolverInstallPlan.toList solverPlan))
      where
        f (SolverInstallPlan.PreExisting inst)
            | let ipkg = instSolverPkgIPI inst
            , not (IPI.indefinite ipkg)
            = Just (IPI.installedUnitId ipkg,
                     (FullUnitId (IPI.installedComponentId ipkg)
                                 (Map.fromList (IPI.instantiatedWith ipkg))))
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
                             quotes (pretty (packageId pkg))) $
               map InstallPlan.Configured <$> elaborateSolverToComponents mapDep pkg

    -- NB: We don't INSTANTIATE packages at this point.  That's
    -- a post-pass.  This makes it simpler to compute dependencies.
    elaborateSolverToComponents
        :: (SolverId -> [ElaboratedPlanPackage])
        -> SolverPackage UnresolvedPkgLoc
        -> LogProgress [ElaboratedConfiguredPackage]
    elaborateSolverToComponents mapDep spkg@(SolverPackage _ _ _ deps0 exe_deps0)
        = case mkComponentsGraph (elabEnabledSpec elab0) pd of
           Right g -> do
            let src_comps = componentsGraphToList g
            infoProgress $ hang (text "Component graph for" <+> pretty pkgid <<>> colon)
                            4 (dispComponentsWithDeps src_comps)
            (_, comps) <- mapAccumM buildComponent
                            (Map.empty, Map.empty, Map.empty)
                            (map fst src_comps)
            let not_per_component_reasons = why_not_per_component src_comps
            if null not_per_component_reasons
                then return comps
                else do checkPerPackageOk comps not_per_component_reasons
                        return [elaborateSolverToPackage spkg g $
                                comps ++ maybeToList setupComponent]
           Left cns ->
            dieProgress $
                hang (text "Dependency cycle between the following components:") 4
                     (vcat (map (text . componentNameStanza) cns))
      where
        -- You are eligible to per-component build if this list is empty
        why_not_per_component g
            = cuz_buildtype ++ cuz_spec ++ cuz_length ++ cuz_flag ++ cuz_coverage
          where
            cuz reason = [text reason]
            -- We have to disable per-component for now with
            -- Configure-type scripts in order to prevent parallel
            -- invocation of the same `./configure` script.
            -- See https://github.com/haskell/cabal/issues/4548
            --
            -- Moreoever, at this point in time, only non-Custom setup scripts
            -- are supported.  Implementing per-component builds with
            -- Custom would require us to create a new 'ElabSetup'
            -- type, and teach all of the code paths how to handle it.
            -- Once you've implemented this, swap it for the code below.
            cuz_buildtype =
                case PD.buildType (elabPkgDescription elab0) of
                    PD.Configure -> cuz "build-type is Configure"
                    PD.Custom -> cuz "build-type is Custom"
                    _         -> []
            -- cabal-format versions prior to 1.8 have different build-depends semantics
            -- for now it's easier to just fallback to legacy-mode when specVersion < 1.8
            -- see, https://github.com/haskell/cabal/issues/4121
            cuz_spec
                | PD.specVersion pd >= CabalSpecV1_8 = []
                | otherwise = cuz "cabal-version is less than 1.8"
            -- In the odd corner case that a package has no components at all
            -- then keep it as a whole package, since otherwise it turns into
            -- 0 component graph nodes and effectively vanishes. We want to
            -- keep it around at least for error reporting purposes.
            cuz_length
                | length g > 0 = []
                | otherwise    = cuz "there are no buildable components"
            -- For ease of testing, we let per-component builds be toggled
            -- at the top level
            cuz_flag
                | fromFlagOrDefault True (projectConfigPerComponent sharedPackageConfig)
                = []
                | otherwise = cuz "you passed --disable-per-component"
            -- Enabling program coverage introduces odd runtime dependencies
            -- between components.
            cuz_coverage
                | fromFlagOrDefault False (packageConfigCoverage localPackagesConfig)
                = cuz "program coverage is enabled"
                | otherwise = []

        -- | Sometimes a package may make use of features which are only
        -- supported in per-package mode.  If this is the case, we should
        -- give an error when this occurs.
        checkPerPackageOk comps reasons = do
            let is_sublib (CLibName (LSubLibName _)) = True
                is_sublib _ = False
            when (any (matchElabPkg is_sublib) comps) $
                dieProgress $
                    text "Internal libraries only supported with per-component builds." $$
                    text "Per-component builds were disabled because" <+>
                        fsep (punctuate comma reasons)
            -- TODO: Maybe exclude Backpack too

        elab0 = elaborateSolverToCommon spkg
        pkgid = elabPkgSourceId    elab0
        pd    = elabPkgDescription elab0

        -- TODO: This is just a skeleton to get elaborateSolverToPackage
        -- working correctly
        -- TODO: When we actually support building these components, we
        -- have to add dependencies on this from all other components
        setupComponent :: Maybe ElaboratedConfiguredPackage
        setupComponent
            | PD.buildType (elabPkgDescription elab0) == PD.Custom
            = Just elab0 {
                elabModuleShape = emptyModuleShape,
                elabUnitId = notImpl "elabUnitId",
                elabComponentId = notImpl "elabComponentId",
                elabLinkedInstantiatedWith = Map.empty,
                elabInstallDirs = notImpl "elabInstallDirs",
                elabPkgOrComp = ElabComponent (ElaboratedComponent {..})
              }
            | otherwise
            = Nothing
          where
            compSolverName      = CD.ComponentSetup
            compComponentName   = Nothing
            dep_pkgs = elaborateLibSolverId mapDep =<< CD.setupDeps deps0
            compLibDependencies
                = map configuredId dep_pkgs
            compLinkedLibDependencies = notImpl "compLinkedLibDependencies"
            compOrderLibDependencies = notImpl "compOrderLibDependencies"
            -- Not supported:
            compExeDependencies         = []
            compExeDependencyPaths      = []
            compPkgConfigDependencies   = []

            notImpl f =
                error $ "Distribution.Client.ProjectPlanning.setupComponent: " ++
                        f ++ " not implemented yet"


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

            -- 1. Configure the component, but with a place holder ComponentId.
            cc0 <- toConfiguredComponent
                    pd
                    (error "Distribution.Client.ProjectPlanning.cc_cid: filled in later")
                    (Map.unionWith Map.union external_lib_cc_map cc_map)
                    (Map.unionWith Map.union external_exe_cc_map cc_map)
                    comp


            -- 2. Read out the dependencies from the ConfiguredComponent cc0
            let compLibDependencies =
                    -- Nub because includes can show up multiple times
                    ordNub (map (annotatedIdToConfiguredId . ci_ann_id)
                                (cc_includes cc0))
                compExeDependencies =
                    map annotatedIdToConfiguredId
                        (cc_exe_deps cc0)
                compExeDependencyPaths =
                    [ (annotatedIdToConfiguredId aid', path)
                    | aid' <- cc_exe_deps cc0
                    , Just paths <- [Map.lookup (ann_id aid') exe_map1]
                    , path <- paths ]
                elab_comp = ElaboratedComponent {..}

            -- 3. Construct a preliminary ElaboratedConfiguredPackage,
            -- and use this to compute the component ID.  Fix up cc_id
            -- correctly.
            let elab1 = elab0 {
                        elabPkgOrComp = ElabComponent $ elab_comp
                     }
                cid = case elabBuildStyle elab0 of
                        BuildInplaceOnly ->
                          mkComponentId $
                            prettyShow pkgid ++ "-inplace" ++
                              (case Cabal.componentNameString cname of
                                  Nothing -> ""
                                  Just s -> "-" ++ prettyShow s)
                        BuildAndInstall ->
                          hashedInstalledPackageId
                            (packageHashInputs
                                elaboratedSharedConfig
                                elab1) -- knot tied
                cc = cc0 { cc_ann_id = fmap (const cid) (cc_ann_id cc0) }
            infoProgress $ dispConfiguredComponent cc

            -- 4. Perform mix-in linking
            let lookup_uid def_uid =
                    case Map.lookup (unDefUnitId def_uid) preexistingInstantiatedPkgs of
                        Just full -> full
                        Nothing -> error ("lookup_uid: " ++ prettyShow def_uid)
            lc <- toLinkedComponent verbosity lookup_uid (elabPkgSourceId elab0)
                        (Map.union external_lc_map lc_map) cc
            infoProgress $ dispLinkedComponent lc
            -- NB: elab is setup to be the correct form for an
            -- indefinite library, or a definite library with no holes.
            -- We will modify it in 'instantiateInstallPlan' to handle
            -- instantiated packages.

            -- 5. Construct the final ElaboratedConfiguredPackage
            let
                elab2 = elab1 {
                    elabModuleShape = lc_shape lc,
                    elabUnitId      = abstractUnitId (lc_uid lc),
                    elabComponentId = lc_cid lc,
                    elabLinkedInstantiatedWith = Map.fromList (lc_insts lc),
                    elabPkgOrComp = ElabComponent $ elab_comp {
                        compLinkedLibDependencies = ordNub (map ci_id (lc_includes lc)),
                        compOrderLibDependencies =
                          ordNub (map (abstractUnitId . ci_id)
                                      (lc_includes lc ++ lc_sig_includes lc))
                      }
                   }
                elab = elab2 {
                    elabInstallDirs = computeInstallDirs
                      storeDirLayout
                      defaultInstallDirs
                      elaboratedSharedConfig
                      elab2
                   }

            -- 6. Construct the updated local maps
            let cc_map'  = extendConfiguredComponentMap cc cc_map
                lc_map'  = extendLinkedComponentMap lc lc_map
                exe_map' = Map.insert cid (inplace_bin_dir elab) exe_map

            return ((cc_map', lc_map', exe_map'), elab)
          where
            compLinkedLibDependencies = error "buildComponent: compLinkedLibDependencies"
            compOrderLibDependencies = error "buildComponent: compOrderLibDependencies"

            cname = Cabal.componentName comp
            compComponentName = Just cname
            compSolverName = CD.componentNameToComponent cname

            -- NB: compLinkedLibDependencies and
            -- compOrderLibDependencies are defined when we define
            -- 'elab'.
            external_lib_dep_sids = CD.select (== compSolverName) deps0
            external_exe_dep_sids = CD.select (== compSolverName) exe_deps0

            external_lib_dep_pkgs = concatMap mapDep external_lib_dep_sids

            -- Combine library and build-tool dependencies, for backwards
            -- compatibility (See issue #5412 and the documentation for
            -- InstallPlan.fromSolverInstallPlan), but prefer the versions
            -- specified as build-tools.
            external_exe_dep_pkgs =
                concatMap mapDep $
                ordNubBy (pkgName . packageId) $
                external_exe_dep_sids ++ external_lib_dep_sids

            external_exe_map = Map.fromList $
                [ (getComponentId pkg, paths)
                | pkg <- external_exe_dep_pkgs
                , let paths = planPackageExePaths pkg ]
            exe_map1 = Map.union external_exe_map $ fmap (\x -> [x]) exe_map

            external_lib_cc_map = Map.fromListWith Map.union
                                $ map mkCCMapping external_lib_dep_pkgs
            external_exe_cc_map = Map.fromListWith Map.union
                                $ map mkCCMapping external_exe_dep_pkgs
            external_lc_map =
                Map.fromList $ map mkShapeMapping $
                external_lib_dep_pkgs ++ concatMap mapDep external_exe_dep_sids

            compPkgConfigDependencies =
                [ (pn, fromMaybe (error $ "compPkgConfigDependencies: impossible! "
                                            ++ prettyShow pn ++ " from "
                                            ++ prettyShow (elabPkgSourceId elab0))
                                 (pkgConfigDbPkgVersion pkgConfigDB pn))
                | PkgconfigDependency pn _ <- PD.pkgconfigDepends
                                                (Cabal.componentBuildInfo comp) ]

            inplace_bin_dir elab =
                binDirectoryFor
                    distDirLayout
                    elaboratedSharedConfig
                    elab $
                    case Cabal.componentNameString cname of
                             Just n -> prettyShow n
                             Nothing -> ""


    -- | Given a 'SolverId' referencing a dependency on a library, return
    -- the 'ElaboratedPlanPackage' corresponding to the library.  This
    -- returns at most one result.
    elaborateLibSolverId :: (SolverId -> [ElaboratedPlanPackage])
                         -> SolverId -> [ElaboratedPlanPackage]
    elaborateLibSolverId mapDep = filter (matchPlanPkg (== (CLibName LMainLibName))) . mapDep

    -- | Given an 'ElaboratedPlanPackage', return the paths to where the
    -- executables that this package represents would be installed.
    -- The only case where multiple paths can be returned is the inplace
    -- monolithic package one, since there can be multiple exes and each one
    -- has its own directory.
    planPackageExePaths :: ElaboratedPlanPackage -> [FilePath]
    planPackageExePaths =
        -- Pre-existing executables are assumed to be in PATH
        -- already.  In fact, this should be impossible.
        InstallPlan.foldPlanPackage (const []) $ \elab ->
            let
              executables :: [FilePath]
              executables =
                case elabPkgOrComp elab of
                    -- Monolithic mode: all exes of the package
                    ElabPackage _ -> unUnqualComponentName . PD.exeName
                                 <$> PD.executables (elabPkgDescription elab)
                    -- Per-component mode: just the selected exe
                    ElabComponent comp ->
                        case fmap Cabal.componentNameString
                                  (compComponentName comp) of
                            Just (Just n) -> [prettyShow n]
                            _ -> [""]
            in
              binDirectoryFor
                 distDirLayout
                 elaboratedSharedConfig
                 elab
                 <$> executables

    elaborateSolverToPackage :: SolverPackage UnresolvedPkgLoc
                             -> ComponentsGraph
                             -> [ElaboratedConfiguredPackage]
                             -> ElaboratedConfiguredPackage
    elaborateSolverToPackage
        pkg@(SolverPackage (SourcePackage pkgid _gpd _srcloc _descOverride)
                           _flags _stanzas _deps0 _exe_deps0)
        compGraph comps =
        -- Knot tying: the final elab includes the
        -- pkgInstalledId, which is calculated by hashing many
        -- of the other fields of the elaboratedPackage.
        elab
      where
        elab0@ElaboratedConfiguredPackage{..} = elaborateSolverToCommon pkg
        elab1 = elab0 {
                elabUnitId = newSimpleUnitId pkgInstalledId,
                elabComponentId = pkgInstalledId,
                elabLinkedInstantiatedWith = Map.empty,
                elabPkgOrComp = ElabPackage $ ElaboratedPackage {..},
                elabModuleShape = modShape
            }
        elab = elab1 {
                elabInstallDirs =
                  computeInstallDirs storeDirLayout
                                     defaultInstallDirs
                                     elaboratedSharedConfig
                                     elab1
            }

        modShape = case find (matchElabPkg (== (CLibName LMainLibName))) comps of
                        Nothing -> emptyModuleShape
                        Just e -> Ty.elabModuleShape e

        pkgInstalledId
          | shouldBuildInplaceOnly pkg
          = mkComponentId (prettyShow pkgid ++ "-inplace")

          | otherwise
          = assert (isJust elabPkgSourceHash) $
            hashedInstalledPackageId
              (packageHashInputs
                elaboratedSharedConfig
                elab)  -- recursive use of elab

          | otherwise
          = error $ "elaborateInstallPlan: non-inplace package "
                 ++ " is missing a source hash: " ++ prettyShow pkgid

        -- Need to filter out internal dependencies, because they don't
        -- correspond to anything real anymore.
        isExt confid = confSrcId confid /= pkgid
        filterExt  = filter isExt
        filterExt' = filter (isExt . fst)

        pkgLibDependencies
            = buildComponentDeps (filterExt  . compLibDependencies)
        pkgExeDependencies
            = buildComponentDeps (filterExt  . compExeDependencies)
        pkgExeDependencyPaths
            = buildComponentDeps (filterExt' . compExeDependencyPaths)
        -- TODO: Why is this flat?
        pkgPkgConfigDependencies
            = CD.flatDeps $ buildComponentDeps compPkgConfigDependencies

        pkgDependsOnSelfLib
            = CD.fromList [ (CD.componentNameToComponent cn, [()])
                          | Graph.N _ cn _ <- fromMaybe [] mb_closure ]
          where
            mb_closure = Graph.revClosure compGraph [ k | k <- Graph.keys compGraph, is_lib k ]
            -- NB: the sublib case should not occur, because sub-libraries
            -- are not supported without per-component builds
            is_lib (CLibName _) = True
            is_lib _ = False

        buildComponentDeps f
            = CD.fromList [ (compSolverName comp, f comp)
                          | ElaboratedConfiguredPackage{
                                elabPkgOrComp = ElabComponent comp
                            } <- comps
                          ]

        -- NB: This is not the final setting of 'pkgStanzasEnabled'.
        -- See [Sticky enabled testsuites]; we may enable some extra
        -- stanzas opportunistically when it is cheap to do so.
        --
        -- However, we start off by enabling everything that was
        -- requested, so that we can maintain an invariant that
        -- pkgStanzasEnabled is a superset of elabStanzasRequested
        pkgStanzasEnabled  = optStanzaKeysFilteredByValue (fromMaybe False) elabStanzasRequested

    elaborateSolverToCommon :: SolverPackage UnresolvedPkgLoc
                            -> ElaboratedConfiguredPackage
    elaborateSolverToCommon
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
        elabModuleShape     = error "elaborateSolverToCommon: elabModuleShape"

        elabIsCanonical     = True
        elabPkgSourceId     = pkgid
        elabPkgDescription  = case PD.finalizePD
                                    flags elabEnabledSpec (const True)
                                    platform (compilerInfo compiler)
                                    [] gdesc of
                               Right (desc, _) -> desc
                               Left _          -> error "Failed to finalizePD in elaborateSolverToCommon"
        elabFlagAssignment  = flags
        elabFlagDefaults    = PD.mkFlagAssignment
                              [ (Cabal.flagName flag, Cabal.flagDefault flag)
                              | flag <- PD.genPackageFlags gdesc ]

        elabEnabledSpec      = enableStanzas stanzas
        elabStanzasAvailable = stanzas

        elabStanzasRequested :: OptionalStanzaMap (Maybe Bool)
        elabStanzasRequested = optStanzaTabulate $ \o -> case o of
            -- NB: even if a package stanza is requested, if the package
            -- doesn't actually have any of that stanza we omit it from
            -- the request, to ensure that we don't decide that this
            -- package needs to be rebuilt.  (It needs to be done here,
            -- because the ElaboratedConfiguredPackage is where we test
            -- whether or not there have been changes.)
            TestStanzas  -> listToMaybe [ v | v <- maybeToList tests, _ <- PD.testSuites elabPkgDescription ]
            BenchStanzas -> listToMaybe [ v | v <- maybeToList benchmarks, _ <- PD.benchmarks elabPkgDescription ]
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
        elabConfigureTargets = []
        elabBuildTargets    = []
        elabTestTargets     = []
        elabBenchTargets    = []
        elabReplTarget      = Nothing
        elabHaddockTargets  = []

        elabBuildHaddocks   =
          perPkgOptionFlag pkgid False packageConfigDocumentation

        elabPkgSourceLocation = srcloc
        elabPkgSourceHash   = Map.lookup pkgid sourcePackageHashes
        elabLocalToProject  = isLocalToProject pkg
        elabBuildStyle      = if shouldBuildInplaceOnly pkg
                                then BuildInplaceOnly else BuildAndInstall
        elabBuildPackageDBStack    = buildAndRegisterDbs
        elabRegisterPackageDBStack = buildAndRegisterDbs

        elabSetupScriptStyle       = packageSetupScriptStyle elabPkgDescription
        elabSetupScriptCliVersion  =
          packageSetupScriptSpecVersion
          elabSetupScriptStyle elabPkgDescription libDepGraph deps0
        elabSetupPackageDBStack    = buildAndRegisterDbs

        elabInplaceBuildPackageDBStack = inplacePackageDbs
        elabInplaceRegisterPackageDBStack = inplacePackageDbs
        elabInplaceSetupPackageDBStack = inplacePackageDbs

        buildAndRegisterDbs
          | shouldBuildInplaceOnly pkg = inplacePackageDbs
          | otherwise                  = storePackageDbs

        elabPkgDescriptionOverride = descOverride

        elabVanillaLib    = perPkgOptionFlag pkgid True packageConfigVanillaLib --TODO: [required feature]: also needs to be handled recursively
        elabSharedLib     = pkgid `Set.member` pkgsUseSharedLibrary
        elabStaticLib     = perPkgOptionFlag pkgid False packageConfigStaticLib
        elabDynExe        = perPkgOptionFlag pkgid False packageConfigDynExe
        elabFullyStaticExe = perPkgOptionFlag pkgid False packageConfigFullyStaticExe
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
        elabSplitSections = perPkgOptionFlag pkgid False packageConfigSplitSections
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
        elabHaddockForHackage   = perPkgOptionFlag pkgid Cabal.ForDevelopment packageConfigHaddockForHackage
        elabHaddockExecutables  = perPkgOptionFlag pkgid False packageConfigHaddockExecutables
        elabHaddockTestSuites   = perPkgOptionFlag pkgid False packageConfigHaddockTestSuites
        elabHaddockBenchmarks   = perPkgOptionFlag pkgid False packageConfigHaddockBenchmarks
        elabHaddockInternal     = perPkgOptionFlag pkgid False packageConfigHaddockInternal
        elabHaddockCss          = perPkgOptionMaybe pkgid packageConfigHaddockCss
        elabHaddockLinkedSource = perPkgOptionFlag pkgid False packageConfigHaddockLinkedSource
        elabHaddockQuickJump    = perPkgOptionFlag pkgid False packageConfigHaddockQuickJump
        elabHaddockHscolourCss  = perPkgOptionMaybe pkgid packageConfigHaddockHscolourCss
        elabHaddockContents     = perPkgOptionMaybe pkgid packageConfigHaddockContents

        elabTestMachineLog      = perPkgOptionMaybe pkgid packageConfigTestMachineLog
        elabTestHumanLog        = perPkgOptionMaybe pkgid packageConfigTestHumanLog
        elabTestShowDetails     = perPkgOptionMaybe pkgid packageConfigTestShowDetails
        elabTestKeepTix         = perPkgOptionFlag pkgid False packageConfigTestKeepTix
        elabTestWrapper         = perPkgOptionMaybe pkgid packageConfigTestWrapper
        elabTestFailWhenNoTestSuites = perPkgOptionFlag pkgid False packageConfigTestFailWhenNoTestSuites
        elabTestTestOptions     = perPkgOptionList pkgid packageConfigTestTestOptions

        elabBenchmarkOptions    = perPkgOptionList pkgid packageConfigBenchmarkOptions

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
    lookupPerPkgOption pkg f =
        -- This is where we merge the options from the project config that
        -- apply to all packages, all project local packages, and to specific
        -- named packages
        global `mappend` local `mappend` perpkg
      where
        global = f allPackagesConfig
        local  | isLocalToProject pkg
               = f localPackagesConfig
               | otherwise
               = mempty
        perpkg = maybe mempty f (Map.lookup (packageName pkg) perPackageConfig)

    inplacePackageDbs = storePackageDbs
                     ++ [ distPackageDB (compilerId compiler) ]

    storePackageDbs   = storePackageDBStack (compilerId compiler)

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
          (map PlannedId (Set.toList pkgsLocalToProject))

    isLocalToProject :: Package pkg => pkg -> Bool
    isLocalToProject pkg = Set.member (packageId pkg)
                                      pkgsLocalToProject

    pkgsLocalToProject :: Set PackageId
    pkgsLocalToProject =
        Set.fromList (catMaybes (map shouldBeLocal localPackages))
        --TODO: localPackages is a misnomer, it's all project packages
        -- here is where we decide which ones will be local!

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
          , property pkg ] -- just the packages that satisfy the property
      --TODO: [nice to have] this does not check the config consistency,
      -- e.g. a package explicitly turning off profiling, but something
      -- depending on it that needs profiling. This really needs a separate
      -- package config validation/resolution pass.

      --TODO: [nice to have] config consistency checking:
      -- + profiling libs & exes, exe needs lib, recursive
      -- + shared libs & exes, exe needs lib, recursive
      -- + vanilla libs & exes, exe needs lib, recursive
      -- + ghci or shared lib needed by TH, recursive, ghc version dependent

-- TODO: Drop matchPlanPkg/matchElabPkg in favor of mkCCMapping

shouldBeLocal :: PackageSpecifier (SourcePackage (PackageLocation loc)) -> Maybe PackageId
shouldBeLocal NamedPackage{}              = Nothing
shouldBeLocal (SpecificSourcePackage pkg) = case srcpkgSource pkg of
    LocalUnpackedPackage _ -> Just (packageId pkg)
    _                      -> Nothing

-- | Given a 'ElaboratedPlanPackage', report if it matches a 'ComponentName'.
matchPlanPkg :: (ComponentName -> Bool) -> ElaboratedPlanPackage -> Bool
matchPlanPkg p = InstallPlan.foldPlanPackage (p . ipiComponentName) (matchElabPkg p)

-- | Get the appropriate 'ComponentName' which identifies an installed
-- component.
ipiComponentName :: IPI.InstalledPackageInfo -> ComponentName
ipiComponentName = CLibName . IPI.sourceLibName

-- | Given a 'ElaboratedConfiguredPackage', report if it matches a
-- 'ComponentName'.
matchElabPkg :: (ComponentName -> Bool) -> ElaboratedConfiguredPackage -> Bool
matchElabPkg p elab =
    case elabPkgOrComp elab of
        ElabComponent comp -> maybe False p (compComponentName comp)
        ElabPackage _ ->
            -- So, what should we do here?  One possibility is to
            -- unconditionally return 'True', because whatever it is
            -- that we're looking for, it better be in this package.
            -- But this is a bit dodgy if the package doesn't actually
            -- have, e.g., a library.  Fortunately, it's not possible
            -- for the build of the library/executables to be toggled
            -- by 'pkgStanzasEnabled', so the only thing we have to
            -- test is if the component in question is *buildable.*
            any (p . componentName)
                (Cabal.pkgBuildableComponents (elabPkgDescription elab))

-- | Given an 'ElaboratedPlanPackage', generate the mapping from 'PackageName'
-- and 'ComponentName' to the 'ComponentId' that should be used
-- in this case.
mkCCMapping :: ElaboratedPlanPackage
            -> (PackageName, Map ComponentName (AnnotatedId ComponentId))
mkCCMapping =
    InstallPlan.foldPlanPackage
       (\ipkg -> (packageName ipkg,
                    Map.singleton (ipiComponentName ipkg)
                                  -- TODO: libify
                                  (AnnotatedId {
                                    ann_id = IPI.installedComponentId ipkg,
                                    ann_pid = packageId ipkg,
                                    ann_cname = IPI.sourceComponentName ipkg
                                  })))
      $ \elab ->
        let mk_aid cn = AnnotatedId {
                            ann_id = elabComponentId elab,
                            ann_pid = packageId elab,
                            ann_cname = cn
                        }
        in (packageName elab,
            case elabPkgOrComp elab of
                ElabComponent comp ->
                    case compComponentName comp of
                        Nothing -> Map.empty
                        Just n  -> Map.singleton n (mk_aid n)
                ElabPackage _ ->
                    Map.fromList $
                        map (\comp -> let cn = Cabal.componentName comp in (cn, mk_aid cn))
                            (Cabal.pkgBuildableComponents (elabPkgDescription elab)))

-- | Given an 'ElaboratedPlanPackage', generate the mapping from 'ComponentId'
-- to the shape of this package, as per mix-in linking.
mkShapeMapping :: ElaboratedPlanPackage
               -> (ComponentId, (OpenUnitId, ModuleShape))
mkShapeMapping dpkg =
    (getComponentId dpkg, (indef_uid, shape))
  where
    (dcid, shape) =
        InstallPlan.foldPlanPackage
            -- Uses Monad (->)
            (liftM2 (,) IPI.installedComponentId shapeInstalledPackage)
            (liftM2 (,) elabComponentId elabModuleShape)
            dpkg
    indef_uid =
        IndefFullUnitId dcid
            (Map.fromList [ (req, OpenModuleVar req)
                          | req <- Set.toList (modShapeRequires shape)])

-- | Get the bin\/ directories that a package's executables should reside in.
--
-- The result may be empty if the package does not build any executables.
--
-- The result may have several entries if this is an inplace build of a package
-- with multiple executables.
binDirectories
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> [FilePath]
binDirectories layout config package = case elabBuildStyle package of
  -- quick sanity check: no sense returning a bin directory if we're not going
  -- to put any executables in it, that will just clog up the PATH
  _ | noExecutables -> []
  BuildAndInstall -> [installedBinDirectory package]
  BuildInplaceOnly -> map (root</>) $ case elabPkgOrComp package of
    ElabComponent comp -> case compSolverName comp of
      CD.ComponentExe n -> [prettyShow n]
      _ -> []
    ElabPackage _ -> map (prettyShow . PD.exeName)
                   . PD.executables
                   . elabPkgDescription
                   $ package
  where
  noExecutables = null . PD.executables . elabPkgDescription $ package
  root  =  distBuildDirectory layout (elabDistDirParams config package)
       </> "build"

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

extractElabBuildStyle :: InstallPlan.GenericPlanPackage ipkg ElaboratedConfiguredPackage
                      -> BuildStyle
extractElabBuildStyle (InstallPlan.Configured elab) = elabBuildStyle elab
extractElabBuildStyle _ = BuildAndInstall

-- instantiateInstallPlan is responsible for filling out an InstallPlan
-- with all of the extra Configured packages that would be generated by
-- recursively instantiating the dependencies of packages.
--
-- Suppose we are compiling the following packages:
--
--  unit f where
--    signature H
--
--  unit g where
--    dependency f[H=containers:Data.Map]
--
-- At entry, we have an InstallPlan with a single plan package per
-- actual source package, e.g., only (indefinite!) f and g.  The job of
-- instantiation is to turn this into three plan packages: each of the
-- packages as before, but also a new, definite package f[H=containers:Data.Map]
--
-- How do we do this?  The general strategy is to iterate over every
-- package in the existing plan and recursively create new entries for
-- each of its dependencies which is an instantiated package (e.g.,
-- f[H=p:G]).  This process must be recursive, as f itself may depend on
-- OTHER packages which it instantiated using its hole H.
--
-- Some subtleties:
--
--  * We have to keep track of whether or not we are instantiating with
--    inplace packages, because instantiating a non-inplace package with
--    an inplace packages makes it inplace (since it depends on
--    something in the inplace store)!  The rule is that if any of the
--    modules in an instantiation are inplace, then the instantiated
--    unit itself must be inplace.  There is then a bunch of faffing
--    about to keep track of BuildStyle.
--
--  * ElaboratedConfiguredPackage was never really designed for post
--    facto instantiation, so some of the steps for generating new
--    instantiations are a little fraught.  For example, the act of
--    flipping a package to be inplace involves faffing about with four
--    fields, because these fields are precomputed.  A good refactor
--    would be to reduce the amount of precomputation to simplify the
--    algorithm here.
--
--  * We use the state monad to cache already instantiated modules, so
--    we don't instantiate the same thing multiple times.
--
instantiateInstallPlan :: StoreDirLayout -> InstallDirs.InstallDirTemplates -> ElaboratedSharedConfig -> ElaboratedInstallPlan -> ElaboratedInstallPlan
instantiateInstallPlan storeDirLayout defaultInstallDirs elaboratedShared plan =
    InstallPlan.new (IndependentGoals False)
                    (Graph.fromDistinctList (Map.elems ready_map))
  where
    pkgs = InstallPlan.toList plan

    cmap = Map.fromList [ (getComponentId pkg, pkg) | pkg <- pkgs ]

    instantiateUnitId :: ComponentId -> Map ModuleName (Module, BuildStyle)
                      -> InstM (DefUnitId, BuildStyle)
    instantiateUnitId cid insts = state $ \s ->
        case Map.lookup uid s of
            Nothing ->
                -- Knot tied
                -- TODO: I don't think the knot tying actually does
                -- anything useful
                let (r, s') = runState (instantiateComponent uid cid insts)
                                       (Map.insert uid r s)
                in ((def_uid, extractElabBuildStyle r), Map.insert uid r s')
            Just r -> ((def_uid, extractElabBuildStyle r), s)
      where
        def_uid = mkDefUnitId cid (fmap fst insts)
        uid = unDefUnitId def_uid

    -- No need to InplaceT; the inplace-ness is properly computed for
    -- the ElaboratedPlanPackage, so that will implicitly pass it on
    instantiateComponent
        :: UnitId -> ComponentId -> Map ModuleName (Module, BuildStyle)
        -> InstM ElaboratedPlanPackage
    instantiateComponent uid cid insts
      | Just planpkg <- Map.lookup cid cmap
      = case planpkg of
          InstallPlan.Configured (elab0@ElaboratedConfiguredPackage
                                    { elabPkgOrComp = ElabComponent comp }) -> do
            deps <-
              traverse (fmap fst . substUnitId insts) (compLinkedLibDependencies comp)
            let build_style = fold (fmap snd insts)
            let getDep (Module dep_uid _) = [dep_uid]
                elab1 = fixupBuildStyle build_style $ elab0 {
                    elabUnitId = uid,
                    elabComponentId = cid,
                    elabInstantiatedWith = fmap fst insts,
                    elabIsCanonical = Map.null (fmap fst insts),
                    elabPkgOrComp = ElabComponent comp {
                        compOrderLibDependencies =
                            (if Map.null insts then [] else [newSimpleUnitId cid]) ++
                            ordNub (map unDefUnitId
                                (deps ++ concatMap (getDep . fst) (Map.elems insts)))
                    }
                  }
                elab = elab1 {
                    elabInstallDirs = computeInstallDirs storeDirLayout
                                                         defaultInstallDirs
                                                         elaboratedShared
                                                         elab1
                  }
            return $ InstallPlan.Configured elab
          _ -> return planpkg
      | otherwise = error ("instantiateComponent: " ++ prettyShow cid)

    substUnitId :: Map ModuleName (Module, BuildStyle) -> OpenUnitId -> InstM (DefUnitId, BuildStyle)
    substUnitId _ (DefiniteUnitId uid) =
        -- This COULD actually, secretly, be an inplace package, but in
        -- that case it doesn't matter as it's already been recorded
        -- in the package that depends on this
        return (uid, BuildAndInstall)
    substUnitId subst (IndefFullUnitId cid insts) = do
        insts' <- substSubst subst insts
        instantiateUnitId cid insts'

    -- NB: NOT composition
    substSubst :: Map ModuleName (Module, BuildStyle)
               -> Map ModuleName OpenModule
               -> InstM (Map ModuleName (Module, BuildStyle))
    substSubst subst insts = traverse (substModule subst) insts

    substModule :: Map ModuleName (Module, BuildStyle) -> OpenModule -> InstM (Module, BuildStyle)
    substModule subst (OpenModuleVar mod_name)
        | Just m <- Map.lookup mod_name subst = return m
        | otherwise = error "substModule: non-closing substitution"
    substModule subst (OpenModule uid mod_name) = do
        (uid', build_style) <- substUnitId subst uid
        return (Module uid' mod_name, build_style)

    indefiniteUnitId :: ComponentId -> InstM UnitId
    indefiniteUnitId cid = do
        let uid = newSimpleUnitId cid
        r <- indefiniteComponent uid cid
        state $ \s -> (uid, Map.insert uid r s)

    indefiniteComponent :: UnitId -> ComponentId -> InstM ElaboratedPlanPackage
    indefiniteComponent _uid cid
      -- Only need Configured; this phase happens before improvement, so
      -- there shouldn't be any Installed packages here.
      | Just (InstallPlan.Configured epkg) <- Map.lookup cid cmap
      , ElabComponent elab_comp <- elabPkgOrComp epkg
      = do -- We need to do a little more processing of the includes: some
           -- of them are fully definite even without substitution.  We
           -- want to build those too; see #5634.
           --
           -- This code mimics similar code in Distribution.Backpack.ReadyComponent;
           -- however, unlike the conversion from LinkedComponent to
           -- ReadyComponent, this transformation is done *without*
           -- changing the type in question; and what we are simply
           -- doing is enforcing tighter invariants on the data
           -- structure in question.  The new invariant is that there
           -- is no IndefFullUnitId in compLinkedLibDependencies that actually
           -- has no holes.  We couldn't specify this invariant when
           -- we initially created the ElaboratedPlanPackage because
           -- we have no way of actually reifying the UnitId into a
           -- DefiniteUnitId (that's what substUnitId does!)
           new_deps <- for (compLinkedLibDependencies elab_comp) $ \uid ->
             if Set.null (openUnitIdFreeHoles uid)
                then fmap (DefiniteUnitId . fst) (substUnitId Map.empty uid)
                else return uid
           -- NB: no fixupBuildStyle needed here, as if the indefinite
           -- component depends on any inplace packages, it itself must
           -- be indefinite!  There is no substitution here, we can't
           -- post facto add inplace deps
           return . InstallPlan.Configured $ epkg {
            elabPkgOrComp = ElabComponent elab_comp {
                compLinkedLibDependencies = new_deps,
                -- I think this is right: any new definite unit ids we
                -- minted in the phase above need to be built before us.
                -- Add 'em in.  This doesn't remove any old dependencies
                -- on the indefinite package; they're harmless.
                compOrderLibDependencies =
                    ordNub $ compOrderLibDependencies elab_comp ++
                             [unDefUnitId d | DefiniteUnitId d <- new_deps]
            }
           }
      | Just planpkg <- Map.lookup cid cmap
      = return planpkg
      | otherwise = error ("indefiniteComponent: " ++ prettyShow cid)

    fixupBuildStyle BuildAndInstall elab = elab
    fixupBuildStyle _ (elab@ElaboratedConfiguredPackage { elabBuildStyle = BuildInplaceOnly }) = elab
    fixupBuildStyle BuildInplaceOnly elab = elab {
      elabBuildStyle = BuildInplaceOnly,
      elabBuildPackageDBStack = elabInplaceBuildPackageDBStack elab,
      elabRegisterPackageDBStack = elabInplaceRegisterPackageDBStack elab,
      elabSetupPackageDBStack = elabInplaceSetupPackageDBStack elab
    }

    ready_map = execState work Map.empty

    work = for_ pkgs $ \pkg ->
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

-- data ComponentTarget = ...
-- data SubComponentTarget = ...

-- One step in the build system is to translate higher level intentions like
-- "build this package", "test that package", or "repl that component" into
-- a more detailed specification of exactly which components to build (or other
-- actions like repl or build docs). This translation is somewhat different for
-- different commands. For example "test" for a package will build a different
-- set of components than "build". In addition, the translation of these
-- intentions can fail. For example "run" for a package is only unambiguous
-- when the package has a single executable.
--
-- So we need a little bit of infrastructure to make it easy for the command
-- implementations to select what component targets are meant when a user asks
-- to do something with a package or component. To do this (and to be able to
-- produce good error messages for mistakes and when targets are not available)
-- we need to gather and summarise accurate information about all the possible
-- targets, both available and unavailable. Then a command implementation can
-- decide which of the available component targets should be selected.

-- | An available target represents a component within a package that a user
-- command could plausibly refer to. In this sense, all the components defined
-- within the package are things the user could refer to, whether or not it
-- would actually be possible to build that component.
--
-- In particular the available target contains an 'AvailableTargetStatus' which
-- informs us about whether it's actually possible to select this component to
-- be built, and if not why not. This detail makes it possible for command
-- implementations (like @build@, @test@ etc) to accurately report why a target
-- cannot be used.
--
-- Note that the type parameter is used to help enforce that command
-- implementations can only select targets that can actually be built (by
-- forcing them to return the @k@ value for the selected targets).
-- In particular 'resolveTargets' makes use of this (with @k@ as
-- @('UnitId', ComponentName')@) to identify the targets thus selected.
--
data AvailableTarget k = AvailableTarget {
       availableTargetPackageId      :: PackageId,
       availableTargetComponentName  :: ComponentName,
       availableTargetStatus         :: AvailableTargetStatus k,
       availableTargetLocalToProject :: Bool
     }
  deriving (Eq, Show, Functor)

-- | The status of a an 'AvailableTarget' component. This tells us whether
-- it's actually possible to select this component to be built, and if not
-- why not.
--
data AvailableTargetStatus k =
       TargetDisabledByUser   -- ^ When the user does @tests: False@
     | TargetDisabledBySolver -- ^ When the solver could not enable tests
     | TargetNotBuildable     -- ^ When the component has @buildable: False@
     | TargetNotLocal         -- ^ When the component is non-core in a non-local package
     | TargetBuildable k TargetRequested -- ^ The target can or should be built
  deriving (Eq, Ord, Show, Functor)

-- | This tells us whether a target ought to be built by default, or only if
-- specifically requested. The policy is that components like libraries and
-- executables are built by default by @build@, but test suites and benchmarks
-- are not, unless this is overridden in the project configuration.
--
data TargetRequested =
       TargetRequestedByDefault    -- ^ To be built by default
     | TargetNotRequestedByDefault -- ^ Not to be built by default
  deriving (Eq, Ord, Show)

-- | Given the install plan, produce the set of 'AvailableTarget's for each
-- package-component pair.
--
-- Typically there will only be one such target for each component, but for
-- example if we have a plan with both normal and profiling variants of a
-- component then we would get both as available targets, or similarly if we
-- had a plan that contained two instances of the same version of a package.
-- This approach makes it relatively easy to select all instances\/variants
-- of a component.
--
availableTargets :: ElaboratedInstallPlan
                 -> Map (PackageId, ComponentName)
                        [AvailableTarget (UnitId, ComponentName)]
availableTargets installPlan =
    let rs = [ (pkgid, cname, fake, target)
             | pkg <- InstallPlan.toList installPlan
             , (pkgid, cname, fake, target) <- case pkg of
                 InstallPlan.PreExisting ipkg -> availableInstalledTargets ipkg
                 InstallPlan.Installed   elab -> availableSourceTargets elab
                 InstallPlan.Configured  elab -> availableSourceTargets elab
             ]
     in Map.union
         (Map.fromListWith (++)
            [ ((pkgid, cname), [target])
            | (pkgid, cname, fake, target) <- rs, not fake])
         (Map.fromList
            [ ((pkgid, cname), [target])
            | (pkgid, cname, fake, target) <- rs, fake])
    -- The normal targets mask the fake ones. We get all instances of the
    -- normal ones and only one copy of the fake ones (as there are many
    -- duplicates of the fake ones). See 'availableSourceTargets' below for
    -- more details on this fake stuff is about.

availableInstalledTargets :: IPI.InstalledPackageInfo
                          -> [(PackageId, ComponentName, Bool,
                               AvailableTarget (UnitId, ComponentName))]
availableInstalledTargets ipkg =
    let unitid = installedUnitId ipkg
        cname  = CLibName LMainLibName
        status = TargetBuildable (unitid, cname) TargetRequestedByDefault
        target = AvailableTarget (packageId ipkg) cname status False
        fake   = False
     in [(packageId ipkg, cname, fake, target)]

availableSourceTargets :: ElaboratedConfiguredPackage
                       -> [(PackageId, ComponentName, Bool,
                            AvailableTarget (UnitId, ComponentName))]
availableSourceTargets elab =
    -- We have a somewhat awkward problem here. We need to know /all/ the
    -- components from /all/ the packages because these are the things that
    -- users could refer to. Unfortunately, at this stage the elaborated install
    -- plan does /not/ contain all components: some components have already
    -- been deleted because they cannot possibly be built. This is the case
    -- for components that are marked @buildable: False@ in their .cabal files.
    -- (It's not unreasonable that the unbuildable components have been pruned
    -- as the plan invariant is considerably simpler if all nodes can be built)
    --
    -- We can recover the missing components but it's not exactly elegant. For
    -- a graph node corresponding to a component we still have the information
    -- about the package that it came from, and this includes the names of
    -- /all/ the other components in the package. So in principle this lets us
    -- find the names of all components, plus full details of the buildable
    -- components.
    --
    -- Consider for example a package with 3 exe components: foo, bar and baz
    -- where foo and bar are buildable, but baz is not. So the plan contains
    -- nodes for the components foo and bar. Now we look at each of these two
    -- nodes and look at the package they come from and the names of the
    -- components in this package. This will give us the names foo, bar and
    -- baz, twice (once for each of the two buildable components foo and bar).
    --
    -- We refer to these reconstructed missing components as fake targets.
    -- It is an invariant that they are not available to be built.
    --
    -- To produce the final set of targets we put the fake targets in a finite
    -- map (thus eliminating the duplicates) and then we overlay that map with
    -- the normal buildable targets. (This is done above in 'availableTargets'.)
    --
    [ (packageId elab, cname, fake, target)
    | component <- pkgComponents (elabPkgDescription elab)
    , let cname  = componentName component
          status = componentAvailableTargetStatus component
          target = AvailableTarget {
                     availableTargetPackageId      = packageId elab,
                     availableTargetComponentName  = cname,
                     availableTargetStatus         = status,
                     availableTargetLocalToProject = elabLocalToProject elab
                   }
          fake   = isFakeTarget cname

    -- TODO: The goal of this test is to exclude "instantiated"
    -- packages as available targets. This means that you can't
    -- ask for a particular instantiated component to be built;
    -- it will only get built by a dependency.  Perhaps the
    -- correct way to implement this is to run selection
    -- prior to instantiating packages.  If you refactor
    -- this, then you can delete this test.
    , elabIsCanonical elab

      -- Filter out some bogus parts of the cross product that are never needed
    , case status of
        TargetBuildable{} | fake -> False
        _                        -> True
    ]
  where
    isFakeTarget cname =
      case elabPkgOrComp elab of
        ElabPackage _               -> False
        ElabComponent elabComponent -> compComponentName elabComponent
                                       /= Just cname

    componentAvailableTargetStatus
      :: Component -> AvailableTargetStatus (UnitId, ComponentName)
    componentAvailableTargetStatus component =
        case componentOptionalStanza $ CD.componentNameToComponent cname of
          -- it is not an optional stanza, so a library, exe or foreign lib
          Nothing
            | not buildable  -> TargetNotBuildable
            | otherwise      -> TargetBuildable (elabUnitId elab, cname)
                                                TargetRequestedByDefault

          -- it is not an optional stanza, so a testsuite or benchmark
          Just stanza ->
            case (optStanzaLookup stanza (elabStanzasRequested elab), -- TODO
                  optStanzaSetMember stanza (elabStanzasAvailable elab)) of
              _ | not withinPlan -> TargetNotLocal
              (Just False,   _)  -> TargetDisabledByUser
              (Nothing,  False)  -> TargetDisabledBySolver
              _ | not buildable  -> TargetNotBuildable
              (Just True, True)  -> TargetBuildable (elabUnitId elab, cname)
                                                    TargetRequestedByDefault
              (Nothing,   True)  -> TargetBuildable (elabUnitId elab, cname)
                                                    TargetNotRequestedByDefault
              (Just True, False) ->
                error $ "componentAvailableTargetStatus: impossible; cname=" ++ prettyShow cname
      where
        cname      = componentName component
        buildable  = PD.buildable (componentBuildInfo component)
        withinPlan = elabLocalToProject elab
                  || case elabPkgOrComp elab of
                       ElabComponent elabComponent ->
                         compComponentName elabComponent == Just cname
                       ElabPackage _ ->
                         case componentName component of
                           CLibName (LMainLibName) -> True
                           CExeName _ -> True
                           --TODO: what about sub-libs and foreign libs?
                           _          -> False

-- | Merge component targets that overlap each other. Specially when we have
-- multiple targets for the same component and one of them refers to the whole
-- component (rather than a module or file within) then all the other targets
-- for that component are subsumed.
--
-- We also allow for information associated with each component target, and
-- whenever we targets subsume each other we aggregate their associated info.
--
nubComponentTargets :: [(ComponentTarget, a)] -> [(ComponentTarget, NonEmpty a)]
nubComponentTargets =
    concatMap (wholeComponentOverrides . map snd)
  . groupBy ((==)    `on` fst)
  . sortBy  (compare `on` fst)
  . map (\t@((ComponentTarget cname _, _)) -> (cname, t))
  . map compatSubComponentTargets
  where
    -- If we're building the whole component then that the only target all we
    -- need, otherwise we can have several targets within the component.
    wholeComponentOverrides :: [(ComponentTarget,  a )]
                            -> [(ComponentTarget, NonEmpty a)]
    wholeComponentOverrides ts =
      case [ ta | ta@(ComponentTarget _ WholeComponent, _) <- ts ] of
        ((t, x):_) -> 
                let
                    -- Delete tuple (t, x) from original list to avoid duplicates.
                    -- Use 'deleteBy', to avoid additional Class constraint on 'nubComponentTargets'.
                    ts' = deleteBy (\(t1, _) (t2, _) -> t1 == t2) (t, x) ts
                in
                    [ (t, x :| map snd ts') ]
        []    -> [ (t, x :| []) | (t,x) <- ts ]

    -- Not all Cabal Setup.hs versions support sub-component targets, so switch
    -- them over to the whole component
    compatSubComponentTargets :: (ComponentTarget, a) -> (ComponentTarget, a)
    compatSubComponentTargets target@(ComponentTarget cname _subtarget, x)
      | not setupHsSupportsSubComponentTargets
                  = (ComponentTarget cname WholeComponent, x)
      | otherwise = target

    -- Actually the reality is that no current version of Cabal's Setup.hs
    -- build command actually support building specific files or modules.
    setupHsSupportsSubComponentTargets = False
    -- TODO: when that changes, adjust this test, e.g.
    -- | pkgSetupScriptCliVersion >= Version [x,y] []

pkgHasEphemeralBuildTargets :: ElaboratedConfiguredPackage -> Bool
pkgHasEphemeralBuildTargets elab =
    isJust (elabReplTarget elab)
 || (not . null) (elabTestTargets elab)
 || (not . null) (elabBenchTargets elab)
 || (not . null) (elabHaddockTargets elab)
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

-- | How 'pruneInstallPlanToTargets' should interpret the per-package
-- 'ComponentTarget's: as build, repl or haddock targets.
--
data TargetAction = TargetActionConfigure
                  | TargetActionBuild
                  | TargetActionRepl
                  | TargetActionTest
                  | TargetActionBench
                  | TargetActionHaddock

-- | Given a set of per-package\/per-component targets, take the subset of the
-- install plan needed to build those targets. Also, update the package config
-- to specify which optional stanzas to enable, and which targets within each
-- package to build.
--
-- NB: Pruning happens after improvement, which is important because we
-- will prune differently depending on what is already installed (to
-- implement "sticky" test suite enabling behavior).
--
pruneInstallPlanToTargets :: TargetAction
                          -> Map UnitId [ComponentTarget]
                          -> ElaboratedInstallPlan -> ElaboratedInstallPlan
pruneInstallPlanToTargets targetActionType perPkgTargetsMap elaboratedPlan =
    InstallPlan.new (InstallPlan.planIndepGoals elaboratedPlan)
  . Graph.fromDistinctList
    -- We have to do the pruning in two passes
  . pruneInstallPlanPass2
  . pruneInstallPlanPass1
    -- Set the targets that will be the roots for pruning
  . setRootTargets targetActionType perPkgTargetsMap
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

-- | Set the build targets based on the user targets (but not rev deps yet).
-- This is required before we can prune anything.
--
setRootTargets :: TargetAction
               -> Map UnitId [ComponentTarget]
               -> [ElaboratedPlanPackage]
               -> [ElaboratedPlanPackage]
setRootTargets targetAction perPkgTargetsMap =
    assert (not (Map.null perPkgTargetsMap)) $
    assert (all (not . null) (Map.elems perPkgTargetsMap)) $

    map (mapConfiguredPackage setElabBuildTargets)
  where
    -- Set the targets we'll build for this package/component. This is just
    -- based on the root targets from the user, not targets implied by reverse
    -- dependencies. Those comes in the second pass once we know the rev deps.
    --
    setElabBuildTargets elab =
      case (Map.lookup (installedUnitId elab) perPkgTargetsMap,
            targetAction) of
        (Nothing, _)                      -> elab
        (Just tgts,  TargetActionConfigure) -> elab { elabConfigureTargets = tgts }
        (Just tgts,  TargetActionBuild)   -> elab { elabBuildTargets = tgts }
        (Just tgts,  TargetActionTest)    -> elab { elabTestTargets  = tgts }
        (Just tgts,  TargetActionBench)   -> elab { elabBenchTargets  = tgts }
        (Just [tgt], TargetActionRepl)    -> elab { elabReplTarget = Just tgt
                                                  , elabBuildHaddocks = False }
        (Just tgts,  TargetActionHaddock) ->
          foldr setElabHaddockTargets (elab { elabHaddockTargets = tgts
                                            , elabBuildHaddocks = True }) tgts
        (Just _,     TargetActionRepl)    ->
          error "pruneInstallPlanToTargets: multiple repl targets"

    setElabHaddockTargets tgt elab
      | isTestComponentTarget tgt       = elab { elabHaddockTestSuites  = True }
      | isBenchComponentTarget tgt      = elab { elabHaddockBenchmarks  = True }
      | isForeignLibComponentTarget tgt = elab { elabHaddockForeignLibs = True }
      | isExeComponentTarget tgt        = elab { elabHaddockExecutables = True }
      | isSubLibComponentTarget tgt     = elab { elabHaddockInternal    = True }
      | otherwise                       = elab

-- | Assuming we have previously set the root build targets (i.e. the user
-- targets but not rev deps yet), the first pruning pass does two things:
--
-- * A first go at determining which optional stanzas (testsuites, benchmarks)
--   are needed. We have a second go in the next pass.
-- * Take the dependency closure using pruned dependencies. We prune deps that
--   are used only by unneeded optional stanzas. These pruned deps are only
--   used for the dependency closure and are not persisted in this pass.
--
pruneInstallPlanPass1 :: [ElaboratedPlanPackage]
                      -> [ElaboratedPlanPackage]
pruneInstallPlanPass1 pkgs =
    map (mapConfiguredPackage fromPrunedPackage)
        (fromMaybe [] $ Graph.closure graph roots)
  where
    pkgs' = map (mapConfiguredPackage prune) pkgs
    graph = Graph.fromDistinctList pkgs'
    roots = mapMaybe find_root pkgs'

    prune elab = PrunedPackage elab' (pruneOptionalDependencies elab')
      where elab' =
                setDocumentation
              $ addOptionalStanzas elab

    is_root :: PrunedPackage -> Maybe UnitId
    is_root (PrunedPackage elab _) =
      if not $ and [ null (elabConfigureTargets elab)
                   , null (elabBuildTargets elab)
                   , null (elabTestTargets elab)
                   , null (elabBenchTargets elab)
                   , isNothing (elabReplTarget elab)
                   , null (elabHaddockTargets elab)
                   ]
          then Just (installedUnitId elab)
          else Nothing

    find_root (InstallPlan.Configured pkg) = is_root pkg
    -- When using the extra-packages stanza we need to
    -- look at installed packages as well.
    find_root (InstallPlan.Installed pkg)  = is_root pkg
    find_root _ = Nothing

    -- Note [Sticky enabled testsuites]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

    -- Decide whether or not to enable testsuites and benchmarks.
    -- See [Sticky enabled testsuites]
    addOptionalStanzas :: ElaboratedConfiguredPackage -> ElaboratedConfiguredPackage
    addOptionalStanzas elab@ElaboratedConfiguredPackage{ elabPkgOrComp = ElabPackage pkg } =
        elab {
            elabPkgOrComp = ElabPackage (pkg { pkgStanzasEnabled = stanzas })
        }
      where
        stanzas :: OptionalStanzaSet
               -- By default, we enabled all stanzas requested by the user,
               -- as per elabStanzasRequested, done in
               -- 'elaborateSolverToPackage'
        stanzas = pkgStanzasEnabled pkg
               -- optionalStanzasRequiredByTargets has to be done at
               -- prune-time because it depends on 'elabTestTargets'
               -- et al, which is done by 'setRootTargets' at the
               -- beginning of pruning.
               <> optionalStanzasRequiredByTargets elab
               -- optionalStanzasWithDepsAvailable has to be done at
               -- prune-time because it depends on what packages are
               -- installed, which is not known until after improvement
               -- (pruning is done after improvement)
               <> optionalStanzasWithDepsAvailable availablePkgs elab pkg
    addOptionalStanzas elab = elab

    setDocumentation :: ElaboratedConfiguredPackage -> ElaboratedConfiguredPackage
    setDocumentation elab@ElaboratedConfiguredPackage { elabPkgOrComp = ElabComponent comp } =
      elab {
        elabBuildHaddocks =
            elabBuildHaddocks elab && documentationEnabled (compSolverName comp) elab
      }

      where
        documentationEnabled c =
          case c of
            CD.ComponentLib      -> const True
            CD.ComponentSubLib _ -> elabHaddockInternal
            CD.ComponentFLib _   -> elabHaddockForeignLibs
            CD.ComponentExe _    -> elabHaddockExecutables
            CD.ComponentTest _   -> elabHaddockTestSuites
            CD.ComponentBench _  -> elabHaddockBenchmarks
            CD.ComponentSetup    -> const False

    setDocumentation elab = elab

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
        keepNeeded (CD.ComponentTest  _) _ = TestStanzas  `optStanzaSetMember` stanzas
        keepNeeded (CD.ComponentBench _) _ = BenchStanzas `optStanzaSetMember` stanzas
        keepNeeded _                     _ = True
        stanzas = pkgStanzasEnabled pkg

    optionalStanzasRequiredByTargets :: ElaboratedConfiguredPackage
                                     -> OptionalStanzaSet
    optionalStanzasRequiredByTargets pkg =
      optStanzaSetFromList
        [ stanza
        | ComponentTarget cname _ <- elabBuildTargets pkg
                                  ++ elabTestTargets pkg
                                  ++ elabBenchTargets pkg
                                  ++ maybeToList (elabReplTarget pkg)
                                  ++ elabHaddockTargets pkg
        , stanza <- maybeToList $
                    componentOptionalStanza $
                    CD.componentNameToComponent cname
        ]

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
                                 -> OptionalStanzaSet
optionalStanzasWithDepsAvailable availablePkgs elab pkg =
    optStanzaSetFromList
      [ stanza
      | stanza <- optStanzaSetToList (elabStanzasAvailable elab)
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
                    keepNeeded (CD.ComponentTest  _) _ = TestStanzas  `optStanzaSetMember` stanzas
                    keepNeeded (CD.ComponentBench _) _ = BenchStanzas `optStanzaSetMember` stanzas
                    keepNeeded _                     _ = True
                in ElabPackage $ pkg {
                  pkgStanzasEnabled = stanzas,
                  pkgLibDependencies   = CD.filterDeps keepNeeded (pkgLibDependencies pkg),
                  pkgExeDependencies   = CD.filterDeps keepNeeded (pkgExeDependencies pkg),
                  pkgExeDependencyPaths = CD.filterDeps keepNeeded (pkgExeDependencyPaths pkg)
                }
              r@(ElabComponent _) -> r
        }
      where
        libTargetsRequiredForRevDeps =
          [ ComponentTarget (CLibName Cabal.defaultLibName) WholeComponent
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
             , let missingDeps = mapMaybe lookupDep missingDepIds
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
  deriving (Show)


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
-- while in case 4 we can use the internal library API.
--
-- TODO:In case 3 we should fail. We don't know how to talk to
-- newer ./Setup.hs
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

  -- here we should fail.
  | PD.specVersion pkg > cabalSpecLatest  -- one cabal-install is built against
  = SetupNonCustomExternalLib

  | otherwise
  = SetupNonCustomInternalLib
  where
    buildType = PD.buildType pkg


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
        [ Dependency depPkgname anyVersion mainLibSet
        | depPkgname <- legacyCustomSetupPkgs compiler platform ] ++
        [ Dependency cabalPkgname cabalConstraint mainLibSet
        | packageName pkg /= cabalPkgname ]
        where
          -- The Cabal dep is slightly special:
          -- * We omit the dep for the Cabal lib itself, since it bootstraps.
          -- * We constrain it to be < 1.25
          --
          -- Note: we also add a global constraint to require Cabal >= 1.20
          -- for Setup scripts (see use addSetupCabalMinVersionConstraint).
          --
          cabalConstraint   = orLaterVersion (csvToVersion (PD.specVersion pkg))
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
        Just [ Dependency cabalPkgname cabalConstraint mainLibSet
             , Dependency basePkgname  anyVersion mainLibSet]
        where
          cabalConstraint = orLaterVersion (csvToVersion (PD.specVersion pkg))

      -- The internal setup wrapper method has no deps at all.
      SetupNonCustomInternalLib -> Just []

      -- This case gets ruled out by the caller, planPackages, see the note
      -- above in the SetupCustomImplicitDeps case.
      SetupCustomExplicitDeps ->
        error $ "defaultSetupDeps: called for a package with explicit "
             ++ "setup deps: " ++ prettyShow (packageId pkg)
  where
    -- we require one less
    --
    -- This maps e.g. CabalSpecV3_0 to mkVersion [2,5]
    csvToVersion :: CabalSpecVersion -> Version
    csvToVersion = mkVersion . cabalSpecMinimumLibraryVersion

-- | Work out which version of the Cabal we will be using to talk to the
-- Setup.hs interface for this package.
--
-- This depends somewhat on the 'SetupScriptStyle' but most cases are a result
-- of what the solver picked for us, based on the explicit setup deps or the
-- ones added implicitly by 'defaultSetupDeps'.
--
packageSetupScriptSpecVersion :: SetupScriptStyle
                              -> PD.PackageDescription
                              -> Graph.Graph NonSetupLibDepSolverPlanPackage
                              -> ComponentDeps [SolverId]
                              -> Version

-- We're going to be using the internal Cabal library, so the spec version of
-- that is simply the version of the Cabal library that cabal-install has been
-- built with.
packageSetupScriptSpecVersion SetupNonCustomInternalLib _ _ _ =
    cabalVersion

-- If we happen to be building the Cabal lib itself then because that
-- bootstraps itself then we use the version of the lib we're building.
packageSetupScriptSpecVersion SetupCustomImplicitDeps pkg _ _
  | packageName pkg == cabalPkgname
  = packageVersion pkg

-- In all other cases we have a look at what version of the Cabal lib the
-- solver picked. Or if it didn't depend on Cabal at all (which is very rare)
-- then we look at the .cabal file to see what spec version it declares.
packageSetupScriptSpecVersion _ pkg libDepGraph deps =
    case find ((cabalPkgname ==) . packageName) setupLibDeps of
      Just dep -> packageVersion dep
      Nothing  -> mkVersion (cabalSpecMinimumLibraryVersion (PD.specVersion pkg))
  where
    setupLibDeps = map packageId $ fromMaybe [] $
                   Graph.closure libDepGraph (CD.setupDeps deps)


cabalPkgname, basePkgname :: PackageName
cabalPkgname = mkPackageName "Cabal"
basePkgname  = mkPackageName "base"


legacyCustomSetupPkgs :: Compiler -> Platform -> [PackageName]
legacyCustomSetupPkgs compiler (Platform _ os) =
    map mkPackageName $
        [ "array", "base", "binary", "bytestring", "containers"
        , "deepseq", "directory", "filepath", "pretty"
        , "process", "time", "transformers" ]
     ++ [ "Win32" | os == Windows ]
     ++ [ "unix"  | os /= Windows ]
     ++ [ "ghc-prim"         | isGHC ]
     ++ [ "template-haskell" | isGHC ]
     ++ [ "old-time" | notGHC710 ]
  where
    isGHC = compilerCompatFlavor GHC compiler
    notGHC710 = case compilerCompatVersion GHC compiler of
        Nothing -> False
        Just v  -> v <= mkVersion [7,9]

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
                     -> ElaboratedInstallPlan
                     -> ElaboratedSharedConfig
                     -> DistDirLayout
                     -> FilePath
                     -> FilePath
                     -> Bool
                     -> Lock
                     -> SetupScriptOptions
-- TODO: Fix this so custom is a separate component.  Custom can ALWAYS
-- be a separate component!!!
setupHsScriptOptions (ReadyPackage elab@ElaboratedConfiguredPackage{..})
                     plan ElaboratedSharedConfig{..} distdir srcdir builddir
                     isParallelBuild cacheLock =
    SetupScriptOptions {
      useCabalVersion          = thisVersion elabSetupScriptCliVersion,
      useCabalSpecVersion      = Just elabSetupScriptCliVersion,
      useCompiler              = Just pkgConfigCompiler,
      usePlatform              = Just pkgConfigPlatform,
      usePackageDB             = elabSetupPackageDBStack,
      usePackageIndex          = Nothing,
      useDependencies          = [ (uid, srcid)
                                 | ConfiguredId srcid (Just (CLibName LMainLibName)) uid
                                 <- elabSetupDependencies elab ],
      useDependenciesExclusive = True,
      useVersionMacros         = elabSetupScriptStyle == SetupCustomExplicitDeps,
      useProgramDb             = pkgConfigCompilerProgs,
      useDistPref              = builddir,
      useLoggingHandle         = Nothing, -- this gets set later
      useWorkingDir            = Just srcdir,
      useExtraPathEnv          = elabExeDependencyPaths elab,
      useExtraEnvOverrides     = dataDirsEnvironmentForPlan distdir plan,
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

storePackageInstallDirs :: StoreDirLayout
                        -> CompilerId
                        -> InstalledPackageId
                        -> InstallDirs.InstallDirs FilePath
storePackageInstallDirs storeDirLayout compid ipkgid =
  storePackageInstallDirs' storeDirLayout compid $ newSimpleUnitId ipkgid

storePackageInstallDirs' :: StoreDirLayout
                         -> CompilerId
                         -> UnitId
                         -> InstallDirs.InstallDirs FilePath
storePackageInstallDirs' StoreDirLayout{ storePackageDirectory
                                       , storeDirectory }
                         compid unitid =
    InstallDirs.InstallDirs {..}
  where
    store        = storeDirectory compid
    prefix       = storePackageDirectory compid unitid
    bindir       = prefix </> "bin"
    libdir       = prefix </> "lib"
    libsubdir    = ""
    -- Note: on macOS, we place libraries into
    --       @store/lib@ to work around the load
    --       command size limit of macOSs mach-o linker.
    --       See also @PackageHash.hashedInstalledPackageIdVeryShort@
    dynlibdir    | buildOS == OSX = store </> "lib"
                 | otherwise      = libdir
    flibdir      = libdir
    libexecdir   = prefix </> "libexec"
    libexecsubdir= ""
    includedir   = libdir </> "include"
    datadir      = prefix </> "share"
    datasubdir   = ""
    docdir       = datadir </> "doc"
    mandir       = datadir </> "man"
    htmldir      = docdir  </> "html"
    haddockdir   = htmldir
    sysconfdir   = prefix </> "etc"



computeInstallDirs :: StoreDirLayout
                   -> InstallDirs.InstallDirTemplates
                   -> ElaboratedSharedConfig
                   -> ElaboratedConfiguredPackage
                   -> InstallDirs.InstallDirs FilePath
computeInstallDirs storeDirLayout defaultInstallDirs elaboratedShared elab
  | elabBuildStyle elab == BuildInplaceOnly
  -- use the ordinary default install dirs
  = (InstallDirs.absoluteInstallDirs
       (elabPkgSourceId elab)
       (elabUnitId elab)
       (compilerInfo (pkgConfigCompiler elaboratedShared))
       InstallDirs.NoCopyDest
       (pkgConfigPlatform elaboratedShared)
       defaultInstallDirs) {

      -- absoluteInstallDirs sets these as 'undefined' but we have
      -- to use them as "Setup.hs configure" args
      InstallDirs.libsubdir  = "",
      InstallDirs.libexecsubdir  = "",
      InstallDirs.datasubdir = ""
    }

  | otherwise
  -- use special simplified install dirs
  = storePackageInstallDirs'
      storeDirLayout
      (compilerId (pkgConfigCompiler elaboratedShared))
      (elabUnitId elab)


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
                                  ElabPackage pkg -> toFlag (prettyShow (pkgInstalledId pkg))
                                  ElabComponent _ -> mempty
    configCID                 = case elabPkgOrComp of
                                  ElabPackage _ -> mempty
                                  ElabComponent _ -> toFlag elabComponentId

    configProgramPaths        = Map.toList elabProgramPaths
    configProgramArgs
        | {- elabSetupScriptCliVersion < mkVersion [1,24,3] -} True
          -- workaround for <https://github.com/haskell/cabal/issues/4010>
          --
          -- It turns out, that even with Cabal 2.0, there's still cases such as e.g.
          -- custom Setup.hs scripts calling out to GHC even when going via
          -- @runProgram ghcProgram@, as e.g. happy does in its
          -- <http://hackage.haskell.org/package/happy-1.19.5/src/Setup.lhs>
          -- (see also <https://github.com/haskell/cabal/pull/4433#issuecomment-299396099>)
          --
          -- So for now, let's pass the rather harmless and idempotent
          -- `-hide-all-packages` flag to all invocations (which has
          -- the benefit that every GHC invocation starts with a
          -- conistently well-defined clean slate) until we find a
          -- better way.
                              = Map.toList $
                                Map.insertWith (++) "ghc" ["-hide-all-packages"]
                                               elabProgramArgs
        | otherwise           = Map.toList elabProgramArgs
    configProgramPathExtra    = toNubList elabProgramPathExtra
    configHcFlavor            = toFlag (compilerFlavor pkgConfigCompiler)
    configHcPath              = mempty -- we use configProgramPaths instead
    configHcPkg               = mempty -- we use configProgramPaths instead

    configVanillaLib          = toFlag elabVanillaLib
    configSharedLib           = toFlag elabSharedLib
    configStaticLib           = toFlag elabStaticLib

    configDynExe              = toFlag elabDynExe
    configFullyStaticExe      = toFlag elabFullyStaticExe
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
    configSplitSections       = toFlag elabSplitSections
    configSplitObjs           = toFlag elabSplitObjs
    configStripExes           = toFlag elabStripExes
    configStripLibs           = toFlag elabStripLibs
    configDebugInfo           = toFlag elabDebugInfo

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
    configDependencies        = [ GivenComponent
                                    (packageName srcid)
                                    ln
                                    cid
                                | ConfiguredId srcid mb_cn cid <- elabLibDependencies elab
                                , let ln = case mb_cn
                                           of Just (CLibName lname) -> lname
                                              Just _ -> error "non-library dependency"
                                              Nothing -> LMainLibName
                                ]
    configConstraints         =
        case elabPkgOrComp of
            ElabPackage _ ->
                [ thisPackageVersionConstraint srcid
                | ConfiguredId srcid _ _uid <- elabLibDependencies elab ]
            ElabComponent _ -> []


    -- explicitly clear, then our package db stack
    -- TODO: [required eventually] have to do this differently for older Cabal versions
    configPackageDBs          = Nothing : map Just elabBuildPackageDBStack

    configTests               = case elabPkgOrComp of
                                    ElabPackage pkg -> toFlag (TestStanzas  `optStanzaSetMember` pkgStanzasEnabled pkg)
                                    ElabComponent _ -> mempty
    configBenchmarks          = case elabPkgOrComp of
                                    ElabPackage pkg -> toFlag (BenchStanzas `optStanzaSetMember` pkgStanzasEnabled pkg)
                                    ElabComponent _ -> mempty

    configExactConfiguration  = toFlag True
    configFlagError           = mempty --TODO: [research required] appears not to be implemented
    configRelocatable         = mempty --TODO: [research required] ???
    configScratchDir          = mempty -- never use
    configUserInstall         = mempty -- don't rely on defaults
    configPrograms_           = mempty -- never use, shouldn't exist
    configUseResponseFiles    = mempty
    configAllowDependingOnPrivateLibs = Flag $ not $ libraryVisibilitySupported pkgConfigCompiler

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
      buildArgs         = mempty, -- unused, passed via args not flags
      buildCabalFilePath= mempty
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
setupHsTestFlags (ElaboratedConfiguredPackage{..}) _ verbosity builddir = Cabal.TestFlags
    { testDistPref    = toFlag builddir
    , testVerbosity   = toFlag verbosity
    , testMachineLog  = maybe mempty toFlag elabTestMachineLog
    , testHumanLog    = maybe mempty toFlag elabTestHumanLog
    , testShowDetails = maybe (Flag Cabal.Always) toFlag elabTestShowDetails
    , testKeepTix     = toFlag elabTestKeepTix
    , testWrapper     = maybe mempty toFlag elabTestWrapper
    , testFailWhenNoTestSuites = toFlag elabTestFailWhenNoTestSuites
    , testOptions     = elabTestTestOptions
    }

setupHsTestArgs :: ElaboratedConfiguredPackage -> [String]
-- TODO: Does the issue #3335 affects test as well
setupHsTestArgs elab =
    mapMaybe (showTestComponentTarget (packageId elab)) (elabTestTargets elab)


setupHsBenchFlags :: ElaboratedConfiguredPackage
                  -> ElaboratedSharedConfig
                  -> Verbosity
                  -> FilePath
                  -> Cabal.BenchmarkFlags
setupHsBenchFlags (ElaboratedConfiguredPackage{..}) _ verbosity builddir = Cabal.BenchmarkFlags
    { benchmarkDistPref  = toFlag builddir
    , benchmarkVerbosity = toFlag verbosity
    , benchmarkOptions   = elabBenchmarkOptions
    }

setupHsBenchArgs :: ElaboratedConfiguredPackage -> [String]
setupHsBenchArgs elab =
    mapMaybe (showBenchComponentTarget (packageId elab)) (elabBenchTargets elab)


setupHsReplFlags :: ElaboratedConfiguredPackage
                 -> ElaboratedSharedConfig
                 -> Verbosity
                 -> FilePath
                 -> Cabal.ReplFlags
setupHsReplFlags _ sharedConfig verbosity builddir =
    Cabal.ReplFlags {
      replProgramPaths = mempty, --unused, set at configure time
      replProgramArgs  = mempty, --unused, set at configure time
      replVerbosity    = toFlag verbosity,
      replDistPref     = toFlag builddir,
      replReload       = mempty, --only used as callback from repl
      replReplOptions  = pkgConfigReplOptions sharedConfig       --runtime override for repl flags
    }


setupHsReplArgs :: ElaboratedConfiguredPackage -> [String]
setupHsReplArgs elab =
    maybe [] (\t -> [showComponentTarget (packageId elab) t]) (elabReplTarget elab)
    --TODO: should be able to give multiple modules in one component


setupHsCopyFlags :: ElaboratedConfiguredPackage
                 -> ElaboratedSharedConfig
                 -> Verbosity
                 -> FilePath
                 -> FilePath
                 -> Cabal.CopyFlags
setupHsCopyFlags _ _ verbosity builddir destdir =
    Cabal.CopyFlags {
      copyArgs      = [], -- TODO: could use this to only copy what we enabled
      copyDest      = toFlag (InstallDirs.CopyTo destdir),
      copyDistPref  = toFlag builddir,
      copyVerbosity = toFlag verbosity,
      copyCabalFilePath = mempty
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
      regVerbosity   = toFlag verbosity,
      regCabalFilePath = mempty
    }

setupHsHaddockFlags :: ElaboratedConfiguredPackage
                    -> ElaboratedSharedConfig
                    -> Verbosity
                    -> FilePath
                    -> Cabal.HaddockFlags
setupHsHaddockFlags (ElaboratedConfiguredPackage{..}) _ verbosity builddir =
    Cabal.HaddockFlags {
      haddockProgramPaths  = mempty, --unused, set at configure time
      haddockProgramArgs   = mempty, --unused, set at configure time
      haddockHoogle        = toFlag elabHaddockHoogle,
      haddockHtml          = toFlag elabHaddockHtml,
      haddockHtmlLocation  = maybe mempty toFlag elabHaddockHtmlLocation,
      haddockForHackage    = toFlag elabHaddockForHackage,
      haddockForeignLibs   = toFlag elabHaddockForeignLibs,
      haddockExecutables   = toFlag elabHaddockExecutables,
      haddockTestSuites    = toFlag elabHaddockTestSuites,
      haddockBenchmarks    = toFlag elabHaddockBenchmarks,
      haddockInternal      = toFlag elabHaddockInternal,
      haddockCss           = maybe mempty toFlag elabHaddockCss,
      haddockLinkedSource  = toFlag elabHaddockLinkedSource,
      haddockQuickJump     = toFlag elabHaddockQuickJump,
      haddockHscolourCss   = maybe mempty toFlag elabHaddockHscolourCss,
      haddockContents      = maybe mempty toFlag elabHaddockContents,
      haddockDistPref      = toFlag builddir,
      haddockKeepTempFiles = mempty, --TODO: from build settings
      haddockVerbosity     = toFlag verbosity,
      haddockCabalFilePath = mempty,
      haddockArgs          = mempty
    }

setupHsHaddockArgs :: ElaboratedConfiguredPackage -> [String]
-- TODO: Does the issue #3335 affects test as well
setupHsHaddockArgs elab =
  map (showComponentTarget (packageId elab)) (elabHaddockTargets elab)

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
            Set.fromList (map confInstId (compLibDependencies comp
                                       ++ compExeDependencies comp)),
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
         ++ prettyShow (packageId pkg)

packageHashConfigInputs :: ElaboratedSharedConfig
                        -> ElaboratedConfiguredPackage
                        -> PackageHashConfigInputs
packageHashConfigInputs shared@ElaboratedSharedConfig{..} pkg =
    PackageHashConfigInputs {
      pkgHashCompilerId          = compilerId pkgConfigCompiler,
      pkgHashPlatform            = pkgConfigPlatform,
      pkgHashFlagAssignment      = elabFlagAssignment,
      pkgHashConfigureScriptArgs = elabConfigureScriptArgs,
      pkgHashVanillaLib          = elabVanillaLib,
      pkgHashSharedLib           = elabSharedLib,
      pkgHashDynExe              = elabDynExe,
      pkgHashFullyStaticExe      = elabFullyStaticExe,
      pkgHashGHCiLib             = elabGHCiLib,
      pkgHashProfLib             = elabProfLib,
      pkgHashProfExe             = elabProfExe,
      pkgHashProfLibDetail       = elabProfLibDetail,
      pkgHashProfExeDetail       = elabProfExeDetail,
      pkgHashCoverage            = elabCoverage,
      pkgHashOptimization        = elabOptimization,
      pkgHashSplitSections       = elabSplitSections,
      pkgHashSplitObjs           = elabSplitObjs,
      pkgHashStripLibs           = elabStripLibs,
      pkgHashStripExes           = elabStripExes,
      pkgHashDebugInfo           = elabDebugInfo,
      pkgHashProgramArgs         = elabProgramArgs,
      pkgHashExtraLibDirs        = elabExtraLibDirs,
      pkgHashExtraFrameworkDirs  = elabExtraFrameworkDirs,
      pkgHashExtraIncludeDirs    = elabExtraIncludeDirs,
      pkgHashProgPrefix          = elabProgPrefix,
      pkgHashProgSuffix          = elabProgSuffix,

      pkgHashDocumentation       = elabBuildHaddocks,
      pkgHashHaddockHoogle       = elabHaddockHoogle,
      pkgHashHaddockHtml         = elabHaddockHtml,
      pkgHashHaddockHtmlLocation = elabHaddockHtmlLocation,
      pkgHashHaddockForeignLibs  = elabHaddockForeignLibs,
      pkgHashHaddockExecutables  = elabHaddockExecutables,
      pkgHashHaddockTestSuites   = elabHaddockTestSuites,
      pkgHashHaddockBenchmarks   = elabHaddockBenchmarks,
      pkgHashHaddockInternal     = elabHaddockInternal,
      pkgHashHaddockCss          = elabHaddockCss,
      pkgHashHaddockLinkedSource = elabHaddockLinkedSource,
      pkgHashHaddockQuickJump    = elabHaddockQuickJump,
      pkgHashHaddockContents     = elabHaddockContents
    }
  where
    ElaboratedConfiguredPackage{..} = normaliseConfiguredPackage shared pkg

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


-- Path construction
------

-- | The path to the directory that contains a specific executable.
-- NB: For inplace NOT InstallPaths.bindir installDirs; for an
-- inplace build those values are utter nonsense.  So we
-- have to guess where the directory is going to be.
-- Fortunately this is "stable" part of Cabal API.
-- But the way we get the build directory is A HORRIBLE
-- HACK.
binDirectoryFor
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> FilePath
  -> FilePath
binDirectoryFor layout config package exe = case elabBuildStyle package of
  BuildAndInstall -> installedBinDirectory package
  BuildInplaceOnly -> inplaceBinRoot layout config package </> exe

-- package has been built and installed.
installedBinDirectory :: ElaboratedConfiguredPackage -> FilePath
installedBinDirectory = InstallDirs.bindir . elabInstallDirs

-- | The path to the @build@ directory for an inplace build.
inplaceBinRoot
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> FilePath
inplaceBinRoot layout config package
  =  distBuildDirectory layout (elabDistDirParams config package)
 </> "build"
