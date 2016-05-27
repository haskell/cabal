{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, RankNTypes #-}

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

    --TODO: [code cleanup] these types should live with execution, not with
    --      plan definition. Need to better separate InstallPlan definition.
    GenericBuildResult(..),
    BuildResult,
    BuildSuccess(..),
    BuildFailure(..),
    DocsResult(..),
    TestsResult(..),

    -- * Producing the elaborated install plan
    rebuildInstallPlan,

    -- * Build targets
    PackageTarget(..),
    ComponentTarget(..),
    SubComponentTarget(..),
    showComponentTarget,

    -- * Selecting a plan subset
    pruneInstallPlanToTargets,

    -- * Utils required for building
    pkgHasEphemeralBuildTargets,
    pkgBuildTargetWholeComponents,

    -- * Setup.hs CLI flags for building
    setupHsScriptOptions,
    setupHsConfigureFlags,
    setupHsBuildFlags,
    setupHsBuildArgs,
    setupHsReplFlags,
    setupHsReplArgs,
    setupHsCopyFlags,
    setupHsRegisterFlags,
    setupHsHaddockFlags,

    packageHashInputs,

    -- TODO: [code cleanup] utils that should live in some shared place?
    createPackageDBIfMissing
  ) where

import           Distribution.Client.ProjectPlanning.Types
import           Distribution.Client.PackageHash
import           Distribution.Client.RebuildMonad
import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanOutput

import           Distribution.Client.Types
                   hiding ( BuildResult, BuildSuccess(..), BuildFailure(..)
                          , DocsResult(..), TestsResult(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
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

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import           Distribution.Solver.Types.ConstraintSource
import           Distribution.Solver.Types.LabeledPackageConstraint
import           Distribution.Solver.Types.OptionalStanza
import           Distribution.Solver.Types.PackageFixedDeps
import           Distribution.Solver.Types.PkgConfigDb
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.SourcePackage

import           Distribution.Package hiding
  (InstalledPackageId, installedPackageId)
import           Distribution.System
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import           Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
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
import           Distribution.Simple.LocalBuildInfo (ComponentName(..))
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.BuildTarget as Cabal

import           Distribution.Simple.Utils hiding (matchFileGlob)
import           Distribution.Version
import           Distribution.Verbosity
import           Distribution.Text

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import qualified Data.Tree  as Tree
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad
import           Control.Monad.State as State
import           Control.Exception
import           Data.List
import           Data.Maybe
import           Data.Either
import           Data.Monoid
import           Data.Function
import           System.FilePath
import           System.Directory (doesDirectoryExist)

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
sanityCheckElaboratedConfiguredPackage :: ElaboratedSharedConfig
                                       -> ElaboratedConfiguredPackage
                                       -> a
                                       -> a
sanityCheckElaboratedConfiguredPackage sharedConfig
                                       pkg@ElaboratedConfiguredPackage{..}
                                       ret =

    -- we should only have enabled stanzas that actually can be built
    -- (according to the solver)
    assert (pkgStanzasEnabled `Set.isSubsetOf` pkgStanzasAvailable)

    -- the stanzas that the user explicitly requested should be
    -- enabled (by the previous test, they are also available)
  . assert (Map.keysSet (Map.filter id pkgStanzasRequested)
                `Set.isSubsetOf` pkgStanzasEnabled)

    -- the stanzas explicitly disabled should not be available
  . assert (Set.null (Map.keysSet (Map.filter not pkgStanzasRequested)
                `Set.intersection` pkgStanzasAvailable))

    -- either a package is being built inplace, or the
    -- 'installedPackageId' we assigned is consistent with
    -- the 'hashedInstalledPackageId' we would compute from
    -- the elaborated configured package
  . assert (pkgBuildStyle == BuildInplaceOnly ||
     installedPackageId pkg == hashedInstalledPackageId
                                 (packageHashInputs sharedConfig pkg))

    -- either a package is built inplace, or we are not attempting to
    -- build any test suites or benchmarks (we never build these
    -- for remote packages!)
  . assert (pkgBuildStyle == BuildInplaceOnly ||
     Set.null pkgStanzasAvailable)

  $ ret


------------------------------------------------------------------------------
-- * Deciding what to do: making an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------

rebuildInstallPlan :: Verbosity
                   -> FilePath -> DistDirLayout -> CabalDirLayout
                   -> ProjectConfig
                   -> IO ( ElaboratedInstallPlan
                         , ElaboratedSharedConfig
                         , ProjectConfig )
rebuildInstallPlan verbosity
                   projectRootDir
                   distDirLayout@DistDirLayout {
                     distDirectory,
                     distProjectCacheFile,
                     distProjectCacheDirectory
                   }
                   cabalDirLayout@CabalDirLayout {
                     cabalPackageCacheDirectory,
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
          solverPlan    <- phaseRunSolver         projectConfigTransient
                                                  compilerEtc localPackages
          (elaboratedPlan,
           elaboratedShared) <- phaseElaboratePlan projectConfigTransient
                                                   compilerEtc
                                                   solverPlan localPackages
          phaseMaintainPlanOutputs elaboratedPlan elaboratedShared

          return (elaboratedPlan, elaboratedShared,
                  projectConfig)

      -- The improved plan changes each time we install something, whereas
      -- the underlying elaborated plan only changes when input config
      -- changes, so it's worth caching them separately.
      improvedPlan <- phaseImprovePlan elaboratedPlan elaboratedShared
      return (improvedPlan, elaboratedShared, projectConfig)

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
        createDirectoryIfMissingVerbose verbosity False distDirectory
        createDirectoryIfMissingVerbose verbosity False distProjectCacheDirectory

      projectConfig <- readProjectConfig verbosity projectRootDir

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
                   -> Rebuild SolverInstallPlan
    phaseRunSolver projectConfig@ProjectConfig {
                     projectConfigShared,
                     projectConfigBuildOnly
                   }
                   (compiler, platform, progdb)
                   localPackages =
        rerunIfChanged verbosity fileMonitorSolverPlan
                       (solverSettings, cabalPackageCacheDirectory,
                        localPackages, localPackagesEnabledStanzas,
                        compiler, platform, programsDbSignature progdb) $ do

          installedPkgIndex <- getInstalledPackages verbosity
                                                    compiler progdb platform
                                                    corePackageDbs
          sourcePkgDb       <- getSourcePackages    verbosity withRepoCtx
          pkgConfigDB       <- getPkgConfigDb      verbosity progdb

          --TODO: [code cleanup] it'd be better if the Compiler contained the
          -- ConfiguredPrograms that it needs, rather than relying on the progdb
          -- since we don't need to depend on all the programs here, just the
          -- ones relevant for the compiler.

          liftIO $ do
            solver <- chooseSolver verbosity
                                   (solverSettingSolver solverSettings)
                                   (compilerInfo compiler)

            notice verbosity "Resolving dependencies..."
            foldProgress logMsg die return $
              planPackages compiler platform solver solverSettings
                           installedPkgIndex sourcePkgDb pkgConfigDB
                           localPackages localPackagesEnabledStanzas
      where
        corePackageDbs = [GlobalPackageDB]
        withRepoCtx    = projectConfigWithSolverRepoContext verbosity
                           cabalPackageCacheDirectory
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
                       (compiler, platform, progdb)
                       solverPlan localPackages = do

        liftIO $ debug verbosity "Elaborating the install plan..."

        sourcePackageHashes <-
          rerunIfChanged verbosity fileMonitorSourceHashes
                         (packageLocationsSignature solverPlan) $
            getPackageSourceHashes verbosity withRepoCtx solverPlan

        defaultInstallDirs <- liftIO $ userInstallDirTemplates compiler
        return $
          elaborateInstallPlan
            platform compiler progdb
            distDirLayout
            cabalDirLayout
            solverPlan
            localPackages
            sourcePackageHashes
            defaultInstallDirs
            projectConfigShared
            projectConfigLocalPackages
            (getMapMappend projectConfigSpecificPackage)
      where
        withRepoCtx = projectConfigWithSolverRepoContext verbosity
                        cabalPackageCacheDirectory
                        projectConfigShared
                        projectConfigBuildOnly


    -- Update the files we maintain that reflect our current build environment.
    -- In particular we maintain a JSON representation of the elaborated
    -- install plan.
    --
    -- TODO: [required eventually] maintain the ghc environment file reflecting
    -- the libs available. This will need to be after plan improvement phase.
    --
    phaseMaintainPlanOutputs :: ElaboratedInstallPlan
                             -> ElaboratedSharedConfig
                             -> Rebuild ()
    phaseMaintainPlanOutputs elaboratedPlan elaboratedShared = do
        liftIO $ debug verbosity "Updating plan.json"
        liftIO $ writePlanExternalRepresentation
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
        recreateDirectory verbosity True storeDirectory
        storePkgIndex <- getPackageDBContents verbosity
                                              compiler progdb platform
                                              storePackageDb
        let improvedPlan = improveInstallPlanWithPreExistingPackages
                             storePkgIndex
                             elaboratedPlan
        return improvedPlan

      where
        storeDirectory  = cabalStoreDirectory (compilerId compiler)
        storePackageDb  = cabalStorePackageDB (compilerId compiler)
        ElaboratedSharedConfig {
          pkgConfigPlatform      = platform,
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
programsDbSignature :: ProgramDb -> [ConfiguredProgram]
programsDbSignature progdb =
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
      createPackageDBIfMissing verbosity compiler
                               progdb [packagedb]
      Cabal.getPackageDBContents verbosity compiler
                                 packagedb progdb

getSourcePackages :: Verbosity -> (forall a. (RepoContext -> IO a) -> IO a)
                  -> Rebuild SourcePackageDb
getSourcePackages verbosity withRepoCtx = do
    (sourcePkgDb, repos) <-
      liftIO $
        withRepoCtx $ \repoctx -> do
          sourcePkgDb <- IndexUtils.getSourcePackages verbosity repoctx
          return (sourcePkgDb, repoContextRepos repoctx)

    monitorFiles . map monitorFile
                 . IndexUtils.getSourcePackagesMonitorFiles
                 $ repos
    return sourcePkgDb

createPackageDBIfMissing :: Verbosity -> Compiler -> ProgramDb
                         -> PackageDBStack -> IO ()
createPackageDBIfMissing verbosity compiler progdb packageDbs =
  case reverse packageDbs of
    SpecificPackageDB dbPath : _ -> do
      exists <- liftIO $ Cabal.doesPackageDBExist dbPath
      unless exists $ do
        createDirectoryIfMissingVerbose verbosity False (takeDirectory dbPath)
        Cabal.createPackageDB verbosity compiler progdb False dbPath
    _ -> return ()


getPkgConfigDb :: Verbosity -> ProgramDb -> Rebuild PkgConfigDb
getPkgConfigDb verbosity progdb = do
    dirs <- liftIO $ getPkgConfigDbDirs verbosity progdb
    -- Just monitor the dirs so we'll notice new .pc files.
    -- Alternatively we could monitor all the .pc files too.
    forM_ dirs $ \dir -> do
        dirExists <- liftIO $ doesDirectoryExist dir
        -- TODO: turn this into a utility function
        monitorFiles [if dirExists
                        then monitorDirectory dir
                        else monitorNonExistentDirectory dir]

    liftIO $ readPkgConfigDb verbosity progdb


recreateDirectory :: Verbosity -> Bool -> FilePath -> Rebuild ()
recreateDirectory verbosity createParents dir = do
    liftIO $ createDirectoryIfMissingVerbose verbosity createParents dir
    monitorFiles [monitorDirectoryExistence dir]


-- | Select the config values to monitor for changes package source hashes.
packageLocationsSignature :: SolverInstallPlan
                          -> [(PackageId, PackageLocation (Maybe FilePath))]
packageLocationsSignature solverPlan =
    [ (packageId pkg, packageSource pkg)
    | InstallPlan.Configured (SolverPackage { solverPkgSource = pkg})
        <- InstallPlan.toList solverPlan
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
          | InstallPlan.Configured (SolverPackage { solverPkgSource = pkg})
              <- InstallPlan.toList solverPlan ]

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

planPackages :: Compiler
             -> Platform
             -> Solver -> SolverSettings
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> PkgConfigDb
             -> [UnresolvedSourcePackage]
             -> Map PackageName (Map OptionalStanza Bool)
             -> Progress String String SolverInstallPlan
planPackages comp platform solver SolverSettings{..}
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

        --TODO: [required eventually] should only be configurable for custom installs
   -- . setAvoidReinstalls solverSettingAvoidReinstalls

        --TODO: [required eventually] should only be configurable for custom installs
   -- . setShadowPkgs solverSettingShadowPkgs

      . setStrongFlags solverSettingStrongFlags

        --TODO: [required eventually] decide if we need to prefer installed for
        -- global packages, or prefer latest even for global packages. Perhaps
        -- should be configurable but with a different name than "upgrade-dependencies".
      . setPreferenceDefault PreferLatestForSelected
                           {-(if solverSettingUpgradeDeps
                                then PreferAllLatest
                                else PreferLatestForSelected)-}

      . removeUpperBounds solverSettingAllowNewer

      . addDefaultSetupDependencies (defaultSetupDeps comp platform
                                   . PD.packageDescription
                                   . packageDescription)

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
              (PackageConstraintStanzas pkgname stanzas)
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
              (PackageConstraintFlags pkgname flags)
              ConstraintSourceConfigFlagOrTarget
          | (pkgname, flags) <- Map.toList solverSettingFlagAssignments ]

      . addConstraints
          --TODO: [nice to have] we have user-supplied flags for unspecified
          -- local packages (as well as specific per-package flags). For the
          -- former we just apply all these flags to all local targets which
          -- is silly. We should check if the flags are appropriate.
          [ LabeledPackageConstraint
              (PackageConstraintFlags pkgname flags)
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
  :: Platform -> Compiler -> ProgramDb
  -> DistDirLayout
  -> CabalDirLayout
  -> SolverInstallPlan
  -> [SourcePackage loc]
  -> Map PackageId PackageSourceHash
  -> InstallDirs.InstallDirTemplates
  -> ProjectConfigShared
  -> PackageConfig
  -> Map PackageName PackageConfig
  -> (ElaboratedInstallPlan, ElaboratedSharedConfig)
elaborateInstallPlan platform compiler compilerprogdb
                     DistDirLayout{..}
                     cabalDirLayout@CabalDirLayout{cabalStorePackageDB}
                     solverPlan localPackages
                     sourcePackageHashes
                     defaultInstallDirs
                     _sharedPackageConfig
                     localPackagesConfig
                     perPackageConfig =
    (elaboratedInstallPlan, elaboratedSharedConfig)
  where
    elaboratedSharedConfig =
      ElaboratedSharedConfig {
        pkgConfigPlatform      = platform,
        pkgConfigCompiler      = compiler,
        pkgConfigCompilerProgs = compilerprogdb
      }

    elaboratedInstallPlan =
      flip InstallPlan.mapPreservingGraph solverPlan $ \mapDep planpkg ->
        case planpkg of
          InstallPlan.PreExisting pkg ->
            InstallPlan.PreExisting pkg

          InstallPlan.Configured  pkg ->
            InstallPlan.Configured
              (elaborateSolverPackage mapDep pkg)

          _ -> error "elaborateInstallPlan: unexpected package state"

    elaborateSolverPackage :: (UnitId -> UnitId)
                           -> SolverPackage UnresolvedPkgLoc
                           -> ElaboratedConfiguredPackage
    elaborateSolverPackage
        mapDep
        pkg@(SolverPackage (SourcePackage pkgid gdesc srcloc descOverride)
                           flags stanzas deps0) =
        elaboratedPackage
      where
        -- Knot tying: the final elaboratedPackage includes the
        -- pkgInstalledId, which is calculated by hashing many
        -- of the other fields of the elaboratedPackage.
        --
        elaboratedPackage = ElaboratedConfiguredPackage {..}

        deps = fmap (map elaborateSolverId) deps0

        elaborateSolverId sid =
            ConfiguredId {
                confSrcId  = packageId sid,
                -- Update the 'UnitId' to the final nix-style hashed ID
                confInstId = mapDep (installedPackageId sid)
            }

        pkgInstalledId
          | shouldBuildInplaceOnly pkg
          = mkUnitId (display pkgid ++ "-inplace")

          | otherwise
          = assert (isJust pkgSourceHash) $
            hashedInstalledPackageId
              (packageHashInputs
                elaboratedSharedConfig
                elaboratedPackage)  -- recursive use of elaboratedPackage

          | otherwise
          = error $ "elaborateInstallPlan: non-inplace package "
                 ++ " is missing a source hash: " ++ display pkgid

        -- All the other fields of the ElaboratedConfiguredPackage
        --
        pkgSourceId         = pkgid
        pkgDescription      = let Right (desc, _) =
                                    PD.finalizePackageDescription
                                    flags (const True)
                                    platform (compilerInfo compiler)
                                    [] gdesc
                               in desc
        pkgFlagAssignment   = flags
        pkgFlagDefaults     = [ (Cabal.flagName flag, Cabal.flagDefault flag)
                              | flag <- PD.genPackageFlags gdesc ]
        pkgDependencies     = deps
        pkgStanzasAvailable = Set.fromList stanzas
        pkgStanzasRequested =
            -- NB: even if a package stanza is requested, if the package
            -- doesn't actually have any of that stanza we omit it from
            -- the request, to ensure that we don't decide that this
            -- package needs to be rebuilt.  (It needs to be done here,
            -- because the ElaboratedConfiguredPackage is where we test
            -- whether or not there have been changes.)
            Map.fromList $ [ (TestStanzas,  v) | v <- maybeToList tests
                                               , _ <- PD.testSuites pkgDescription ]
                        ++ [ (BenchStanzas, v) | v <- maybeToList benchmarks
                                               , _ <- PD.benchmarks pkgDescription ]
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
        pkgStanzasEnabled   = Set.empty
        pkgBuildTargets     = []
        pkgReplTarget       = Nothing
        pkgBuildHaddocks    = False

        pkgSourceLocation   = srcloc
        pkgSourceHash       = Map.lookup pkgid sourcePackageHashes
        pkgBuildStyle       = if shouldBuildInplaceOnly pkg
                                then BuildInplaceOnly else BuildAndInstall
        pkgBuildPackageDBStack    = buildAndRegisterDbs
        pkgRegisterPackageDBStack = buildAndRegisterDbs
        pkgRequiresRegistration   = PD.hasPublicLib pkgDescription

        pkgSetupScriptStyle       = packageSetupScriptStyle pkgDescription
        pkgSetupScriptCliVersion  = packageSetupScriptSpecVersion
                                      pkgSetupScriptStyle pkgDescription deps
        pkgSetupPackageDBStack    = buildAndRegisterDbs

        buildAndRegisterDbs
          | shouldBuildInplaceOnly pkg = inplacePackageDbs
          | otherwise                  = storePackageDbs

        pkgDescriptionOverride    = descOverride

        pkgVanillaLib    = perPkgOptionFlag pkgid True packageConfigVanillaLib --TODO: [required feature]: also needs to be handled recursively
        pkgSharedLib     = pkgid `Set.member` pkgsUseSharedLibrary
        pkgDynExe        = perPkgOptionFlag pkgid False packageConfigDynExe
        pkgGHCiLib       = perPkgOptionFlag pkgid False packageConfigGHCiLib --TODO: [required feature] needs to default to enabled on windows still

        pkgProfExe       = perPkgOptionFlag pkgid False packageConfigProf
        pkgProfLib       = pkgid `Set.member` pkgsUseProfilingLibrary

        (pkgProfExeDetail,
         pkgProfLibDetail) = perPkgOptionLibExeFlag pkgid ProfDetailDefault
                               packageConfigProfDetail
                               packageConfigProfLibDetail
        pkgCoverage      = perPkgOptionFlag pkgid False packageConfigCoverage

        pkgOptimization  = perPkgOptionFlag pkgid NormalOptimisation packageConfigOptimization
        pkgSplitObjs     = perPkgOptionFlag pkgid False packageConfigSplitObjs
        pkgStripLibs     = perPkgOptionFlag pkgid False packageConfigStripLibs
        pkgStripExes     = perPkgOptionFlag pkgid False packageConfigStripExes
        pkgDebugInfo     = perPkgOptionFlag pkgid NoDebugInfo packageConfigDebugInfo

        -- Combine the configured compiler prog settings with the user-supplied
        -- config. For the compiler progs any user-supplied config was taken
        -- into account earlier when configuring the compiler so its ok that
        -- our configured settings for the compiler override the user-supplied
        -- config here.
        pkgProgramPaths  = Map.fromList
                             [ (programId prog, programPath prog)
                             | prog <- configuredPrograms compilerprogdb ]
                        <> perPkgOptionMapLast pkgid packageConfigProgramPaths
        pkgProgramArgs   = Map.fromList
                             [ (programId prog, args)
                             | prog <- configuredPrograms compilerprogdb
                             , let args = programOverrideArgs prog
                             , not (null args)
                             ]
                        <> perPkgOptionMapMappend pkgid packageConfigProgramArgs
        pkgProgramPathExtra    = perPkgOptionNubList pkgid packageConfigProgramPathExtra
        pkgConfigureScriptArgs = perPkgOptionList pkgid packageConfigConfigureArgs
        pkgExtraLibDirs        = perPkgOptionList pkgid packageConfigExtraLibDirs
        pkgExtraFrameworkDirs  = perPkgOptionList pkgid packageConfigExtraFrameworkDirs
        pkgExtraIncludeDirs    = perPkgOptionList pkgid packageConfigExtraIncludeDirs
        pkgProgPrefix          = perPkgOptionMaybe pkgid packageConfigProgPrefix
        pkgProgSuffix          = perPkgOptionMaybe pkgid packageConfigProgSuffix

        pkgInstallDirs
          | shouldBuildInplaceOnly pkg
          -- use the ordinary default install dirs
          = (InstallDirs.absoluteInstallDirs
               pkgid
               (installedUnitId pkg)
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

        pkgHaddockHoogle       = perPkgOptionFlag pkgid False packageConfigHaddockHoogle
        pkgHaddockHtml         = perPkgOptionFlag pkgid False packageConfigHaddockHtml
        pkgHaddockHtmlLocation = perPkgOptionMaybe pkgid packageConfigHaddockHtmlLocation
        pkgHaddockExecutables  = perPkgOptionFlag pkgid False packageConfigHaddockExecutables
        pkgHaddockTestSuites   = perPkgOptionFlag pkgid False packageConfigHaddockTestSuites
        pkgHaddockBenchmarks   = perPkgOptionFlag pkgid False packageConfigHaddockBenchmarks
        pkgHaddockInternal     = perPkgOptionFlag pkgid False packageConfigHaddockInternal
        pkgHaddockCss          = perPkgOptionMaybe pkgid packageConfigHaddockCss
        pkgHaddockHscolour     = perPkgOptionFlag pkgid False packageConfigHaddockHscolour
        pkgHaddockHscolourCss  = perPkgOptionMaybe pkgid packageConfigHaddockHscolourCss
        pkgHaddockContents     = perPkgOptionMaybe pkgid packageConfigHaddockContents

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
      | isLocalToProject pkg = local <> perpkg
      | otherwise            =          perpkg
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
    shouldBuildInplaceOnly :: HasUnitId pkg => pkg -> Bool
    shouldBuildInplaceOnly pkg = Set.member (installedPackageId pkg)
                                            pkgsToBuildInplaceOnly

    pkgsToBuildInplaceOnly :: Set InstalledPackageId
    pkgsToBuildInplaceOnly =
        Set.fromList
      $ map installedPackageId
      $ InstallPlan.reverseDependencyClosure
          solverPlan
          [ installedPackageId (PlannedId (packageId pkg))
          | pkg <- localPackages ]

    isLocalToProject :: Package pkg => pkg -> Bool
    isLocalToProject pkg = Set.member (packageId pkg)
                                      pkgsLocalToProject

    pkgsLocalToProject :: Set PackageId
    pkgsLocalToProject = Set.fromList [ packageId pkg | pkg <- localPackages ]

    pkgsUseSharedLibrary :: Set PackageId
    pkgsUseSharedLibrary =
        packagesWithDownwardClosedProperty needsSharedLib
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
        packagesWithDownwardClosedProperty needsProfilingLib
      where
        needsProfilingLib pkg =
            fromFlagOrDefault False (profBothFlag <> profLibFlag)
          where
            pkgid        = packageId pkg
            profBothFlag = lookupPerPkgOption pkgid packageConfigProf
            profLibFlag  = lookupPerPkgOption pkgid packageConfigProfLib
            --TODO: [code cleanup] unused: the old deprecated packageConfigProfExe

    packagesWithDownwardClosedProperty property =
        Set.fromList
      $ map packageId
      $ InstallPlan.dependencyClosure
          solverPlan
          [ installedPackageId pkg
          | pkg <- InstallPlan.toList solverPlan
          , property pkg ] -- just the packages that satisfy the propety
      --TODO: [nice to have] this does not check the config consistency,
      -- e.g. a package explicitly turning off profiling, but something
      -- depending on it that needs profiling. This really needs a separate
      -- package config validation/resolution pass.

      --TODO: [nice to have] config consistency checking:
      -- * profiling libs & exes, exe needs lib, recursive
      -- * shared libs & exes, exe needs lib, recursive
      -- * vanilla libs & exes, exe needs lib, recursive
      -- * ghci or shared lib needed by TH, recursive, ghc version dependent


---------------------------
-- Build targets
--

-- Refer to ProjectPlanning.Types for details of these important types:

-- data PackageTarget = ...
-- data ComponentTarget = ...
-- data SubComponentTarget = ...


--TODO: this needs to report some user target/config errors
elaboratePackageTargets :: ElaboratedConfiguredPackage -> [PackageTarget]
                        -> ([ComponentTarget], Maybe ComponentTarget, Bool)
elaboratePackageTargets ElaboratedConfiguredPackage{..} targets =
    let buildTargets  = nubComponentTargets
                      . map compatSubComponentTargets
                      . concatMap elaborateBuildTarget
                      $ targets
        --TODO: instead of listToMaybe we should be reporting an error here
        replTargets   = listToMaybe
                      . nubComponentTargets
                      . map compatSubComponentTargets
                      . concatMap elaborateReplTarget
                      $ targets
        buildHaddocks = HaddockDefaultComponents `elem` targets

     in (buildTargets, replTargets, buildHaddocks)
  where
    --TODO: need to report an error here if defaultComponents is empty
    elaborateBuildTarget  BuildDefaultComponents    = pkgDefaultComponents
    elaborateBuildTarget (BuildSpecificComponent t) = [t]
    elaborateBuildTarget  _                         = []

    --TODO: need to report an error here if defaultComponents is empty
    elaborateReplTarget  ReplDefaultComponent     = take 1 pkgDefaultComponents
    elaborateReplTarget (ReplSpecificComponent t) = [t]
    elaborateReplTarget  _                        = []

    pkgDefaultComponents =
        [ ComponentTarget cname WholeComponent
        | c <- Cabal.pkgComponents pkgDescription
        , PD.buildable (Cabal.componentBuildInfo c)
        , let cname = Cabal.componentName c
        , enabledOptionalStanza cname
        ]
      where
        enabledOptionalStanza cname =
          case componentOptionalStanza cname of
            Nothing     -> True
            Just stanza -> Map.lookup stanza pkgStanzasRequested
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
pkgHasEphemeralBuildTargets pkg =
    isJust (pkgReplTarget pkg)
 || (not . null) [ () | ComponentTarget _ subtarget <- pkgBuildTargets pkg
                      , subtarget /= WholeComponent ]

-- | The components that we'll build all of, meaning that after they're built
-- we can skip building them again (unlike with building just some modules or
-- other files within a component).
--
pkgBuildTargetWholeComponents :: ElaboratedConfiguredPackage
                              -> Set ComponentName
pkgBuildTargetWholeComponents pkg =
    Set.fromList
      [ cname | ComponentTarget cname WholeComponent <- pkgBuildTargets pkg ]


------------------------------------------------------------------------------
-- * Install plan pruning
------------------------------------------------------------------------------

-- | Given a set of package targets (and optionally component targets within
-- those packages), take the subset of the install plan needed to build those
-- targets. Also, update the package config to specify which optional stanzas
-- to enable, and which targets within each package to build.
--
pruneInstallPlanToTargets :: Map InstalledPackageId [PackageTarget]
                          -> ElaboratedInstallPlan -> ElaboratedInstallPlan
pruneInstallPlanToTargets perPkgTargetsMap =
    either (\_ -> assert False undefined) id
  . InstallPlan.new (IndependentGoals False)
  . PackageIndex.fromList
    -- We have to do this in two passes
  . pruneInstallPlanPass2
  . pruneInstallPlanPass1 perPkgTargetsMap
  . InstallPlan.toList

-- | The first pass does three things:
--
-- * Set the build targets based on the user targets (but not rev deps yet).
-- * A first go at determining which optional stanzas (testsuites, benchmarks)
--   are needed. We have a second go in the next pass.
-- * Take the dependency closure using pruned dependencies. We prune deps that
--   are used only by unneeded optional stanzas. These pruned deps are only
--   used for the dependency closure and are not persisted in this pass.
--
pruneInstallPlanPass1 :: Map InstalledPackageId [PackageTarget]
                      -> [ElaboratedPlanPackage]
                      -> [ElaboratedPlanPackage]
pruneInstallPlanPass1 perPkgTargetsMap pkgs =
    map fst $
    dependencyClosure
      (installedPackageId . fst)  -- the pkg id
      snd                         -- the pruned deps
      [ (pkg', pruneOptionalDependencies pkg')
      | pkg <- pkgs
      , let pkg' = mapConfiguredPackage
                     (pruneOptionalStanzas . setBuildTargets) pkg
      ]
      (Map.keys perPkgTargetsMap)
  where
    -- Elaborate and set the targets we'll build for this package. This is just
    -- based on the targets from the user, not targets implied by reverse
    -- dependencies. Those comes in the second pass once we know the rev deps.
    --
    setBuildTargets pkg =
        pkg {
          pkgBuildTargets   = buildTargets,
          pkgReplTarget     = replTarget,
          pkgBuildHaddocks  = buildHaddocks
        }
      where
        (buildTargets, replTarget, buildHaddocks)
                = elaboratePackageTargets pkg targets
        targets = fromMaybe []
                $ Map.lookup (installedPackageId pkg) perPkgTargetsMap

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
    pruneOptionalStanzas pkg = pkg { pkgStanzasEnabled = stanzas }
      where
        stanzas :: Set OptionalStanza
        stanzas = optionalStanzasRequiredByTargets  pkg
               <> optionalStanzasRequestedByDefault pkg
               <> optionalStanzasWithDepsAvailable availablePkgs pkg

    -- Calculate package dependencies but cut out those needed only by
    -- optional stanzas that we've determined we will not enable.
    -- These pruned deps are not persisted in this pass since they're based on
    -- the optional stanzas and we'll make further tweaks to the optional
    -- stanzas in the next pass.
    --
    pruneOptionalDependencies :: ElaboratedPlanPackage -> [InstalledPackageId]
    pruneOptionalDependencies (InstallPlan.Configured pkg) =
        (CD.flatDeps . CD.filterDeps keepNeeded) (depends pkg)
      where
        keepNeeded (CD.ComponentTest  _) _ = TestStanzas  `Set.member` stanzas
        keepNeeded (CD.ComponentBench _) _ = BenchStanzas `Set.member` stanzas
        keepNeeded _                     _ = True
        stanzas = pkgStanzasEnabled pkg
    pruneOptionalDependencies pkg =
        CD.flatDeps (depends pkg)

    optionalStanzasRequiredByTargets :: ElaboratedConfiguredPackage
                                     -> Set OptionalStanza
    optionalStanzasRequiredByTargets pkg =
      Set.fromList
        [ stanza
        | ComponentTarget cname _ <- pkgBuildTargets pkg
                                  ++ maybeToList (pkgReplTarget pkg)
        , stanza <- maybeToList (componentOptionalStanza cname)
        ]

    optionalStanzasRequestedByDefault :: ElaboratedConfiguredPackage
                                      -> Set OptionalStanza
    optionalStanzasRequestedByDefault =
        Map.keysSet
      . Map.filter (id :: Bool -> Bool)
      . pkgStanzasRequested

    availablePkgs =
      Set.fromList
        [ installedPackageId pkg
        | InstallPlan.PreExisting pkg <- pkgs ]

-- | Given a set of already installed packages @availablePkgs@,
-- determine the set of available optional stanzas from @pkg@
-- which have all of their dependencies already installed.  This is used
-- to implement "sticky" testsuites, where once we have installed
-- all of the deps needed for the test suite, we go ahead and
-- enable it always.
optionalStanzasWithDepsAvailable :: Set InstalledPackageId
                                 -> ElaboratedConfiguredPackage
                                 -> Set OptionalStanza
optionalStanzasWithDepsAvailable availablePkgs pkg =
    Set.fromList
      [ stanza
      | stanza <- Set.toList (pkgStanzasAvailable pkg)
      , let deps :: [InstalledPackageId]
            deps = map installedPackageId
                 $ CD.select (optionalStanzaDeps stanza)
                             (pkgDependencies pkg)
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
    setStanzasDepsAndTargets pkg =
        pkg {
          pkgStanzasEnabled = stanzas,
          pkgDependencies   = CD.filterDeps keepNeeded (pkgDependencies pkg),
          pkgBuildTargets   = pkgBuildTargets pkg ++ targetsRequiredForRevDeps
        }
      where
        stanzas :: Set OptionalStanza
        stanzas = pkgStanzasEnabled pkg
               <> optionalStanzasWithDepsAvailable availablePkgs pkg

        keepNeeded (CD.ComponentTest  _) _ = TestStanzas  `Set.member` stanzas
        keepNeeded (CD.ComponentBench _) _ = BenchStanzas `Set.member` stanzas
        keepNeeded _                     _ = True

        targetsRequiredForRevDeps =
          [ ComponentTarget (Cabal.defaultLibName (pkgSourceId pkg)) WholeComponent
          -- if anything needs this pkg, build the library component
          | installedPackageId pkg `Set.member` hasReverseLibDeps
          ]
        --TODO: also need to track build-tool rev-deps for exes

    availablePkgs :: Set InstalledPackageId
    availablePkgs = Set.fromList (map installedPackageId pkgs)

    hasReverseLibDeps :: Set InstalledPackageId
    hasReverseLibDeps =
      Set.fromList [ depid | pkg <- pkgs
                           , depid <- CD.flatDeps (depends pkg) ]


mapConfiguredPackage :: (ElaboratedConfiguredPackage -> ElaboratedConfiguredPackage)
                     -> ElaboratedPlanPackage
                     -> ElaboratedPlanPackage
mapConfiguredPackage f (InstallPlan.Configured pkg) =
  InstallPlan.Configured (f pkg)
mapConfiguredPackage _ pkg = pkg

componentOptionalStanza :: Cabal.ComponentName -> Maybe OptionalStanza
componentOptionalStanza (Cabal.CTestName  _) = Just TestStanzas
componentOptionalStanza (Cabal.CBenchName _) = Just BenchStanzas
componentOptionalStanza _                    = Nothing


dependencyClosure :: (pkg -> InstalledPackageId)
                  -> (pkg -> [InstalledPackageId])
                  -> [pkg]
                  -> [InstalledPackageId]
                  -> [pkg]
dependencyClosure pkgid deps allpkgs =
    map vertexToPkg
  . concatMap Tree.flatten
  . Graph.dfs graph
  . map pkgidToVertex
  where
    (graph, vertexToPkg, pkgidToVertex) = dependencyGraph pkgid deps allpkgs

dependencyGraph :: (pkg -> InstalledPackageId)
                -> (pkg -> [InstalledPackageId])
                -> [pkg]
                -> (Graph.Graph,
                    Graph.Vertex -> pkg,
                    InstalledPackageId -> Graph.Vertex)
dependencyGraph pkgid deps pkgs =
    (graph, vertexToPkg', pkgidToVertex')
  where
    (graph, vertexToPkg, pkgidToVertex) =
      Graph.graphFromEdges [ ( pkg, pkgid pkg, deps pkg )
                           | pkg <- pkgs ]
    vertexToPkg'   = (\(pkg,_,_) -> pkg)
                   . vertexToPkg
    pkgidToVertex' = fromMaybe (error "dependencyGraph: lookup failure")
                   . pkgidToVertex


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
          -- * We constrain it to be >= 1.18 < 2
          --
          cabalConstraint   = orLaterVersion cabalCompatMinVer
                                `intersectVersionRanges`
                              orLaterVersion (PD.specVersion pkg)
                                `intersectVersionRanges`
                              earlierVersion cabalCompatMaxVer
          -- The idea here is that at some point we will make significant
          -- breaking changes to the Cabal API that Setup.hs scripts use.
          -- So for old custom Setup scripts that do not specify explicit
          -- constraints, we constrain them to use a compatible Cabal version.
          -- The exact version where we'll make this API break has not yet been
          -- decided, so for the meantime we guess at 2.x.
          cabalCompatMaxVer = Version [2] []
          -- In principle we can talk to any old Cabal version, and we need to
          -- be able to do that for custom Setup scripts that require older
          -- Cabal lib versions. However in practice we have currently have
          -- problems with Cabal-1.16. (1.16 does not know about build targets)
          -- If this is fixed we can relax this constraint.
          cabalCompatMinVer = Version [1,18] []

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
cabalPkgname = PackageName "Cabal"
basePkgname  = PackageName "base"


legacyCustomSetupPkgs :: Compiler -> Platform -> [PackageName]
legacyCustomSetupPkgs compiler (Platform _ os) =
    map PackageName $
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
setupHsScriptOptions (ReadyPackage ElaboratedConfiguredPackage{..})
                     ElaboratedSharedConfig{..} srcdir builddir
                     isParallelBuild cacheLock =
    SetupScriptOptions {
      useCabalVersion          = thisVersion pkgSetupScriptCliVersion,
      useCabalSpecVersion      = Just pkgSetupScriptCliVersion,
      useCompiler              = Just pkgConfigCompiler,
      usePlatform              = Just pkgConfigPlatform,
      usePackageDB             = pkgSetupPackageDBStack,
      usePackageIndex          = Nothing,
      useDependencies          = [ (uid, srcid)
                                 | ConfiguredId srcid uid <- CD.setupDeps pkgDependencies ],
      useDependenciesExclusive = True,
      useVersionMacros         = pkgSetupScriptStyle == SetupCustomExplicitDeps,
      useProgramConfig         = pkgConfigCompilerProgs,
      useDistPref              = builddir,
      useLoggingHandle         = Nothing, -- this gets set later
      useWorkingDir            = Just srcdir,
      useWin32CleanHack        = False,   --TODO: [required eventually]
      forceExternalSetupMethod = isParallelBuild,
      setupCacheLock           = Just cacheLock
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
setupHsConfigureFlags (ReadyPackage
                         pkg@ElaboratedConfiguredPackage{..})
                      sharedConfig@ElaboratedSharedConfig{..}
                      verbosity builddir =
    sanityCheckElaboratedConfiguredPackage sharedConfig pkg
        (Cabal.ConfigFlags {..})
  where
    configDistPref            = toFlag builddir
    configVerbosity           = toFlag verbosity

    configIPID                = toFlag (display (installedUnitId pkg))

    configProgramPaths        = Map.toList pkgProgramPaths
    configProgramArgs         = Map.toList pkgProgramArgs
    configProgramPathExtra    = toNubList pkgProgramPathExtra
    configHcFlavor            = toFlag (compilerFlavor pkgConfigCompiler)
    configHcPath              = mempty -- we use configProgramPaths instead
    configHcPkg               = mempty -- we use configProgramPaths instead

    configVanillaLib          = toFlag pkgVanillaLib
    configSharedLib           = toFlag pkgSharedLib
    configDynExe              = toFlag pkgDynExe
    configGHCiLib             = toFlag pkgGHCiLib
    configProfExe             = mempty
    configProfLib             = toFlag pkgProfLib
    configProf                = toFlag pkgProfExe

    -- configProfDetail is for exe+lib, but overridden by configProfLibDetail
    -- so we specify both so we can specify independently
    configProfDetail          = toFlag pkgProfExeDetail
    configProfLibDetail       = toFlag pkgProfLibDetail

    configCoverage            = toFlag pkgCoverage
    configLibCoverage         = mempty

    configOptimization        = toFlag pkgOptimization
    configSplitObjs           = toFlag pkgSplitObjs
    configStripExes           = toFlag pkgStripExes
    configStripLibs           = toFlag pkgStripLibs
    configDebugInfo           = toFlag pkgDebugInfo
    configAllowNewer          = mempty -- we use configExactConfiguration True

    configConfigurationsFlags = pkgFlagAssignment
    configConfigureArgs       = pkgConfigureScriptArgs
    configExtraLibDirs        = pkgExtraLibDirs
    configExtraFrameworkDirs  = pkgExtraFrameworkDirs
    configExtraIncludeDirs    = pkgExtraIncludeDirs
    configProgPrefix          = maybe mempty toFlag pkgProgPrefix
    configProgSuffix          = maybe mempty toFlag pkgProgSuffix

    configInstallDirs         = fmap (toFlag . InstallDirs.toPathTemplate)
                                     pkgInstallDirs

    -- we only use configDependencies, unless we're talking to an old Cabal
    -- in which case we use configConstraints
    configDependencies        = [ (packageName srcid, uid)
                                | ConfiguredId srcid uid <- CD.nonSetupDeps pkgDependencies ]
    configConstraints         = [ thisPackageVersion srcid
                                | ConfiguredId srcid _uid <- CD.nonSetupDeps pkgDependencies ]

    -- explicitly clear, then our package db stack
    -- TODO: [required eventually] have to do this differently for older Cabal versions
    configPackageDBs          = Nothing : map Just pkgBuildPackageDBStack

    configTests               = toFlag (TestStanzas  `Set.member` pkgStanzasEnabled)
    configBenchmarks          = toFlag (BenchStanzas `Set.member` pkgStanzasEnabled)

    configExactConfiguration  = toFlag True
    configFlagError           = mempty --TODO: [research required] appears not to be implemented
    configRelocatable         = mempty --TODO: [research required] ???
    configScratchDir          = mempty -- never use
    configUserInstall         = mempty -- don't rely on defaults
    configPrograms_           = mempty -- never use, shouldn't exist


setupHsBuildFlags :: ElaboratedConfiguredPackage
                  -> ElaboratedSharedConfig
                  -> Verbosity
                  -> FilePath
                  -> Cabal.BuildFlags
setupHsBuildFlags ElaboratedConfiguredPackage{..} _ verbosity builddir =
    Cabal.BuildFlags {
      buildProgramPaths = mempty, --unused, set at configure time
      buildProgramArgs  = mempty, --unused, set at configure time
      buildVerbosity    = toFlag verbosity,
      buildDistPref     = toFlag builddir,
      buildAssumeDepsUpToDate = toFlag False,
      buildNumJobs      = mempty, --TODO: [nice to have] sometimes want to use toFlag (Just numBuildJobs),
      buildArgs         = mempty  -- unused, passed via args not flags
    }


setupHsBuildArgs :: ElaboratedConfiguredPackage -> [String]
setupHsBuildArgs pkg =
    map (showComponentTarget pkg) (pkgBuildTargets pkg)


showComponentTarget :: ElaboratedConfiguredPackage -> ComponentTarget -> String
showComponentTarget _pkg =
    showBuildTarget . toBuildTarget
  where
    showBuildTarget t =
      Cabal.showBuildTarget (qlBuildTarget t) t

    qlBuildTarget Cabal.BuildTargetComponent{} = Cabal.QL2
    qlBuildTarget _                            = Cabal.QL3

    toBuildTarget :: ComponentTarget -> Cabal.BuildTarget
    toBuildTarget (ComponentTarget cname subtarget) =
      case subtarget of
        WholeComponent     -> Cabal.BuildTargetComponent cname
        ModuleTarget mname -> Cabal.BuildTargetModule    cname mname
        FileTarget   fname -> Cabal.BuildTargetFile      cname fname


setupHsReplFlags :: ElaboratedConfiguredPackage
                 -> ElaboratedSharedConfig
                 -> Verbosity
                 -> FilePath
                 -> Cabal.ReplFlags
setupHsReplFlags ElaboratedConfiguredPackage{..} _ verbosity builddir =
    Cabal.ReplFlags {
      replProgramPaths = mempty, --unused, set at configure time
      replProgramArgs  = mempty, --unused, set at configure time
      replVerbosity    = toFlag verbosity,
      replDistPref     = toFlag builddir,
      replReload       = mempty  --only used as callback from repl
    }


setupHsReplArgs :: ElaboratedConfiguredPackage -> [String]
setupHsReplArgs pkg =
    maybe [] (\t -> [showComponentTarget pkg t]) (pkgReplTarget pkg)
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
      copyAssumeDepsUpToDate = toFlag False,
      copyVerbosity = toFlag verbosity
    }

setupHsRegisterFlags :: ElaboratedConfiguredPackage
                     -> ElaboratedSharedConfig
                     -> Verbosity
                     -> FilePath
                     -> FilePath
                     -> Cabal.RegisterFlags
setupHsRegisterFlags ElaboratedConfiguredPackage {pkgBuildStyle} _
                     verbosity builddir pkgConfFile =
    Cabal.RegisterFlags {
      regPackageDB   = mempty,  -- misfeature
      regGenScript   = mempty,  -- never use
      regGenPkgConf  = toFlag (Just pkgConfFile),
      regInPlace     = case pkgBuildStyle of
                         BuildInplaceOnly -> toFlag True
                         _                -> toFlag False,
      regPrintId     = mempty,  -- never use
      regDistPref    = toFlag builddir,
      regVerbosity   = toFlag verbosity,
      -- Currently not used, because this is per-package.
      regAssumeDepsUpToDate = toFlag False,
      regArgs        = []
    }

setupHsHaddockFlags :: ElaboratedConfiguredPackage
                    -> ElaboratedSharedConfig
                    -> Verbosity
                    -> FilePath
                    -> Cabal.HaddockFlags
setupHsHaddockFlags ElaboratedConfiguredPackage{..} _ verbosity builddir =
    Cabal.HaddockFlags {
      haddockProgramPaths  = mempty, --unused, set at configure time
      haddockProgramArgs   = mempty, --unused, set at configure time
      haddockHoogle        = toFlag pkgHaddockHoogle,
      haddockHtml          = toFlag pkgHaddockHtml,
      haddockHtmlLocation  = maybe mempty toFlag pkgHaddockHtmlLocation,
      haddockForHackage    = mempty, --TODO: new flag
      haddockExecutables   = toFlag pkgHaddockExecutables,
      haddockTestSuites    = toFlag pkgHaddockTestSuites,
      haddockBenchmarks    = toFlag pkgHaddockBenchmarks,
      haddockInternal      = toFlag pkgHaddockInternal,
      haddockCss           = maybe mempty toFlag pkgHaddockCss,
      haddockHscolour      = toFlag pkgHaddockHscolour,
      haddockHscolourCss   = maybe mempty toFlag pkgHaddockHscolourCss,
      haddockContents      = maybe mempty toFlag pkgHaddockContents,
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
    pkg@ElaboratedConfiguredPackage{
      pkgSourceId,
      pkgSourceHash = Just srchash,
      pkgDependencies
    } =
    PackageHashInputs {
      pkgHashPkgId       = pkgSourceId,
      pkgHashSourceHash  = srchash,
      pkgHashDirectDeps  = Set.fromList
                             [ installedPackageId dep
                             | dep <- CD.select relevantDeps pkgDependencies ],
      pkgHashOtherConfig = packageHashConfigInputs pkgshared pkg
    }
  where
    -- Obviously the main deps are relevant
    relevantDeps (CD.ComponentLib _)   = True
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
      pkgHashFlagAssignment      = pkgFlagAssignment,
      pkgHashConfigureScriptArgs = pkgConfigureScriptArgs,
      pkgHashVanillaLib          = pkgVanillaLib,
      pkgHashSharedLib           = pkgSharedLib,
      pkgHashDynExe              = pkgDynExe,
      pkgHashGHCiLib             = pkgGHCiLib,
      pkgHashProfLib             = pkgProfLib,
      pkgHashProfExe             = pkgProfExe,
      pkgHashProfLibDetail       = pkgProfLibDetail,
      pkgHashProfExeDetail       = pkgProfExeDetail,
      pkgHashCoverage            = pkgCoverage,
      pkgHashOptimization        = pkgOptimization,
      pkgHashSplitObjs           = pkgSplitObjs,
      pkgHashStripLibs           = pkgStripLibs,
      pkgHashStripExes           = pkgStripExes,
      pkgHashDebugInfo           = pkgDebugInfo,
      pkgHashExtraLibDirs        = pkgExtraLibDirs,
      pkgHashExtraFrameworkDirs  = pkgExtraFrameworkDirs,
      pkgHashExtraIncludeDirs    = pkgExtraIncludeDirs,
      pkgHashProgPrefix          = pkgProgPrefix,
      pkgHashProgSuffix          = pkgProgSuffix
    }


-- | Given the 'InstalledPackageIndex' for a nix-style package store, and an
-- 'ElaboratedInstallPlan', replace configured source packages by pre-existing
-- installed packages whenever they exist.
--
improveInstallPlanWithPreExistingPackages :: InstalledPackageIndex
                                          -> ElaboratedInstallPlan
                                          -> ElaboratedInstallPlan
improveInstallPlanWithPreExistingPackages installedPkgIndex installPlan =
    replaceWithPreExisting installPlan
      [ ipkg
      | InstallPlan.Configured pkg
          <- InstallPlan.reverseTopologicalOrder installPlan
      , ipkg <- maybeToList (canPackageBeImproved pkg) ]
  where
    --TODO: sanity checks:
    -- * the installed package must have the expected deps etc
    -- * the installed package must not be broken, valid dep closure

    --TODO: decide what to do if we encounter broken installed packages,
    -- since overwriting is never safe.

    canPackageBeImproved pkg =
      PackageIndex.lookupUnitId
        installedPkgIndex (installedPackageId pkg)

    replaceWithPreExisting =
      foldl' (\plan ipkg -> InstallPlan.preexisting
                              (installedPackageId ipkg) ipkg plan)
