{-# LANGUAGE RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, 
             RankNTypes, ScopedTypeVariables #-}

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

    -- * Selecting a plan subset
    ComponentTarget(..),
    pruneInstallPlanToTargets,
    
    -- * Setup.hs CLI flags for building
    setupHsScriptOptions,
    setupHsConfigureFlags,
    setupHsBuildFlags,
    setupHsBuildArgs,
    setupHsCopyFlags,
    setupHsRegisterFlags,

    packageHashInputs,
    
    -- TODO: [code cleanup] utils that should live in some shared place?
    createPackageDBIfMissing
  ) where

import           Distribution.Client.PackageHash
import           Distribution.Client.RebuildMonad
import           Distribution.Client.ProjectConfig

import           Distribution.Client.Types hiding (BuildResult, BuildSuccess(..), BuildFailure(..), DocsResult(..), TestsResult(..))
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan, InstallPlan, GenericPlanPackage )
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.Dependency
import           Distribution.Client.Dependency.Types
import qualified Distribution.Client.ComponentDeps as CD
import           Distribution.Client.ComponentDeps (ComponentDeps)
import qualified Distribution.Client.IndexUtils as IndexUtils
import           Distribution.Client.Targets
import           Distribution.Client.DistDirLayout
import           Distribution.Client.SetupWrapper
import           Distribution.Client.JobControl
import           Distribution.Client.FetchUtils
import           Distribution.Client.Setup hiding (packageName, cabalVersion)
import           Distribution.Utils.NubList (toNubList)

import           Distribution.Package hiding (InstalledPackageId, installedPackageId)
import           Distribution.System
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import qualified Distribution.PackageDescription.Parse as Cabal
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import           Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import           Distribution.Simple.Compiler hiding (Flag)
import qualified Distribution.Simple.GHC   as GHC   --TODO: [code cleanup] eliminate
import qualified Distribution.Simple.GHCJS as GHCJS --TODO: [code cleanup] eliminate
import           Distribution.Simple.Program
import           Distribution.Simple.Program.Db
import           Distribution.Simple.Program.Find
import qualified Distribution.Simple.Setup as Cabal
import           Distribution.Simple.Setup (Flag, toFlag, flagToMaybe, flagToList, fromFlag, fromFlagOrDefault)
import qualified Distribution.Simple.Configure as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.InstallDirs as InstallDirs
import           Distribution.Simple.InstallDirs (PathTemplate)
import           Distribution.Simple.BuildTarget as Cabal

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
import qualified Data.ByteString.Lazy as LBS

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State as State
import           Control.Exception
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Function

import           Distribution.Compat.Binary
import           GHC.Generics (Generic)
import           Data.Typeable (Typeable)

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

-- | The combination of an elaborated install plan plus a
-- 'ElaboratedSharedConfig' contains all the details necessary to be able
-- to execute the plan without having to make further policy decisions.
--
-- It does not include dynamic elements such as resources (such as http
-- connections).
--
type ElaboratedInstallPlan
   = GenericInstallPlan InstalledPackageInfo
                        ElaboratedConfiguredPackage
                        BuildSuccess BuildFailure

type ElaboratedPlanPackage
   = GenericPlanPackage InstalledPackageInfo
                        ElaboratedConfiguredPackage
                        BuildSuccess BuildFailure

type SolverInstallPlan
   = InstallPlan --TODO: [code cleanup] redefine locally or move def to solver interface

--TODO: [code cleanup] decide if we really need this, there's not much in it, and in principle
--      even platform and compiler could be different if we're building things
--      like a server + client with ghc + ghcjs
data ElaboratedSharedConfig
   = ElaboratedSharedConfig {

       pkgConfigPlatform         :: Platform,
       pkgConfigCompiler         :: Compiler, --TODO: [code cleanup] replace with CompilerInfo
       pkgConfigProgramDb        :: ProgramDb --TODO: [code cleanup] no Eq instance
       --TODO: [code cleanup] binary instance does not preserve the prog paths
       --      perhaps should keep the configured progs separately
     }
  deriving (Show, Generic)

instance Binary ElaboratedSharedConfig

data ElaboratedConfiguredPackage
   = ElaboratedConfiguredPackage {

       pkgInstalledId :: InstalledPackageId,
       pkgSourceId    :: PackageId,

       -- | TODO: [code cleanup] we don't need this, just a few bits from it:
       --   build type, spec version
       pkgDescription :: Cabal.PackageDescription,

       -- | A total flag assignment for the package
       pkgFlagAssignment   :: Cabal.FlagAssignment,

       -- | The original default flag assignment, used only for reporting.
       pkgFlagDefaults     :: Cabal.FlagAssignment,

       -- | The exact dependencies (on other plan packages)
       --
       pkgDependencies     :: ComponentDeps [ConfiguredId],

       -- | Which optional stanzas (ie testsuites, benchmarks) can be built.
       -- This means the solver produced a plan that has them available.
       -- This doesn't necessary mean we build them by default.
       pkgStanzasAvailable :: Set OptionalStanza,

       -- | Which optional stanzas the user explicitly asked to enable or
       -- to disable. This tells us which ones we build by default, and
       -- helps with error messages when the user asks to build something
       -- they explicitly disabled.
       pkgStanzasRequested :: Map OptionalStanza Bool,

       -- | Which optional stanzas (ie testsuites, benchmarks) will actually
       -- be enabled during the package configure step.
       pkgStanzasEnabled :: Set OptionalStanza,

       -- | Where the package comes from, e.g. tarball, local dir etc. This
       --   is not the same as where it may be unpacked to for the build.
       pkgSourceLocation :: PackageLocation (Maybe FilePath),

       -- | The hash of the source, e.g. the tarball. We don't have this for
       -- local source dir packages.
       pkgSourceHash     :: Maybe PackageSourceHash,

       --pkgSourceDir ? -- currently passed in later because they can use temp locations
       --pkgBuildDir  ? -- but could in principle still have it here, with optional instr to use temp loc

       pkgBuildStyle             :: BuildStyle,

       pkgSetupPackageDBStack    :: PackageDBStack,
       pkgBuildPackageDBStack    :: PackageDBStack,
       pkgRegisterPackageDBStack :: PackageDBStack,

       -- | The package contains a library and so must be registered
       pkgRequiresRegistration :: Bool,
       pkgDescriptionOverride  :: Maybe CabalFileText,

       pkgVanillaLib           :: Bool,
       pkgSharedLib            :: Bool,
       pkgDynExe               :: Bool,
       pkgGHCiLib              :: Bool,
       pkgProfLib              :: Bool,
       pkgProfExe              :: Bool,
       pkgProfLibDetail        :: ProfDetailLevel,
       pkgProfExeDetail        :: ProfDetailLevel,
       pkgCoverage             :: Bool,
       pkgOptimization         :: OptimisationLevel,
       pkgSplitObjs            :: Bool,
       pkgStripLibs            :: Bool,
       pkgStripExes            :: Bool,
       pkgDebugInfo            :: DebugInfoLevel,

       pkgConfigureScriptArgs   :: [String],
       pkgExtraLibDirs          :: [FilePath],
       pkgExtraIncludeDirs      :: [FilePath],
       pkgProgPrefix            :: Maybe PathTemplate,
       pkgProgSuffix            :: Maybe PathTemplate,

       pkgInstallDirs           :: InstallDirs.InstallDirs FilePath,

       -- Setup.hs related things:

       -- | One of four modes for how we build and interact with the Setup.hs
       -- script, based on whether it's a build-type Custom, with or without
       -- explicit deps and the cabal spec version the .cabal file needs.
       pkgSetupScriptStyle      :: SetupScriptStyle,

       -- | The version of the Cabal command line interface that we are using
       -- for this package. This is typically the version of the Cabal lib
       -- that the Setup.hs is built against.
       pkgSetupScriptCliVersion :: Version,

       -- Build time related:
       pkgBuildTargets          :: [Cabal.BuildTarget]
     }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedConfiguredPackage

instance Package ElaboratedConfiguredPackage where
  packageId = pkgSourceId

instance HasUnitId ElaboratedConfiguredPackage where
  installedUnitId = pkgInstalledId

instance PackageFixedDeps ElaboratedConfiguredPackage where
  depends = fmap (map installedPackageId) . pkgDependencies

-- | This is used in the install plan to indicate how the package will be
-- built.
--
data BuildStyle =
    -- | The classic approach where the package is built, then the files
    -- installed into some location and the result registered in a package db.
    --
    -- If the package came from a tarball then it's built in a temp dir and
    -- the results discarded.
    BuildAndInstall

    -- | The package is built, but the files are not installed anywhere,
    -- rather the build dir is kept and the package is registered inplace.
    --
    -- Such packages can still subsequently be installed.
    --
    -- Typically 'BuildAndInstall' packages will only depend on other
    -- 'BuildAndInstall' style packages and not on 'BuildInplaceOnly' ones.
    --
  | BuildInplaceOnly
  deriving (Eq, Show, Generic)

instance Binary BuildStyle

type CabalFileText = LBS.ByteString

type ElaboratedReadyPackage = GenericReadyPackage ElaboratedConfiguredPackage
                                                  InstalledPackageInfo

--TODO: [code cleanup] this duplicates the InstalledPackageInfo quite a bit in an install plan
-- because the same ipkg is used by many packages. So the binary file will be big.
-- Could we keep just (ipkgid, deps) instead of the whole InstalledPackageInfo?
-- or transform to a shared form when serialising / deserialising

data GenericBuildResult ipkg iresult ifailure
                  = BuildFailure ifailure
                  | BuildSuccess (Maybe ipkg) iresult
  deriving (Eq, Show, Generic)

instance (Binary ipkg, Binary iresult, Binary ifailure) =>
         Binary (GenericBuildResult ipkg iresult ifailure)

type BuildResult  = GenericBuildResult InstalledPackageInfo 
                                       BuildSuccess BuildFailure

data BuildSuccess = BuildOk Bool DocsResult TestsResult
  deriving (Eq, Show, Generic)

data DocsResult  = DocsNotTried  | DocsFailed  | DocsOk
  deriving (Eq, Show, Generic)

data TestsResult = TestsNotTried | TestsOk
  deriving (Eq, Show, Generic)

data BuildFailure = PlanningFailed              --TODO: [required eventually] not yet used
                  | DependentFailed PackageId
                  | DownloadFailed  String      --TODO: [required eventually] not yet used
                  | UnpackFailed    String      --TODO: [required eventually] not yet used
                  | ConfigureFailed String
                  | BuildFailed     String
                  | TestsFailed     String      --TODO: [required eventually] not yet used
                  | InstallFailed   String
  deriving (Eq, Show, Typeable, Generic)

instance Exception BuildFailure

instance Binary BuildFailure
instance Binary BuildSuccess
instance Binary DocsResult
instance Binary TestsResult


sanityCheckElaboratedConfiguredPackage :: ElaboratedSharedConfig
                                       -> ElaboratedConfiguredPackage
                                       -> Bool
sanityCheckElaboratedConfiguredPackage sharedConfig
                                       pkg@ElaboratedConfiguredPackage{..} =

    pkgStanzasEnabled `Set.isSubsetOf` pkgStanzasAvailable

    -- the stanzas explicitly enabled should be available and enabled
 && Map.keysSet (Map.filter id pkgStanzasRequested)
      `Set.isSubsetOf` pkgStanzasEnabled

    -- the stanzas explicitly disabled should not be available
 && Set.null (Map.keysSet (Map.filter not pkgStanzasRequested)
                `Set.intersection` pkgStanzasAvailable)

 && (pkgBuildStyle == BuildInplaceOnly ||
     installedPackageId pkg == hashedInstalledPackageId
                                 (packageHashInputs sharedConfig pkg))

 && (pkgBuildStyle == BuildInplaceOnly ||
     Set.null pkgStanzasAvailable)


------------------------------------------------------------------------------
-- * Deciding what to do: making an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------

type CliConfig = ( ProjectConfigSolver
                 , PackageConfigShared
                 , PackageConfig
                 )

rebuildInstallPlan :: Verbosity
                   -> FilePath -> DistDirLayout -> CabalDirLayout
                   -> CliConfig
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
                   } = \cliConfig ->
    runRebuild $ do
    progsearchpath <- liftIO $ getSystemSearchPath

    -- The overall improved plan is cached
    rerunIfChanged verbosity projectRootDir fileMonitorImprovedPlan
                   -- react to changes in command line args and the path
                   (cliConfig, progsearchpath) $ do

      -- And so is the elaborated plan that the improved plan based on
      (elaboratedPlan, elaboratedShared,
       projectConfig) <-
        rerunIfChanged verbosity projectRootDir fileMonitorElaboratedPlan
                       (cliConfig, progsearchpath) $ do

          projectConfig <- phaseReadProjectConfig cliConfig
          localPackages <- phaseReadLocalPackages projectConfig
          compilerEtc   <- phaseConfigureCompiler projectConfig
          solverPlan    <- phaseRunSolver         projectConfig compilerEtc
                                                  localPackages
          (elaboratedPlan,
           elaboratedShared) <- phaseElaboratePlan projectConfig compilerEtc
                                                   solverPlan localPackages

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
    phaseReadProjectConfig :: CliConfig -> Rebuild ProjectConfig
    phaseReadProjectConfig ( cliConfigSolver
                           , cliConfigAllPackages
                           , cliConfigLocalPackages
                           ) = do
      liftIO $ do
        info verbosity "Project settings changed, reconfiguring..."
        createDirectoryIfMissingVerbose verbosity False distDirectory
        createDirectoryIfMissingVerbose verbosity False distProjectCacheDirectory

      projectConfig <- readProjectConfig verbosity projectRootDir
      return (projectConfigMergeCommandLineFlags
                cliConfigSolver
                cliConfigAllPackages
                cliConfigLocalPackages
                projectConfig)

    -- Look for all the cabal packages in the project
    -- some of which may be local src dirs, tarballs etc
    --
    phaseReadLocalPackages :: ProjectConfig
                           -> Rebuild [SourcePackage]
    phaseReadLocalPackages projectConfig = do

      localCabalFiles <- findProjectCabalFiles projectConfig
      mapM (readSourcePackage verbosity) localCabalFiles


    -- Configure the compiler we're using.
    --
    -- This is moderately expensive and doesn't change that often so we cache
    -- it independently.
    --
    phaseConfigureCompiler :: ProjectConfig
                           -> Rebuild (Compiler, Platform, ProgramDb)
    phaseConfigureCompiler ProjectConfig{projectConfigAllPackages} = do
        progsearchpath <- liftIO $ getSystemSearchPath
        rerunIfChanged verbosity projectRootDir fileMonitorCompiler
                       (hcFlavor, hcPath, hcPkg, progsearchpath) $ do

          liftIO $ info verbosity "Compiler settings changed, reconfiguring..."
          result@(_, _, progdb) <- liftIO $
            Cabal.configCompilerEx
              hcFlavor hcPath hcPkg
              defaultProgramDb verbosity

          monitorFiles (programsMonitorFiles progdb)

          return result
      where
        hcFlavor = flagToMaybe packageConfigHcFlavor
        hcPath   = flagToMaybe packageConfigHcPath
        hcPkg    = flagToMaybe packageConfigHcPkg
        PackageConfigShared {
          packageConfigHcFlavor,
          packageConfigHcPath,
          packageConfigHcPkg
        }        = projectConfigAllPackages


    -- Run the solver to get the initial install plan.
    -- This is expensive so we cache it independently.
    --
    phaseRunSolver :: ProjectConfig
                   -> (Compiler, Platform, ProgramDb)
                   -> [SourcePackage]
                   -> Rebuild SolverInstallPlan
    phaseRunSolver projectConfig@ProjectConfig {
                     projectConfigSolver,
                     projectConfigBuildOnly
                   }
                   (compiler, platform, progdb)
                   localPackages =
        rerunIfChanged verbosity projectRootDir fileMonitorSolverPlan
                       (projectConfigSolver, cabalPackageCacheDirectory,
                        localPackages, localPackagesEnabledStanzas,
                        compiler, platform, programsDbSignature progdb) $ do

          installedPkgIndex <- getInstalledPackages verbosity
                                                    compiler progdb platform
                                                    corePackageDbs
          sourcePkgDb       <- getSourcePackages    verbosity withRepoCtx

          liftIO $ do
            solver <- chooseSolver verbosity solverpref (compilerInfo compiler)

            notice verbosity "Resolving dependencies..."
            foldProgress logMsg die return $
              planPackages compiler platform solver projectConfigSolver
                           installedPkgIndex sourcePkgDb
                           localPackages localPackagesEnabledStanzas
      where
        corePackageDbs = [GlobalPackageDB]
        withRepoCtx    = projectConfigWithSolverRepoContext verbosity
                           cabalPackageCacheDirectory
                           projectConfigSolver
                           projectConfigBuildOnly
        --TODO: [required eventually] the projectConfigBuildOnly we have here
        -- comes only from the config file, not from the command line, so
        -- cannot select the http transport etc.
        solverpref     = fromFlag projectConfigSolverSolver
        logMsg message rest = debugNoWrap verbosity message >> rest

        ProjectConfigSolver {projectConfigSolverSolver} = projectConfigSolver

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
                       -> [SourcePackage]
                       -> Rebuild ( ElaboratedInstallPlan
                                  , ElaboratedSharedConfig )
    phaseElaboratePlan ProjectConfig {
                         projectConfigAllPackages,
                         projectConfigLocalPackages,
                         projectConfigSpecificPackage,
                         projectConfigSolver,
                         projectConfigBuildOnly
                       }
                       (compiler, platform, progdb)
                       solverPlan localPackages = do

        liftIO $ debug verbosity "Elaborating the install plan..."

        sourcePackageHashes <-
          rerunIfChanged verbosity projectRootDir fileMonitorSourceHashes
                         (map packageId $ InstallPlan.toList solverPlan) $
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
            projectConfigAllPackages
            projectConfigLocalPackages
            projectConfigSpecificPackage
      where
        withRepoCtx = projectConfigWithSolverRepoContext verbosity 
                        cabalPackageCacheDirectory
                        projectConfigSolver
                        projectConfigBuildOnly


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
          pkgConfigCompiler  = compiler,
          pkgConfigPlatform  = platform,
          pkgConfigProgramDb = progdb
        } = elaboratedShared



findProjectCabalFiles :: ProjectConfig -> Rebuild [FilePath]
findProjectCabalFiles ProjectConfig{..} = do
    monitorFiles (map MonitorFileGlob projectConfigPackageGlobs)
    liftIO $ map (projectConfigRootDir </>) . concat
         <$> mapM (matchFileGlob projectConfigRootDir) projectConfigPackageGlobs
    --TODO: certain package things must match. Globs perhaps can match nothing,
    -- but specific files really must match or fail noisily.
    -- silently matching nothing is not ok.


readSourcePackage :: Verbosity -> FilePath -> Rebuild SourcePackage
readSourcePackage verbosity cabalFile = do
    -- no need to monitorFiles because findProjectCabalFiles did it already
    pkgdesc <- liftIO $ Cabal.readPackageDescription verbosity cabalFile
    let srcLocation = LocalUnpackedPackage (takeDirectory cabalFile)
    return SourcePackage {
             packageInfoId        = packageId pkgdesc,
             packageDescription   = pkgdesc,
             packageSource        = srcLocation,
             packageDescrOverride = Nothing
           }

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
    monitorFiles . map MonitorFile
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
    monitorFiles . map MonitorFile
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

    monitorFiles . map MonitorFile
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


recreateDirectory :: Verbosity -> Bool -> FilePath -> Rebuild ()
recreateDirectory verbosity createParents dir = do
    liftIO $ createDirectoryIfMissingVerbose verbosity createParents dir
    monitorFiles [MonitorFile dir]


-- | Get the 'HashValue' for all the source packages where we use hashes,
-- and download any packages required to do so.
--
-- Note that we don't get hashes for local unpacked packages.
--
getPackageSourceHashes :: Verbosity
                       -> (forall a. (RepoContext -> IO a) -> IO a)
                       -> SolverInstallPlan
                       -> Rebuild (Map PackageId PackageSourceHash)
getPackageSourceHashes verbosity withRepoCtx installPlan = do

    -- Determine which packages need fetching, and which are present already
    --
    pkgslocs <- liftIO $ sequence
      [ do let locm = packageSource pkg
           mloc <- checkFetched locm
           return (pkg, locm, mloc)
      | InstallPlan.Configured
          (ConfiguredPackage pkg _ _ _) <- InstallPlan.toList installPlan ]

    let requireDownloading = [ (pkg, locm) | (pkg, locm, Nothing) <- pkgslocs ]
        alreadyDownloaded  = [ (pkg, loc)  | (pkg, _, Just loc)   <- pkgslocs ]

    -- Download the ones we need
    --
    newlyDownloaded <-
      if null requireDownloading
        then return []
        else liftIO $
              withRepoCtx $ \repoctx ->
                sequence
                  [ do loc <- fetchPackage verbosity repoctx locm
                       return (pkg, loc)
                  | (pkg, locm) <- requireDownloading ]

    -- Get the hashes of all the tarball packages (i.e. not local dir pkgs)
    --
    let pkgsTarballs =
          [ (packageId pkg, tarball)
          | (pkg, srcloc) <- newlyDownloaded ++ alreadyDownloaded
          , tarball <- maybeToList (tarballFileLocation srcloc) ]

    monitorFiles [ MonitorFile tarball | (_pkgid, tarball) <- pkgsTarballs ]

    liftM Map.fromList $ liftIO $
      sequence
        [ do srchash <- readFileHashValue tarball
             return (pkgid, srchash)
        | (pkgid, tarball) <- pkgsTarballs ]
  where
    tarballFileLocation (LocalUnpackedPackage _dir)      = Nothing
    tarballFileLocation (LocalTarballPackage    tarball) = Just tarball
    tarballFileLocation (RemoteTarballPackage _ tarball) = Just tarball
    tarballFileLocation (RepoTarballPackage _ _ tarball) = Just tarball


-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

planPackages :: Compiler
             -> Platform
             -> Solver -> ProjectConfigSolver
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> [SourcePackage]
             -> Map PackageName (Map OptionalStanza Bool)
             -> Progress String String InstallPlan
planPackages comp platform solver solverconfig
             installedPkgIndex sourcePkgDb
             localPackages pkgStanzasEnable =

    resolveDependencies
      platform (compilerInfo comp)
      solver
      resolverParams

  where

    --TODO: [nice to have] disable multiple instances restriction in the solver, but then
    -- make sure we can cope with that in the output.
    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . setAvoidReinstalls avoidReinstalls --TODO: [required eventually] should only be configurable for custom installs

      . setShadowPkgs shadowPkgs --TODO: [required eventually] should only be configurable for custom installs

      . setStrongFlags strongFlags

      . setPreferenceDefault (if upgradeDeps then PreferAllLatest
                                             else PreferLatestForSelected)
                             --TODO: [required eventually] decide if we need to prefer installed for global packages?

      . removeUpperBounds allowNewer

      . addDefaultSetupDepends (defaultSetupDeps platform
                              . PD.packageDescription . packageDescription)

      . addPreferences
          -- preferences from the config file or command line
          [ PackageVersionPreference name ver
          | Dependency name ver <- projectConfigSolverPreferences ]

      . addConstraints
          -- version constraints from the config file or command line
            [ LabeledPackageConstraint (userToPackageConstraint pc) src
            | (pc, src) <- projectConfigSolverConstraints ]

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
          --TODO: [nice to have] this just applies all flags to all targets which
          -- is silly. We should check if the flags are appropriate
          [ LabeledPackageConstraint
              (PackageConstraintFlags pkgname flags)
              ConstraintSourceConfigFlagOrTarget
          | let flags = projectConfigConfigurationsFlags
          , not (null flags)
          , pkg <- localPackages
          , let pkgname = packageName pkg ]

      $ standardInstallPolicy
        installedPkgIndex sourcePkgDb
        (map SpecificSourcePackage localPackages)

    reorderGoals     = fromFlag projectConfigSolverReorderGoals
    independentGoals = fromFlag projectConfigSolverIndependentGoals
    avoidReinstalls  = fromFlag projectConfigSolverAvoidReinstalls
    shadowPkgs       = fromFlag projectConfigSolverShadowPkgs
    strongFlags      = fromFlag projectConfigSolverStrongFlags
    maxBackjumps     = fromFlag projectConfigSolverMaxBackjumps
    upgradeDeps      = fromFlag projectConfigSolverUpgradeDeps
    allowNewer       = fromFlag projectConfigSolverAllowNewer

    ProjectConfigSolver{
      projectConfigSolverConstraints,
      projectConfigSolverPreferences,
      projectConfigConfigurationsFlags,

--      projectConfigSolverReinstall,  --TODO: [required eventually] check not configurable for local mode?
      projectConfigSolverReorderGoals,
      projectConfigSolverIndependentGoals,
      projectConfigSolverAvoidReinstalls,
      projectConfigSolverShadowPkgs,
      projectConfigSolverStrongFlags,
      projectConfigSolverMaxBackjumps,
      projectConfigSolverUpgradeDeps,
      projectConfigSolverAllowNewer
    } = solverconfig

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
  -> [SourcePackage]
  -> Map PackageId PackageSourceHash
  -> InstallDirs.InstallDirTemplates
  -> PackageConfigShared
  -> PackageConfig
  -> Map PackageName PackageConfig
  -> (ElaboratedInstallPlan, ElaboratedSharedConfig)
elaborateInstallPlan platform compiler progdb
                     DistDirLayout{..}
                     cabalDirLayout@CabalDirLayout{cabalStorePackageDB}
                     solverPlan localPackages
                     sourcePackageHashes
                     defaultInstallDirs
                     sharedPackageConfig
                     localPackagesConfig
                     perPackageConfig =
    (elaboratedInstallPlan, elaboratedSharedConfig)
  where
    elaboratedSharedConfig =
      ElaboratedSharedConfig {
        pkgConfigPlatform         = platform,
        pkgConfigCompiler         = compiler,
        pkgConfigProgramDb        = progdb
      }

    elaboratedInstallPlan =
      flip InstallPlan.mapPreservingGraph solverPlan $ \mapDep planpkg ->
        case planpkg of
          InstallPlan.PreExisting pkg ->
            InstallPlan.PreExisting pkg

          InstallPlan.Configured  pkg ->
            InstallPlan.Configured
              (elaborateConfiguredPackage (fixupDependencies mapDep pkg))

          _ -> error "elaborateInstallPlan: unexpected package state"

    -- remap the installed package ids of the direct deps, since we're
    -- changing the installed package ids of all the packages to use the
    -- final nix-style hashed ids.
    fixupDependencies mapDep
       (ConfiguredPackage pkg flags stanzas deps) =
        ConfiguredPackage pkg flags stanzas deps'
      where
        deps' = fmap (map (\d -> d { confInstId = mapDep (confInstId d) })) deps

    elaborateConfiguredPackage :: ConfiguredPackage
                               -> ElaboratedConfiguredPackage
    elaborateConfiguredPackage
        pkg@(ConfiguredPackage (SourcePackage pkgid gdesc srcloc descOverride)
                               flags stanzas deps) =
        elaboratedPackage
      where
        -- Knot tying: the final elaboratedPackage includes the
        -- pkgInstalledId, which is calculated by hashing many
        -- of the other fields of the elaboratedPackage.
        --
        elaboratedPackage = ElaboratedConfiguredPackage {..}

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
            Map.fromList $ [ (TestStanzas,  v) | v <- maybeToList tests ]
                        ++ [ (BenchStanzas, v) | v <- maybeToList benchmarks ]
          where
            tests, benchmarks :: Maybe Bool
            tests      = perPkgOptionMaybe pkgid packageConfigTests
            benchmarks = perPkgOptionMaybe pkgid packageConfigBenchmarks

        -- These two sometimes get adjusted later
        pkgStanzasEnabled   = Set.empty
        pkgBuildTargets     = []

        pkgSourceLocation   = srcloc
        pkgSourceHash       = Map.lookup pkgid sourcePackageHashes
        pkgBuildStyle       = if shouldBuildInplaceOnly pkg
                                then BuildInplaceOnly else BuildAndInstall
        pkgBuildPackageDBStack    = buildAndRegisterDbs
        pkgRegisterPackageDBStack = buildAndRegisterDbs
        pkgRequiresRegistration   = isJust (Cabal.condLibrary gdesc)

        pkgSetupScriptStyle       = packageSetupScriptStyle pkgDescription
        pkgSetupScriptCliVersion  = packageSetupScriptSpecVersion
                                      pkgDescription deps
        pkgSetupPackageDBStack    = buildAndRegisterDbs        

        pkgDescriptionOverride    = descOverride

        pkgVanillaLib    = sharedOptionFlag True  packageConfigVanillaLib --TODO: [required feature]: also needs to be handled recursively
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

        pkgConfigureScriptArgs = perPkgOptionList pkgid packageConfigConfigureArgs
        pkgExtraLibDirs        = perPkgOptionList pkgid packageConfigExtraLibDirs
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

        buildAndRegisterDbs
          | shouldBuildInplaceOnly pkg = inplacePackageDbs
          | otherwise                  = storePackageDbs

    sharedOptionFlag  :: a         -> (PackageConfigShared -> Flag a) -> a
    perPkgOptionFlag  :: PackageId -> a ->  (PackageConfig -> Flag a) -> a
    perPkgOptionMaybe :: PackageId ->       (PackageConfig -> Flag a) -> Maybe a
    perPkgOptionList  :: PackageId ->       (PackageConfig -> [a])    -> [a]

    sharedOptionFlag def f = fromFlagOrDefault def shared
      where
        shared = f sharedPackageConfig

    perPkgOptionFlag  pkgid def f = fromFlagOrDefault def (lookupPerPkgOption pkgid f)
    perPkgOptionMaybe pkgid     f = flagToMaybe (lookupPerPkgOption pkgid f)
    perPkgOptionList  pkgid     f = lookupPerPkgOption pkgid f

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
          [ fakeUnitId (packageId pkg)
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
            pkgSharedLib = flagToMaybe (packageConfigSharedLib sharedPackageConfig)
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


------------------------------------------------------------------------------
-- * Install plan pruning
------------------------------------------------------------------------------

-- | The particular bits within a package that we can build.
--
data ComponentTarget =
     -- | Build all the default components. This means the lib and exes, but
     -- can also mean the testsuites and benchmarks if the user requested
     -- enabling tests and benchmarks. 
     BuildAllDefaultComponents
   | BuildSpecificComponent Cabal.BuildTarget
  deriving (Eq, Show)

-- | Given a set of package targets (and optionally component targets within
-- those packages), take the subset of the install plan needed to build those
-- targets. Also, update the package config to specify which optional stanzas
-- to enable, and which targets within each package to build.
--
pruneInstallPlanToTargets :: Map InstalledPackageId [ComponentTarget]
                          -> ElaboratedInstallPlan -> ElaboratedInstallPlan
pruneInstallPlanToTargets perPkgTargetsMap =
    either (\_ -> assert False undefined) id
  . InstallPlan.new False
  . PackageIndex.fromList
    -- We have to do this in two passes
  . pruneInstallPlanPass2 perPkgTargetsMap
  . pruneInstallPlanPass1 perPkgTargetsMap
  . InstallPlan.toList

-- The first pass does three things:
--
-- * a first go at determining which optional stanzas (testsuites, benchmarks)
--   are needed. (We have a second go in the next pass.)
-- * then prune dependencies used only by unneeded optional stanzas
-- * then take the dependency closure, given the pruned deps
--
pruneInstallPlanPass1 :: Map InstalledPackageId [ComponentTarget]
                      -> [ElaboratedPlanPackage]
                      -> [ElaboratedPlanPackage]
pruneInstallPlanPass1 perPkgTargetsMap pkgs =
    map fst $
    dependencyClosure
      (installedPackageId . fst)  -- the pkg id
      snd                         -- the pruned deps
      [ (pkg', pruneOptionalDependencies pkg')
      | pkg <- pkgs
      , let pkg' = mapConfiguredPackage pruneOptionalStanzas pkg ]
      (Map.keys perPkgTargetsMap)
  where
    -- Decide whether or not to enable testsuites and benchmarks
    --
    -- The testsuite and benchmark targets are somewhat special in that we need
    -- to configure the packages with them enabled, and we need to do that even
    -- if we only want to build one of several testsuites.
    -- 
    -- There are two cases in which we will enable the testsuites (or
    -- benchmarks): if one of the targets is a testsuite, or if all of the
    -- testsuite depencencies are already cached in the store. The rationale
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
        stanzas = optionalStanzasRequiredByTargets  targets
               <> optionalStanzasRequestedByDefault pkg
               <> optionalStanzasWithDepsAvailable availablePkgs pkg

        targets = fromMaybe []
                $ Map.lookup (installedPackageId pkg) perPkgTargetsMap

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

    optionalStanzasRequiredByTargets :: [ComponentTarget]
                                     -> Set OptionalStanza
    optionalStanzasRequiredByTargets ctargets =
      Set.fromList
        [ stanza
        | BuildSpecificComponent c <- ctargets
        , let cname = Cabal.buildTargetComponentName c
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


-- The second pass does two things:
--
-- * a second go at deciding which optional stanzas to enable
-- * set which targets within each package to actually build
--
-- Achieving sticky behaviour with enabling\/disabling optional stanzas is
-- tricky. The first approximation was handled by the first pass above, but
-- it's not quite enough. That pass will enable stanzas if all of the deps
-- of the optional stanza are already instaled /in the store/. That's important
-- but it does not account for depencencies that get built inplace as part of
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
-- That depends on which targets we pick in the second pass.
--
pruneInstallPlanPass2 :: Map InstalledPackageId [ComponentTarget]
                      -> [ElaboratedPlanPackage]
                      -> [ElaboratedPlanPackage]
pruneInstallPlanPass2 perPkgTargetsMap pkgs =
    map (mapConfiguredPackage setBuildTargets) pkgs
  where
    setBuildTargets pkg =
        pkg {
          pkgStanzasEnabled = stanzas,
          pkgDependencies   = CD.filterDeps keepNeeded (pkgDependencies pkg),
          pkgBuildTargets   = targets
        }
      where
        stanzas :: Set OptionalStanza
        stanzas = pkgStanzasEnabled pkg
               <> optionalStanzasWithDepsAvailable availablePkgs pkg

        keepNeeded (CD.ComponentTest  _) _ = TestStanzas  `Set.member` stanzas
        keepNeeded (CD.ComponentBench _) _ = BenchStanzas `Set.member` stanzas
        keepNeeded _                     _ = True

        targets :: [Cabal.BuildTarget]
        targets  | Set.null stanzas
                 , ctargets == [BuildAllDefaultComponents]
                   -- common special case of building only the default things
                 = []

                 | otherwise
                 = nubBuildTargets
                 . concatMap (expandBuildTarget pkg) 
                 $ ctargets

        ctargets :: [ComponentTarget]
        ctargets = [ BuildAllDefaultComponents
                 -- all the normal stuff, if anything needs this pkg
                 | installedPackageId pkg `Set.member` hasReverseLibDeps
                 ]
                 -- plus any specific targets
              ++ perPkgTargets pkg

    availablePkgs :: Set InstalledPackageId
    availablePkgs = Set.fromList (map installedPackageId pkgs)

    hasReverseLibDeps :: Set InstalledPackageId
    hasReverseLibDeps =
      Set.fromList [ depid | pkg <- pkgs
                           , depid <- CD.flatDeps (depends pkg) ]

    -- Given the specification of what targets to build, elaborate this into
    -- more specific targets.
    --
    -- In particular, the "all components" target expands to all the lib and
    -- exe targets, but sometimes also to the testsuites and benchmarks if the
    -- user specifically requested them.
    --
    expandBuildTarget :: ElaboratedConfiguredPackage
                      -> ComponentTarget -> [Cabal.BuildTarget]
    expandBuildTarget _  (BuildSpecificComponent t) = [t]
    expandBuildTarget pkg BuildAllDefaultComponents =
        [ BuildTargetComponent cname
        | c <- Cabal.pkgComponents (pkgDescription pkg)
        , PD.buildable (Cabal.componentBuildInfo c)
        , let cname = Cabal.componentName c
        , enabledOptionalStanza cname
        ]
      where
        enabledOptionalStanza cname =
          case componentOptionalStanza cname of
            Nothing     -> True
            Just stanza -> Map.lookup stanza (pkgStanzasRequested pkg)
                        == Just True

    nubBuildTargets :: [Cabal.BuildTarget] -> [Cabal.BuildTarget]
    nubBuildTargets =
        concatMap (wholeComponentOverrides . map snd)
      . groupBy ((==)    `on` fst)
      . sortBy  (compare `on` fst)
      . map (\bt -> (Cabal.buildTargetComponentName bt, bt))

    -- If we're building the whole component then that the only target all we
    -- need, otherwise we can have several targets within the component.
    wholeComponentOverrides bts =
      case [ bt | bt@(BuildTargetComponent _) <- bts ] of
        (bt:_) -> [bt]
        []     -> bts

    -- utils:

    perPkgTargets pkg = fromMaybe []
                      $ Map.lookup (installedPackageId pkg) perPkgTargetsMap


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

-- | There are four major cases for Setup.hs handling:
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
data SetupScriptStyle = SetupCustomExplicitDeps
                      | SetupCustomImplicitDeps
                      | SetupNonCustomExternalLib
                      | SetupNonCustomInternalLib
  deriving (Eq, Show, Generic)

instance Binary SetupScriptStyle


packageSetupScriptStyle :: PD.PackageDescription -> SetupScriptStyle
packageSetupScriptStyle pkg
  | buildType == PD.Custom
  , isJust (PD.setupBuildInfo pkg)
  = SetupCustomExplicitDeps

  | buildType == PD.Custom
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
defaultSetupDeps :: Platform -> PD.PackageDescription -> [Dependency]
defaultSetupDeps platform pkg =
    case packageSetupScriptStyle pkg of

      -- For packages with build type custom that do not specify explicit
      -- setup dependencies, we add a dependency on Cabal and a number
      -- of other packages.
      SetupCustomImplicitDeps ->
        [ Dependency depPkgname anyVersion
        | depPkgname <- legacyCustomSetupPkgs platform ] ++
        -- The Cabal dep is slightly special:
        --  * we omit the dep for the Cabal lib itself (since it bootstraps),
        --  * we constrain it to be less than 1.23 since all packages
        --    relying on later Cabal spec versions are supposed to use
        --    explit setup deps. Having this constraint also allows later
        --    Cabal lib versions to make breaking API changes without breaking
        --    all old Setup.hs scripts.
        [ Dependency cabalPkgname cabalConstraint
        | packageName pkg /= cabalPkgname ]
        where
          cabalConstraint   = orLaterVersion (PD.specVersion pkg)
                                `intersectVersionRanges`
                              earlierVersion cabalCompatMaxVer
          cabalCompatMaxVer = Version [1,23] []
 
      -- For other build types (like Simple) if we still need to compile an
      -- external Setup.hs, it'll be one of the simple ones that only depends
      -- on Cabal and base.
      SetupNonCustomExternalLib ->
        [ Dependency cabalPkgname cabalConstraint
        , Dependency basePkgname  anyVersion ]
        where
          cabalConstraint = orLaterVersion (PD.specVersion pkg)
  
      -- The internal setup wrapper method has no deps at all.
      SetupNonCustomInternalLib -> []

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
                              => PD.PackageDescription
                              -> ComponentDeps [pkg]
                              -> Version
packageSetupScriptSpecVersion pkg deps =
    case packageSetupScriptStyle pkg of

      -- We're going to be using the internal Cabal library, so the spec
      -- version of that is simply the version of the Cabal library that
      -- cabal-install has been built with.
      SetupNonCustomInternalLib ->
        cabalVersion

      -- If we happen to be building the Cabal lib itself then because that
      -- bootstraps itself then we use the version of the lib we're building.
      SetupCustomImplicitDeps | packageName pkg == cabalPkgname ->
        packageVersion pkg

      -- In all other cases we have a look at what version of the Cabal lib
      -- the solver picked. Or if it didn't depend on Cabal at all (which is
      -- very rare) then we look at the .cabal file to see what spec version
      -- it declares.
      _ -> case find ((cabalPkgname ==) . packageName) (CD.setupDeps deps) of 
             Just dep -> packageVersion dep
             Nothing  -> PD.specVersion pkg


cabalPkgname, basePkgname :: PackageName
cabalPkgname = PackageName "Cabal"
basePkgname  = PackageName "base"


legacyCustomSetupPkgs :: Platform -> [PackageName]
legacyCustomSetupPkgs (Platform _ os) =
    map PackageName $
        [ "array", "base", "binary", "bytestring", "containers"
        , "deepseq", "directory", "filepath", "pretty"
        , "process", "time" ]
     ++ [ "Win32" | os == Windows ]
     ++ [ "unix"  | os /= Windows ]

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
setupHsScriptOptions (ReadyPackage ElaboratedConfiguredPackage{..} deps)
                     ElaboratedSharedConfig{..} srcdir builddir
                     isParallelBuild cacheLock =
    SetupScriptOptions {
      useCabalVersion          = thisVersion pkgSetupScriptCliVersion,
      useCabalSpecVersion      = Just pkgSetupScriptCliVersion,
      useCompiler              = Just pkgConfigCompiler,
      usePlatform              = Just pkgConfigPlatform,
      usePackageDB             = pkgSetupPackageDBStack,
      usePackageIndex          = Nothing,
      useDependencies          = [ (installedPackageId ipkg, packageId ipkg)
                                 | ipkg <- CD.setupDeps deps ],
      useDependenciesExclusive = True,
      useVersionMacros         = pkgSetupScriptStyle == SetupCustomExplicitDeps,
      useProgramConfig         = pkgConfigProgramDb,
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
                         pkg@ElaboratedConfiguredPackage{..}
                         pkgdeps)
                      sharedConfig@ElaboratedSharedConfig{..}
                      verbosity builddir =
    assert (sanityCheckElaboratedConfiguredPackage sharedConfig pkg)
    Cabal.ConfigFlags {..}
  where
    configDistPref            = toFlag builddir
    configVerbosity           = toFlag verbosity

    configIPID                = toFlag (display (installedUnitId pkg))

    configProgramPaths        = programDbProgramPaths pkgConfigProgramDb
    configProgramArgs         = programDbProgramArgs  pkgConfigProgramDb
    configProgramPathExtra    = programDbPathExtra    pkgConfigProgramDb
    configHcFlavor            = toFlag (compilerFlavor pkgConfigCompiler)
    configHcPath              = mempty -- use configProgramPaths instead
    configHcPkg               = mempty -- use configProgramPaths instead

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

    configConfigurationsFlags = pkgFlagAssignment
    configConfigureArgs       = pkgConfigureScriptArgs
    configExtraLibDirs        = pkgExtraLibDirs
    configExtraIncludeDirs    = pkgExtraIncludeDirs
    configProgPrefix          = maybe mempty toFlag pkgProgPrefix
    configProgSuffix          = maybe mempty toFlag pkgProgSuffix

    configInstallDirs         = fmap (toFlag . InstallDirs.toPathTemplate)
                                     pkgInstallDirs

    -- we only use configDependencies, unless we're talking to an old Cabal
    -- in which case we use configConstraints
    configDependencies        = [ (packageName (Installed.sourcePackageId deppkg),
                                  Installed.installedUnitId deppkg)
                                | deppkg <- CD.nonSetupDeps pkgdeps ]
    configConstraints         = [ thisPackageVersion (packageId deppkg)
                                | deppkg <- CD.nonSetupDeps pkgdeps ]

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
    configPrograms            = error "setupHsConfigureFlags: configPrograms"

    programDbProgramPaths db =
      [ (programId prog, programPath prog)
      | prog <- configuredPrograms db ]

    programDbProgramArgs db =
      [ (programId prog, programOverrideArgs prog)
      | prog <- configuredPrograms db ]

    programDbPathExtra db =
      case getProgramSearchPath db of
        ProgramSearchPathDefault : extra ->
          toNubList [ dir | ProgramSearchPathDir dir <- extra ]
        _ -> error $ "setupHsConfigureFlags: we cannot currently cope with a "
                  ++ "search path that does not start with the system path"
                  -- the Setup.hs interface only has --extra-prog-path
                  -- so we cannot put things before the $PATH, only after


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
      buildNumJobs      = mempty, --TODO: [nice to have] sometimes want to use toFlag (Just numBuildJobs),
      buildArgs         = mempty  -- unused, passed via args not flags
    }


setupHsBuildArgs :: ElaboratedConfiguredPackage
                 -> [String]
setupHsBuildArgs pkg@ElaboratedConfiguredPackage{pkgBuildTargets} =
    showBuildTargets pkgBuildTargets
  where
    --TODO: [code cleanup] this is all a little hacky
    showBuildTargets = map $ \t ->
      Cabal.showBuildTarget (qlBuildTarget t) (packageId pkg) t

    qlBuildTarget Cabal.BuildTargetComponent{} = QL2
    qlBuildTarget _                            = QL3


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
      regVerbosity   = toFlag verbosity
    }

{- TODO: [required feature]
setupHsHaddockFlags :: ElaboratedConfiguredPackage
                    -> ElaboratedSharedConfig
                    -> Verbosity
                    -> FilePath
                    -> Cabal.HaddockFlags
setupHsHaddockFlags _ _ verbosity builddir =
    Cabal.HaddockFlags {
    }

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
    relevantDeps  CD.ComponentLib      = True
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

