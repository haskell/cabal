{-# LANGUAGE BangPatterns, DeriveGeneric, GeneralizedNewtypeDeriving,
             RecordWildCards, NamedFieldPuns,
             ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Install
-- Copyright   :  (c) 2005 David Himmelstrup
--                    2007 Bjorn Bringert
--                    2007-2010 Duncan Coutts
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An experimental new UI for cabal for working with multiple packages
-----------------------------------------------------------------------------
module Distribution.Client.MultiPkg (
    -- * High level things
    configure,
    build,
  ) where

import           Distribution.Client.Types hiding (BuildResult, BuildSuccess(..), BuildFailure(..), DocsResult(..), TestsResult(..))
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan, InstallPlan )
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.Dependency
import           Distribution.Client.Dependency.Types
import qualified Distribution.Client.ComponentDeps as ComponentDeps
import qualified Distribution.Client.IndexUtils as IndexUtils
import           Distribution.Client.Targets (UserConstraint, pkgSpecifierTarget, userToPackageConstraint)
import           Distribution.Client.DistDirLayout
import           Distribution.Client.FileStatusCache
import           Distribution.Client.Glob
import           Distribution.Client.SetupWrapper
import           Distribution.Client.JobControl
import           Distribution.Client.HttpUtils
import           Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar
import           Distribution.Client.Config ({-SavedConfig(..),-} defaultCabalDir)
import           Distribution.Client.Setup hiding (packageName)
import           Distribution.Client.BuildReports.Types (ReportLevel)

--import           Distribution.Client.Sandbox
--import           Distribution.Client.Sandbox.Types
--import           Distribution.Client.Sandbox.Index as Index

import           Distribution.Package
import           Distribution.System
import qualified Distribution.PackageDescription as Cabal
import           Distribution.PackageDescription (FlagAssignment)
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import           Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
--import qualified Distribution.Client.PackageIndex as Client.PackageIndex
import           Distribution.Simple.Compiler hiding (Flag)
import           Distribution.Simple.Program
import           Distribution.Simple.Program.Db
import qualified Distribution.Simple.Setup as Cabal
import           Distribution.Simple.Setup (Flag, toFlag, flagToMaybe, fromFlag, fromFlagOrDefault, HaddockFlags(..))
import           Distribution.Utils.NubList (NubList, fromNubList)
import qualified Distribution.Simple.Configure as Cabal
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.InstallDirs as InstallDirs
import           Distribution.Simple.InstallDirs (InstallDirs, PathTemplate)

--import           Distribution.Client.Setup ()
import           Distribution.Simple.Utils hiding (matchFileGlob)
import           Distribution.Version
import           Distribution.Verbosity
import           Distribution.Text
import           Distribution.ParseUtils ( showPWarning )

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import qualified Data.Digest.Pure.SHA as SHA

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State as State
import           Control.Exception
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.List
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Function (on)

import           Data.Binary
import           GHC.Generics (Generic)

import           System.FilePath
import           System.IO
import           System.Directory

import Data.Time
import Debug.Trace


------------------------------------------------------------------------------
-- * Understanding this module
------------------------------------------------------------------------------

-- This module deals with building and incrementally rebuilding a collection
-- of packages. It is what backs the "cabal build" and "configure" commands,
-- as well as being a core part of "run", "test", "bench" and others. 
--
-- The primary thing is in fact rebuilding (and trying to make that quick by
-- not redoing unnecessary work), so building from scratch is just a special
-- case.
--
-- The build process and the code can be understood by breaking it down into
-- three major parts:
--
-- * The 'ElaboratedInstallPlan' type
--
-- * The "what to do" phase, where we look at the all input configuration
--   (project files, .cabal files, command line etc) and produce a detailed
--   plan of what to do -- the 'ElaboratedInstallPlan'.
--
-- * The "do it" phase, where we take the 'ElaboratedInstallPlan' and we
-- re-execute it.
--
-- As far as possible, the "what to do" phase embodies all the policy, leaving
-- the "do it" phase policy free. The first phase contains more of the
-- complicated logic, but it is contained in code that is either pure or just
-- has read effects (except cache updates). Then the second phase does all the
-- actions to build packages, but as far as possible it just follows the
-- instructions and avoids any logic for deciding what to do (apart from
-- recompilation avoidance in executing the plan).
--
-- This division helps us keep the code under control, making it easier to
-- understand, test and debug. So when you are extending this module, please
-- think about which parts of your change belong in which part. It is
-- perfectly ok to extend the description of what to do (i.e. the 
-- 'ElaboratedInstallPlan') if that helps keep the policy decisions in the
-- first phase. Also, the second phase does not have direct access to any of
-- the input configuration anyway; all the information has to flow via the
-- 'ElaboratedInstallPlan'.


------------------------------------------------------------------------------
-- * Top level commands: build and configure
------------------------------------------------------------------------------

build :: Verbosity
      -> GlobalFlags
      -> ConfigFlags
      -> ConfigExFlags
      -> InstallFlags
      -> HaddockFlags
      -> [String]
      -> IO ()
build verbosity
      globalFlags
      configFlags configExFlags
      installFlags haddockFlags
      userTargets = do

    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- findProjectRoot
    let distDirLayout = defaultDistDirLayout projectRootDir

    let projectConfigCLI = convertCommandLineFlags 
                             globalFlags
                             configFlags configExFlags
                             installFlags haddockFlags

    -- The (re)build is split into two major parts:

    -- Phase 1: decide what to do.
    --
    -- Take all input configuration and make a description of what to do.
    --
    (elaboratedInstallPlan, sharedPackageConfig, buildFlags) <-
      rebuildInstallPlan verbosity
                         projectRootDir distDirLayout cabalDirLayout
                         projectConfigCLI

    -- The description of what to do is represented by an
    -- 'ElaboratedInstallPlan' (plus an 'ElaboratedSharedConfig' for those
    -- bits that are common between all packages.
    --

    -- Phase 2: now do it.
    --
    -- Execute all or parts of the description of what to do to build or
    -- rebuild the various packages needed.
    --
    rebuildTargets verbosity
                   distDirLayout cabalDirLayout
                   elaboratedInstallPlan
                   sharedPackageConfig
                   buildFlags
                   userTargets

    --TODO: may want to do some earlier parsing/validation of the userTargets
    -- though fully resolving what they refer to requires info from the env
    -- or config (e.g. knowing which packages are in the plan, where they live)

    -- Note that it is a deliberate design choice that the 'userTargets' is
    -- not passed to phase 1, and the various bits of input config is not
    -- passed to phase 2.
    --
    -- We make the install plan without looking at the particular targets the
    -- user asks us to build. The set of available things we can build is
    -- discovered from the env and config and is used to make the install plan.
    -- The targets just tell us which parts of the install plan to execute.
    --
    -- Conversely, executing the plan does not directly depend on any of the
    -- input config. The bits that are needed (or better, the decisions based
    -- on it) all go into the install plan.

    -- Notionally, the 'BuildFlags' should be things that do not affect what
    -- we build, just how we do it. These ones of course do 
    -- TODO: The current 'BuildFlags' is a mix of config and build-only, due
    -- to the old command line interface and needs to be cleaned up.

-- To a first approximation, configure just runs the first phase of 'build'
-- where we bring the install plan up to date (thus checking that it's
-- possible).
--
-- The only difference is that 'configure' also allows the user to specify
-- some extra local temporary config flags.
--

configure :: Verbosity
          -> GlobalFlags
          -> ConfigFlags
          -> ConfigExFlags
          -> InstallFlags
          -> Cabal.HaddockFlags
          -> [String]
          -> IO ()
configure verbosity
          globalFlags
          configFlags configExFlags
          installFlags haddockFlags
          _extraArgs = do

    -- In this prototype version we don't yet support extra local temporary
    -- config, so it's really just the first phase of build.

    projectRootDir <- findProjectRoot
    cabalDir       <- defaultCabalDir
    let distDirLayout  = defaultDistDirLayout projectRootDir
        cabalDirLayout = defaultCabalDirLayout cabalDir
        projectConfigCLI = convertCommandLineFlags 
                             globalFlags
                             configFlags configExFlags
                             installFlags haddockFlags

    _ <- rebuildInstallPlan verbosity
                            projectRootDir distDirLayout cabalDirLayout
                            projectConfigCLI

    return ()


-- | Find the root of this project.
--
-- Searches for an explicit @cabal.project@ file, in the current directory or
-- parent directories. If no project file is found then the current dir is the
-- project root (and the project will use an implicit config).
--
findProjectRoot :: IO FilePath
findProjectRoot = do

    curdir  <- getCurrentDirectory
    homedir <- getHomeDirectory

    -- search upwards if we get to the users home dir or the filesystem root, then use the current dir
    let probe dir | isDrive dir || dir == homedir
                  = return curdir -- implicit project root
        probe dir = do
          exists <- doesFileExist (dir </> "cabal.project")
          if exists
            then return dir       -- explicit project root
            else probe (takeDirectory dir)

    probe curdir
   --TODO: add compat support for old style sandboxes


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

type SolverInstallPlan
   = InstallPlan --TODO: redefine locally or move def to solver interface

--TODO: decide if we really need this, there's not much in it, and in principle
--      even platform and compiler could be different if we're building things
--      like a server + client with ghc + ghcjs
data ElaboratedSharedConfig
   = ElaboratedSharedConfig {

       pkgConfigPlatform         :: Platform,
       pkgConfigCompiler         :: Compiler, --TODO: replace with CompilerInfo
       pkgConfigProgramDb        :: ProgramDb, --TODO: no Eq instance
       --TODO: ^^ binary instance does not preserve the prog paths
       --      perhaps should keep the configured progs separately

       pkgConfigLibraryProfiling :: Bool

     }
  deriving (Show, Generic)

instance Binary ElaboratedSharedConfig

       -- ambient package db stack to use for Setup.hs
       -- package db stack to use for compiling against
       -- package db stack to use for registering into
       -- the compiler
       -- the platform
       -- the program db

data ElaboratedConfiguredPackage
   = ElaboratedConfiguredPackage {

       pkgInstalledId :: InstalledPackageId,
       pkgSourceId    :: PackageId,

       -- | TODO: we don't need this, just a few bits from it:
       --   build type, spec version
       pkgDescription :: Cabal.GenericPackageDescription,

       -- | A total flag assignment for the package
       pkgFlagAssignment   :: Cabal.FlagAssignment,

       -- | Which optional stanzas are enabled (testsuites, benchmarks)
       pkgTestsuitesEnable :: Bool,
       pkgBenchmarksEnable :: Bool,
       pkgEnabledStanzas   :: [OptionalStanza], --TODO: eliminate 

       -- | The exact dependencies (on other plan packages)
       --
       pkgDependencies     :: ComponentDeps.ComponentDeps [InstalledPackageId],
       --TODO: ^^ do we need this given that ReadyPackage includes it?

       -- | Where the package comes from, e.g. tarball, local dir etc. This
       --   is not the same as where it may be unpacked to for the build.
       pkgSourceLocation :: PackageLocation (Maybe FilePath),

       --pkgSourceDir ? -- currently passed in later because they can use temp locations
       --pkgBuildDir  ? -- but could in principle still have it here, with optional instr to use temp loc

       pkgBuildStyle             :: BuildStyle,

       pkgSetupPackageDBStack    :: PackageDBStack,
       pkgBuildPackageDBStack    :: PackageDBStack,
       pkgRegisterPackageDBStack :: PackageDBStack,

       -- | The package contains a library and so must be registered
       pkgRequiresRegistration :: Bool,
       pkgHasExplicitSetupDeps :: Bool, --TODO: maybe? currently unused

       pkgDescriptionOverride  :: Maybe LBS.Char8.ByteString,

       pkgVanillaLib           :: Bool,
       pkgSharedLib            :: Bool,
       pkgDynExe               :: Bool,
       pkgGHCiLib              :: Bool,
       pkgProfLib              :: Bool,
       pkgProfExe              :: Bool,
       pkgProfLibDetail        :: ProfDetailLevel,
       pkgProfExeDetail        :: ProfDetailLevel,
       pkgLibCoverage          :: Bool,
       pkgExeCoverage          :: Bool,
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

       pkgInstallDirs           :: InstallDirs.InstallDirTemplates -- still $prefix relative
{-
       -- Setup.hs related things:

       -- | The version of the Cabal spec that this package claims to support.
       pkgCabalSpecVersion    :: Version,
 
       -- | The .cabal file build type 
       pkgSetupBuildType      :: Cabal.BuildType,

       -- | The version of the Cabal command line interface that this package
       -- is using. In practice this means the version of the Cabal lib it
       -- was built against (or if none or unknown the minimum spec version).
       pkgSetupHsCabalVersion :: Version
-}
     }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedConfiguredPackage

instance Package ElaboratedConfiguredPackage where
  packageId = pkgSourceId

instance HasInstalledPackageId ElaboratedConfiguredPackage where
  installedPackageId = pkgInstalledId

instance PackageFixedDeps ElaboratedConfiguredPackage where
  depends = pkgDependencies

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

type ElaboratedReadyPackage = GenericReadyPackage ElaboratedConfiguredPackage
                                                  InstalledPackageInfo
--TODO: this duplicates the InstalledPackageInfo quite a bit in an install plan
-- because the same ipkg is used by many packages. So the binary file will be big.
-- Could we keep just (ipkgid, deps) instead of the whole InstalledPackageInfo?
-- or transform to a shared form when serialising / deserialising

type BuildResult  = GenericBuildResult InstalledPackageInfo 
                                       BuildSuccess BuildFailure

data BuildSuccess = BuildOk DocsResult TestsResult
  deriving (Eq, Show, Generic)

data DocsResult  = DocsNotTried  | DocsFailed  | DocsOk
  deriving (Eq, Show, Generic)

data TestsResult = TestsNotTried | TestsOk
  deriving (Eq, Show, Generic)

data BuildFailure = PlanningFailed
                  | DependentFailed PackageId
                  | DownloadFailed  String
                  | UnpackFailed    String
                  | ConfigureFailed String
                  | BuildFailed     String
                  | TestsFailed     String
                  | InstallFailed   String
  deriving (Eq, Show, Generic)

instance Binary BuildFailure
instance Binary BuildSuccess
instance Binary DocsResult
instance Binary TestsResult


------------------------------------------------------------------------------
-- * Deciding what to do: making an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------

type CliConfig = ( ProjectConfigSolver
                 , PackageConfigShared
                 , PackageConfig
                 , ProjectConfigBuildOnly
                 )

rebuildInstallPlan :: Verbosity
                   -> FilePath -> DistDirLayout -> CabalDirLayout
                   -> CliConfig
                   -> IO ( ElaboratedInstallPlan
                         , ElaboratedSharedConfig
                         , ProjectConfigBuildOnly )
rebuildInstallPlan verbosity
                   projectRootDir
                   distDirLayout@DistDirLayout{..}
                   CabalDirLayout{..} = \cliConfig ->
    runRebuild $

    -- The overall improved plan is cached
    rerunIfChanged fileMonitorImprovedPlan projectRootDir
                   (valueMonitorImprovedPlan cliConfig) $ do

      -- And so is the elaborated plan that the improved plan based on
      (elaboratedPlan, elaboratedShared,
       buildConfig) <-
        rerunIfChanged fileMonitorElaboratedPlan projectRootDir
                       (valueMonitorImprovedPlan cliConfig) $ do

          projectConfig <- phaseReadProjectConfig cliConfig
          localPackages <- phaseReadLocalPackages projectConfig
          compilerEtc   <- phaseConfigureCompiler projectConfig
          solverPlan    <- phaseRunSolver         projectConfig compilerEtc
                                                  localPackages
          (elaboratedPlan,
           elaboratedShared) <- phaseElaboratePlan projectConfig compilerEtc
                                                   solverPlan localPackages

          return (elaboratedPlan, elaboratedShared,
                  projectConfigBuildOnly projectConfig)

      -- The improved plan changes each time we install something, whereas
      -- the underlying elaborated plan only changes when input config
      -- changes, so it's worth caching them separately.
      improvedPlan <- phaseImprovePlan elaboratedPlan elaboratedShared
      return (improvedPlan, elaboratedShared, buildConfig)

  where
    fileMonitorCompiler       = FileMonitorName (distProjectCacheFile "compiler")
    fileMonitorSolverPlan     = FileMonitorName (distProjectCacheFile "solver-plan")
    fileMonitorElaboratedPlan = FileMonitorName (distProjectCacheFile "elaborated-plan")
    fileMonitorImprovedPlan   = FileMonitorName (distProjectCacheFile "improved-plan")

    valueMonitorImprovedPlan ( cliConfigSolver
                             , cliConfigAllPackages
                             , cliConfigLocalPackages
                             , _cliConfigBuildOnly
                             )
                           = ( cliConfigSolver
                             , cliConfigAllPackages
                             , cliConfigLocalPackages
                             )

    -- Read the cabal.project (or implicit config) and combine it with
    -- arguments from the command line
    --
    phaseReadProjectConfig :: CliConfig -> Rebuild ProjectConfig
    phaseReadProjectConfig ( cliConfigSolver
                           , cliConfigAllPackages
                           , cliConfigLocalPackages
                           , cliConfigBuildOnly
                           ) = do
      liftIO $ do
        info verbosity "Project settings changed, reconfiguring..."
        createDirectoryIfMissingVerbose verbosity False distDirectory
        createDirectoryIfMissingVerbose verbosity False distProjectCacheDirectory

      readProjectConfig projectRootDir
                        cliConfigSolver
                        cliConfigAllPackages
                        cliConfigLocalPackages
                        cliConfigBuildOnly


    -- Look for all the cabal packages in the project
    -- some of which may be local src dirs, tarballs etc
    --
    phaseReadLocalPackages :: ProjectConfig
                           -> Rebuild [PackageSpecifier SourcePackage]
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
    phaseConfigureCompiler ProjectConfig{projectConfigAllPackages} =
        rerunIfChanged fileMonitorCompiler projectRootDir
                       (hcFlavor, hcPath, hcPkg) $
          configureCompiler verbosity (hcFlavor, hcPath, hcPkg)
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
                   -> [PackageSpecifier SourcePackage]
                   -> Rebuild SolverInstallPlan
    phaseRunSolver ProjectConfig{projectConfigSolver, projectConfigBuildOnly}
                   (compiler, platform, progdb)
                   localPackages =
        rerunIfChanged fileMonitorSolverPlan projectRootDir
                       (projectConfigSolver, projectConfigCacheDir,
                        localPackages,
                        compiler, platform, configuredPrograms progdb) $ do
          
          installedPkgIndex <- getInstalledPackages verbosity
                                                    compiler progdb platform
                                                    corePackageDbs
          sourcePkgDb       <- getSourcePackages    verbosity repos

          liftIO $ do
            solver <- chooseSolver verbosity solverpref (compilerInfo compiler)

            notice verbosity "Resolving dependencies..."
            foldProgress logMsg die return $
              planPackages compiler platform solver projectConfigSolver
                           installedPkgIndex sourcePkgDb
                           localPackages
      where
        corePackageDbs = [GlobalPackageDB]
        repos          = projectConfigRepos (fromFlag projectConfigCacheDir)
                                            projectConfigSolver
        solverpref     = fromFlag projectConfigSolverSolver 
        logMsg message rest = debugNoWrap verbosity message >> rest

        ProjectConfigBuildOnly {projectConfigCacheDir} = projectConfigBuildOnly
        ProjectConfigSolver {projectConfigSolverSolver} = projectConfigSolver


    -- Elaborate the solver's install plan to get a fully detailed plan. This
    -- version of the plan has the final nix-style hashed ids.
    --
    phaseElaboratePlan :: ProjectConfig
                       -> (Compiler, Platform, ProgramDb)
                       -> SolverInstallPlan
                       -> [PackageSpecifier SourcePackage]
                       -> Rebuild ( ElaboratedInstallPlan
                                  , ElaboratedSharedConfig )
    phaseElaboratePlan ProjectConfig {
                         projectConfigAllPackages,
                         projectConfigLocalPackages,
                         projectConfigSpecificPackage,
                         projectConfigBuildOnly
                       }
                       (compiler, platform, progdb)
                       solverPlan localPackages = do

        liftIO $ debug verbosity "Elaborating the install plan..."
        liftIO $ putStrLn $ InstallPlan.showInstallPlan solverPlan

        sourcePackageHashes <-
          liftIO $ getPackageSourceHashes verbosity mkTransport
                     (\(ConfiguredPackage pkg _ _ _) -> packageSource pkg)
                     solverPlan
                --TODO: monitor the downloaded files?

        defaultInstallDirs <- liftIO $ InstallDirs.defaultInstallDirs
                                         (compilerFlavor compiler) False False
        return $
          elaborateInstallPlan
            platform compiler progdb
            corePackageDbs storePackageDbs
            distDirLayout
            solverPlan
            localPackages
            sourcePackageHashes
            defaultInstallDirs
            projectConfigAllPackages
            projectConfigLocalPackages
            projectConfigSpecificPackage
      where
        corePackageDbs     = [ GlobalPackageDB ]
        storePackageDbs    = [ GlobalPackageDB, storePackageDb ]
        storePackageDb     = cabalStorePackageDB (compilerId compiler)

        mkTransport        = configureTransport verbosity preferredTransport
        preferredTransport = flagToMaybe (projectConfigHttpTransport
                                              projectConfigBuildOnly)


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

        liftIO $ notice verbosity "Improving the install plan..."
        liftIO $ putStrLn $ InstallPlan.showInstallPlan elaboratedPlan

        recreateDirectory verbosity True storeDirectory
        storePkgIndex <- getPackageDBContents verbosity
                                              compiler progdb platform
                                              storePackageDb

        let improvedPlan = improveInstallPlanWithPreExistingPackages
                             storePkgIndex
                             elaboratedPlan

        liftIO $ notice verbosity "==== The improved install plan ===="
        liftIO $ putStrLn $ InstallPlan.showInstallPlan improvedPlan
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

readSourcePackage :: Verbosity -> FilePath -> Rebuild (PackageSpecifier SourcePackage)
readSourcePackage verbosity cabalFile = do
    -- no need to monitorFiles because findProjectCabalFiles did it already
    pkgdesc <- liftIO $ Cabal.readPackageDescription verbosity cabalFile
    let srcLocation = LocalUnpackedPackage (takeDirectory cabalFile)
    return $ SpecificSourcePackage 
               SourcePackage {
                 packageInfoId        = packageId pkgdesc,
                 packageDescription   = pkgdesc,
                 packageSource        = srcLocation,
                 packageDescrOverride = Nothing
               }

configureCompiler :: Verbosity
                  -> (Maybe CompilerFlavor, Maybe FilePath, Maybe FilePath)
                  -> Rebuild (Compiler, Platform, ProgramConfiguration)
configureCompiler verbosity 
                  (compFlavor, hcPath, hcPkg) = do
    --TODO: need to track $PATH changes
    liftIO $ info verbosity "Compiler settings changed, reconfiguring..."

    result@(_, _, progdb) <- liftIO $
      Cabal.configCompilerEx
        compFlavor hcPath hcPkg
        defaultProgramDb verbosity

    monitorFiles (programsMonitorFiles progdb)

    return result

programsMonitorFiles :: ProgramDb -> [MonitorFilePath]
programsMonitorFiles progdb =   
    [ monitor
    | prog    <- configuredPrograms progdb
    , monitor <- monitorFileSearchPath (programMonitorFiles prog)
                                       (programPath prog)
    ]
  --TODO: also need to react to $PATH changes

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

getSourcePackages :: Verbosity -> [Repo] -> Rebuild SourcePackageDb
getSourcePackages verbosity repos = do
    monitorFiles . map MonitorFile
                 $ IndexUtils.getSourcePackagesMonitorFiles repos
    liftIO $ IndexUtils.getSourcePackages verbosity repos


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
getPackageSourceHashes :: Package srcpkg
                       => Verbosity
                       -> IO HttpTransport
                       -> (srcpkg -> PackageLocation (Maybe FilePath))
                       -> GenericInstallPlan ipkg srcpkg iresult ifailure
                       -> IO (Map PackageId PackageSourceHash)
getPackageSourceHashes verbosity mkTransport
                      packageSourceLocation installPlan = do

    -- Determine which packages need fetching, and which are present already
    --
    pkgslocs <- sequence
      [ do let locm = packageSourceLocation pkg
           mloc <- checkFetched locm
           return (pkg, locm, mloc)
      | InstallPlan.Configured pkg <- InstallPlan.toList installPlan ]
    let requireDownloading = [ (pkg, locm) | (pkg, locm, Nothing) <- pkgslocs ]
        alreadyDownloaded  = [ (pkg, loc)  | (pkg, _, Just loc)   <- pkgslocs ]

    -- Download the ones we need
    --
    newlyDownloaded <-
      if null requireDownloading
        then return []
        else do transport <- mkTransport
                sequence
                  [ do loc <- fetchPackage transport verbosity locm
                       return (pkg, loc)
                  | (pkg, locm) <- requireDownloading ]

    -- Get the hashes of all the tarball packages (i.e. not local dir pkgs)
    --
    liftM Map.fromList $
      sequence
        [ do srchash <- readFileHashValue tarball
             return (packageId pkg, srchash)

        | (pkg, srcloc) <- newlyDownloaded ++ alreadyDownloaded
        , tarball <- maybeToList (tarballFileLocation srcloc) ]
  where
    tarballFileLocation (LocalUnpackedPackage _dir)      = Nothing
    tarballFileLocation (LocalTarballPackage    tarball) = Just tarball
    tarballFileLocation (RemoteTarballPackage _ tarball) = Just tarball
    tarballFileLocation (RepoTarballPackage _ _ tarball) = Just tarball


-------------------------------
-- Project config
--

instance Binary ProjectConfig
instance Binary ProjectConfigSolver
instance Binary ProjectConfigBuildOnly
instance Binary PackageConfigShared
instance Binary PackageConfig

data ProjectConfig
   = ProjectConfig {
       projectConfigRootDir      :: FilePath,
       projectConfigPackageGlobs :: [FilePathGlob],

       projectConfigBuildOnly       :: ProjectConfigBuildOnly,
       projectConfigSolver          :: ProjectConfigSolver,
       projectConfigAllPackages     :: PackageConfigShared,
       projectConfigLocalPackages   :: PackageConfig,
       projectConfigSpecificPackage :: Map PackageName PackageConfig
     }
  deriving (Eq, Show, Generic)

data ProjectConfigBuildOnly
   = ProjectConfigBuildOnly {
       projectConfigVerbosity             :: Flag Verbosity,
       projectConfigDryRun                :: Flag Bool,
       projectConfigOnlyDeps              :: Flag Bool,
       projectConfigSummaryFile           :: NubList PathTemplate,
       projectConfigLogFile               :: Flag PathTemplate,
       projectConfigBuildReports          :: Flag ReportLevel,
       projectConfigReportPlanningFailure :: Flag Bool,
       projectConfigSymlinkBinDir         :: Flag FilePath,
       projectConfigOneShot               :: Flag Bool,
       projectConfigNumJobs               :: Flag (Maybe Int),
       projectConfigOfflineMode           :: Flag Bool,
       projectConfigKeepTempFiles         :: Flag Bool,
       projectConfigHttpTransport         :: Flag String,
       projectConfigCacheDir              :: Flag FilePath,
       projectConfigLogsDir               :: Flag FilePath,
       projectConfigWorldFile             :: Flag FilePath
     }
  deriving (Eq, Show, Generic)

data ProjectConfigSolver
   = ProjectConfigSolver {
       projectConfigSolverConstraints       :: [(UserConstraint, ConstraintSource)],
       projectConfigSolverPreferences       :: [Dependency],
       projectConfigConfigurationsFlags     :: FlagAssignment,
       projectConfigSolverCabalVersion      :: Flag Version,
       projectConfigSolverSolver            :: Flag PreSolver,
       projectConfigSolverAllowNewer        :: Flag AllowNewer,
       projectConfigRemoteRepos             :: NubList RemoteRepo,     -- ^ Available Hackage servers.
       projectConfigLocalRepos              :: NubList FilePath,

       projectConfigSolverMaxBackjumps      :: Flag Int,
       projectConfigSolverReorderGoals      :: Flag Bool,
       projectConfigSolverStrongFlags       :: Flag Bool,

       -- Things that only make sense for manual mode, not --local mode
       -- too much control!
       projectConfigSolverIndependentGoals  :: Flag Bool,
       projectConfigSolverShadowPkgs        :: Flag Bool,
       projectConfigSolverReinstall         :: Flag Bool,
       projectConfigSolverAvoidReinstalls   :: Flag Bool,
       projectConfigSolverOverrideReinstall :: Flag Bool,
       projectConfigSolverUpgradeDeps       :: Flag Bool
     }
  deriving (Eq, Show, Generic)


data PackageConfigShared
   = PackageConfigShared {
       packageConfigProgramPaths     :: [(String, FilePath)],
       packageConfigProgramArgs      :: [(String, [String])],
       packageConfigProgramPathExtra :: NubList FilePath,
       packageConfigHcFlavor         :: Flag CompilerFlavor,
       packageConfigHcPath           :: Flag FilePath,
       packageConfigHcPkg            :: Flag FilePath,
       packageConfigVanillaLib       :: Flag Bool,
       packageConfigProfLib          :: Flag Bool,
       packageConfigSharedLib        :: Flag Bool,
       packageConfigHaddockIndex     :: Flag PathTemplate,

       -- Things that only make sense for manual mode, not --local mode
       -- too much control!
       packageConfigUserInstall      :: Flag Bool,
       packageConfigInstallDirs      :: InstallDirs (Flag PathTemplate),
       packageConfigPackageDBs       :: [Maybe PackageDB],
       packageConfigRelocatable      :: Flag Bool,
       packageConfigRootCmd          :: Flag String
     }
  deriving (Eq, Show, Generic)


data PackageConfig
   = PackageConfig {
       packageConfigDynExe              :: Flag Bool,
       packageConfigProfExe             :: Flag Bool,
       packageConfigProf                :: Flag Bool,
       packageConfigProfDetail          :: Flag ProfDetailLevel,
       packageConfigProfLibDetail       :: Flag ProfDetailLevel,
       packageConfigConfigureArgs       :: [String],
       packageConfigOptimization        :: Flag OptimisationLevel,
       packageConfigProgPrefix          :: Flag PathTemplate,
       packageConfigProgSuffix          :: Flag PathTemplate,
       packageConfigExtraLibDirs        :: [FilePath],
       packageConfigExtraIncludeDirs    :: [FilePath],
       packageConfigGHCiLib             :: Flag Bool,
       packageConfigSplitObjs           :: Flag Bool,
       packageConfigStripExes           :: Flag Bool,
       packageConfigStripLibs           :: Flag Bool,
       packageConfigTests               :: Flag Bool,
       packageConfigBenchmarks          :: Flag Bool,
       packageConfigCoverage            :: Flag Bool,
       packageConfigLibCoverage         :: Flag Bool,
       packageConfigDebugInfo           :: Flag DebugInfoLevel,
       packageConfigRunTests            :: Flag Bool,
       packageConfigDocumentation       :: Flag Bool,
       packageConfigHaddockHoogle       :: Flag Bool,
       packageConfigHaddockHtml         :: Flag Bool,
       packageConfigHaddockHtmlLocation :: Flag String,
       packageConfigHaddockExecutables  :: Flag Bool,
       packageConfigHaddockTestSuites   :: Flag Bool,
       packageConfigHaddockBenchmarks   :: Flag Bool,
       packageConfigHaddockInternal     :: Flag Bool,
       packageConfigHaddockCss          :: Flag FilePath,
       packageConfigHaddockHscolour     :: Flag Bool,
       packageConfigHaddockHscolourCss  :: Flag FilePath,
       packageConfigHaddockContents     :: Flag PathTemplate
     }
  deriving (Eq, Show, Generic)


-- | Reads an explicit @cabal.project@ file in the given project root dir,
-- or returns the default project config for an implicitly defined project.
--
readProjectConfig :: FilePath
                  -> ProjectConfigSolver
                  -> PackageConfigShared
                  -> PackageConfig
                  -> ProjectConfigBuildOnly
                  -> Rebuild ProjectConfig
readProjectConfig projectRootDir
                  cliConfigSolver cliConfigAllPackages
                  cliConfigLocalPackages cliConfigBuildOnly = do
    let projectFile = projectRootDir</> "cabal.project"
    usesExplicitProjectRoot <- liftIO $ doesFileExist projectFile
    if usesExplicitProjectRoot
      then do monitorFiles [MonitorFileHashed projectFile]
              parseProjectConfig <$> liftIO (readFile projectFile)

      else do monitorFiles [MonitorNonExistantFile projectFile]
              return defaultImplicitProjectConfig
  where
    --TODO: this is just a placeholder, need to actually read these settings
    -- from the project file and combine them with the user-wide config and cli
    parseProjectConfig content =
      ProjectConfig
        projectRootDir
        (catMaybes (map simpleParse (lines content)))
        cliConfigBuildOnly
        cliConfigSolver
        cliConfigAllPackages
        cliConfigLocalPackages                     
        Map.empty

    defaultImplicitProjectConfig :: ProjectConfig
    defaultImplicitProjectConfig =
      ProjectConfig
        projectRootDir
        [ GlobFile (Glob [WildCard, Literal ".cabal"])
        , GlobDir  (Glob [WildCard]) $
          GlobFile (Glob [WildCard, Literal ".cabal"])
        ]
        cliConfigBuildOnly
        cliConfigSolver
        cliConfigAllPackages
        cliConfigLocalPackages                     
        Map.empty

convertCommandLineFlags :: GlobalFlags
                        -> ConfigFlags  -> ConfigExFlags
                        -> InstallFlags -> Cabal.HaddockFlags
                        -> ( ProjectConfigSolver
                           , PackageConfigShared
                           , PackageConfig
                           , ProjectConfigBuildOnly
                           )
convertCommandLineFlags globalFlags configFlags configExFlags
                        installFlags haddockFlags =
    ( ProjectConfigSolver{..}
    , PackageConfigShared{..}
    , PackageConfig{..}
    , ProjectConfigBuildOnly{..}
    )
  where
    GlobalFlags {
      globalVersion           = _,
      globalNumericVersion    = _,
      globalConfigFile        = _, -- TODO
      globalSandboxConfigFile = _, -- ??
      globalRemoteRepos       = projectConfigRemoteRepos,
      globalLocalRepos        = projectConfigLocalRepos,
      globalCacheDir          = projectConfigCacheDir,
      globalLogsDir           = projectConfigLogsDir,
      globalWorldFile         = projectConfigWorldFile,
      globalRequireSandbox    = _,
      globalIgnoreSandbox     = _,
      globalHttpTransport     = projectConfigHttpTransport
    } = globalFlags

    ConfigFlags {
      configPrograms            = _,
      configProgramPaths        = packageConfigProgramPaths,
      configProgramArgs         = packageConfigProgramArgs,
      configProgramPathExtra    = packageConfigProgramPathExtra,
      configHcFlavor            = packageConfigHcFlavor,
      configHcPath              = packageConfigHcPath,
      configHcPkg               = packageConfigHcPkg,
      configVanillaLib          = packageConfigVanillaLib,
      configProfLib             = packageConfigProfLib,
      configSharedLib           = packageConfigSharedLib,
      configDynExe              = packageConfigDynExe,
      configProfExe             = packageConfigProfExe,
      configProf                = packageConfigProf,
      configProfDetail          = packageConfigProfDetail,
      configProfLibDetail       = packageConfigProfLibDetail,
      configConfigureArgs       = packageConfigConfigureArgs,
      configOptimization        = packageConfigOptimization,
      configProgPrefix          = packageConfigProgPrefix,
      configProgSuffix          = packageConfigProgSuffix,
      configInstallDirs         = packageConfigInstallDirs,
      configScratchDir          = _,
      configDistPref            = _,
      configVerbosity           = projectConfigVerbosity,
      configUserInstall         = packageConfigUserInstall,
      configPackageDBs          = packageConfigPackageDBs,
      configGHCiLib             = packageConfigGHCiLib,
      configSplitObjs           = packageConfigSplitObjs,
      configStripExes           = packageConfigStripExes,
      configStripLibs           = packageConfigStripLibs,
      configExtraLibDirs        = packageConfigExtraLibDirs,
      configExtraIncludeDirs    = packageConfigExtraIncludeDirs,
      configConstraints         = _, -- instead use configExConstraints
      configDependencies        = _, -- Setup.hs specific
      configInstantiateWith     = _, -- Setup.hs specific
      configConfigurationsFlags = projectConfigConfigurationsFlags,
      configTests               = packageConfigTests,
      configBenchmarks          = packageConfigBenchmarks,
      configCoverage            = packageConfigCoverage,
      configLibCoverage         = packageConfigLibCoverage,
      configExactConfiguration  = _,
      configFlagError           = _,
      configRelocatable         = packageConfigRelocatable,
      configDebugInfo           = packageConfigDebugInfo
    } = configFlags

    ConfigExFlags {
      configCabalVersion        = projectConfigSolverCabalVersion,
      configExConstraints       = projectConfigSolverConstraints,
      configPreferences         = projectConfigSolverPreferences,
      configSolver              = projectConfigSolverSolver,
      configAllowNewer          = projectConfigSolverAllowNewer
    } = configExFlags

    InstallFlags {
      installDocumentation      = packageConfigDocumentation,
      installHaddockIndex       = packageConfigHaddockIndex,
      installDryRun             = projectConfigDryRun,
      installReinstall          = projectConfigSolverReinstall,
      installAvoidReinstalls    = projectConfigSolverAvoidReinstalls,
      installOverrideReinstall  = projectConfigSolverOverrideReinstall,
      installMaxBackjumps       = projectConfigSolverMaxBackjumps,
      installUpgradeDeps        = projectConfigSolverUpgradeDeps,
      installReorderGoals       = projectConfigSolverReorderGoals,
      installIndependentGoals   = projectConfigSolverIndependentGoals,
      installShadowPkgs         = projectConfigSolverShadowPkgs,
      installStrongFlags        = projectConfigSolverStrongFlags,
      installOnly               = _,
      installOnlyDeps           = projectConfigOnlyDeps,
      installRootCmd            = packageConfigRootCmd,
      installSummaryFile        = projectConfigSummaryFile,
      installLogFile            = projectConfigLogFile,
      installBuildReports       = projectConfigBuildReports,
      installReportPlanningFailure = projectConfigReportPlanningFailure,
      installSymlinkBinDir      = projectConfigSymlinkBinDir,
      installOneShot            = projectConfigOneShot,
      installNumJobs            = projectConfigNumJobs,
      installRunTests           = packageConfigRunTests,
      installOfflineMode        = projectConfigOfflineMode
    } = installFlags

    HaddockFlags {
      haddockProgramPaths       = _,
      haddockProgramArgs        = _,
      haddockHoogle             = packageConfigHaddockHoogle,
      haddockHtml               = packageConfigHaddockHtml,
      haddockHtmlLocation       = packageConfigHaddockHtmlLocation,
      haddockExecutables        = packageConfigHaddockExecutables,
      haddockTestSuites         = packageConfigHaddockTestSuites,
      haddockBenchmarks         = packageConfigHaddockBenchmarks,
      haddockInternal           = packageConfigHaddockInternal,
      haddockCss                = packageConfigHaddockCss,
      haddockHscolour           = packageConfigHaddockHscolour,
      haddockHscolourCss        = packageConfigHaddockHscolourCss,
      haddockContents           = packageConfigHaddockContents,
      haddockDistPref           = _,
      haddockKeepTempFiles      = projectConfigKeepTempFiles,
      haddockVerbosity          = _
    } = haddockFlags

projectConfigRepos :: FilePath -> ProjectConfigSolver -> [Repo]
projectConfigRepos downloadCacheRootDir
                   ProjectConfigSolver { projectConfigRemoteRepos
                                       , projectConfigLocalRepos } =
    remoteRepos ++ localRepos
  where
    remoteRepos =
      [ Repo (Left remote) cacheDir
      | remote <- fromNubList projectConfigRemoteRepos
      , let cacheDir = downloadCacheRootDir
                   </> remoteRepoName remote ]
    localRepos =
      [ Repo (Right LocalRepo) local
      | local <- fromNubList projectConfigLocalRepos ]


-------------------------------
-- Simple rebuild abstraction
--

newtype Rebuild a = Rebuild (StateT [MonitorFilePath] IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

monitorFiles :: [MonitorFilePath] -> Rebuild ()
monitorFiles filespecs = Rebuild (State.modify (filespecs++))

unRebuild :: Rebuild a -> IO (a, [MonitorFilePath])
unRebuild (Rebuild action) = runStateT action []

runRebuild :: Rebuild a -> IO a
runRebuild (Rebuild action) = evalStateT action []

--TODO: if we make this a Rebuild then we also have to cache and return the
--      monitor files
rerunIfChanged :: (Eq a, Binary a, Binary b)
               => FileMonitorName a b
               -> FilePath
               -> a
               -> Rebuild b
               -> Rebuild b
rerunIfChanged cacheFile monitorFilesRootDir key action = do
    changed <- liftIO $ checkFileMonitorChanged cacheFile monitorFilesRootDir key
    case changed of
      Changed -> do
        (result, files) <- liftIO $ unRebuild action
--        liftIO $ putStrLn $ takeFileName cacheFile ++ " changed"
        liftIO $ updateFileMonitor cacheFile monitorFilesRootDir
                                   files key result
        monitorFiles files
        return result

      Unchanged (result, files) -> do
        liftIO $ putStrLn $ show cacheFile ++ " unchanged"
        monitorFiles files
        return result


-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

planPackages :: Compiler
             -> Platform
             -> Solver -> ProjectConfigSolver
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> [PackageSpecifier SourcePackage]
             -> Progress String String InstallPlan
planPackages comp platform solver solverconfig
             installedPkgIndex sourcePkgDb pkgSpecifiers =

    resolveDependencies
      platform (compilerInfo comp)
      solver
      resolverParams

  where

    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . setAvoidReinstalls avoidReinstalls --TODO: should only be configurable for custom installs

      . setShadowPkgs shadowPkgs --TODO: should only be configurable for custom installs

      . setStrongFlags strongFlags

      . setPreferenceDefault (if upgradeDeps then PreferAllLatest
                                             else PreferLatestForSelected)
                             --TODO: decide if we need to prefer installed for global packages?

      . removeUpperBounds allowNewer

      . addPreferences
          -- preferences from the config file or command line
          [ PackageVersionPreference name ver
          | Dependency name ver <- projectConfigSolverPreferences ]

      . addConstraints
          -- version constraints from the config file or command line
            [ LabeledPackageConstraint (userToPackageConstraint pc) src
            | (pc, src) <- projectConfigSolverConstraints ]

      . addConstraints
          [ let pc = PackageConstraintStanzas
                     (pkgSpecifierTarget pkgSpecifier) stanzas
            in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
          | pkgSpecifier <- pkgSpecifiers ]

      . addConstraints
          --FIXME: this just applies all flags to all targets which
          -- is silly. We should check if the flags are appropriate
          [ let pc = PackageConstraintFlags
                     (pkgSpecifierTarget pkgSpecifier) flags
            in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
          | let flags = projectConfigConfigurationsFlags
          , not (null flags)
          , pkgSpecifier <- pkgSpecifiers ]

      . reinstallTargets  --TODO: do we want this? we already hide all installed packages in the store from the solver

      $ standardInstallPolicy
        installedPkgIndex sourcePkgDb pkgSpecifiers

    stanzas = [] --TODO: should enable for local only [TestStanzas, BenchStanzas]
    --TODO: while for the local mode we want to run the solver with the tests
    -- and benchmarks turned on by default (so the solution is stable when we
    -- actually enable/disable tests), but really we want to have a solver
    -- mode where it tries to enable these but if it can't work then to turn
    -- them off.
{-
      concat
        [ if testsEnabled then [TestStanzas] else []
        , if benchmarksEnabled then [BenchStanzas] else []
        ]
    testsEnabled = fromFlagOrDefault False $ configTests configFlags
    benchmarksEnabled = fromFlagOrDefault False $ configBenchmarks configFlags
-}
--    reinstall        = fromFlag projectConfigSolverReinstall
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

--      projectConfigSolverReinstall,  --TODO: check not configurable for local mode?
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
  -> PackageDBStack -> PackageDBStack
  -> DistDirLayout
  -> GenericInstallPlan InstalledPackageInfo
                        ConfiguredPackage
                        _iresult _ifailure
  -> [PackageSpecifier SourcePackage]
  -> Map PackageId HashValue
  -> InstallDirs.InstallDirTemplates
  -> PackageConfigShared
  -> PackageConfig
  -> Map PackageName PackageConfig
  -> (ElaboratedInstallPlan, ElaboratedSharedConfig)
elaborateInstallPlan platform compiler progdb
                     ambientPackageDbs storePackageDbs
                     -- TODO: ^^^ these args could come as flag
                     -- sets or other config file input. For now
                     -- just individual args
                     DistDirLayout{..}
                     solverPlan pkgSpecifiers
                     sourcePackageHashes
                     defaultInstallDirs
                     sharedPackageConfig
                     localPackagesConfig
                     perPackageConfig =
    (installPlan, elaboratedSharedConfig)
  where
    elaboratedSharedConfig =
      ElaboratedSharedConfig {
        pkgConfigPlatform         = platform,
        pkgConfigCompiler         = compiler,
        pkgConfigProgramDb        = progdb,
 
        --TODO: for now just an example and not yet configurable
        pkgConfigLibraryProfiling = False
      }

    installPlan = InstallPlan.mapPreservingGraph convertPlanPackage solverPlan

    convertPlanPackage :: InstallPlan.GenericPlanPackage InstalledPackageInfo
                                                         ConfiguredPackage
                                                         _iresult _ifailure
                       -> InstallPlan.GenericPlanPackage InstalledPackageInfo
                                                         ElaboratedConfiguredPackage
                                                         iresult ifailure
    convertPlanPackage (InstallPlan.PreExisting pkg) =
      InstallPlan.PreExisting pkg

    convertPlanPackage (InstallPlan.Configured  pkg) =
      InstallPlan.Configured (individualPackageConfig pkg)

    convertPlanPackage _ =
      error "elaborateInstallPlan: unexpected package state"
    
    individualPackageConfig :: ConfiguredPackage -> ElaboratedConfiguredPackage
    individualPackageConfig
      pkg@(ConfiguredPackage (SourcePackage pkgid desc srcloc descOverride)
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
          = InstalledPackageId (display pkgid ++ "-inplace")
          
          | Just sourceHash <- Map.lookup pkgid sourcePackageHashes
          = hashedInstalledPackageId PackageHashInputs {
              pkgHashPkgId       = pkgid,
              pkgHashSourceHash  = sourceHash,
              pkgHashDirectDeps  = ComponentDeps.libraryDeps (depends pkg), --TODO: consider carefully which deps
              pkgHashOtherConfig = elaboratedPackageHashConfigInputs
                                       elaboratedSharedConfig
                                       elaboratedPackage -- recursive use
            }
          
          | otherwise
          = error $ "elaborateInstallPlan: non-inplace package "
                 ++ " is missing a source hash: " ++ display pkgid

        -- All the other fields of the ElaboratedConfiguredPackage
        --
        pkgSourceId         = pkgid
        pkgDescription      = desc
        pkgFlagAssignment   = flags
        pkgEnabledStanzas   = stanzas
        pkgTestsuitesEnable = TestStanzas  `elem` stanzas --TODO: only actually enable if solver allows it and we want it
        pkgBenchmarksEnable = BenchStanzas `elem` stanzas --TODO: only actually enable if solver allows it and we want it
        pkgDependencies     = fmap (map confInstId) deps
        pkgSourceLocation   = srcloc

        pkgBuildStyle       = if shouldBuildInplaceOnly pkg
                                then BuildInplaceOnly else BuildAndInstall
        pkgSetupPackageDBStack    = ambientPackageDbs
                                    --TODO: ^^ this should be conditional on
                                    -- whether the package is using an explicit
                                    -- custom setup stanza for specific deps.
                                    -- If so, then it must use the storeDb.
        pkgBuildPackageDBStack    = buildAndRegisterDbs
        pkgRegisterPackageDBStack = buildAndRegisterDbs
        pkgRequiresRegistration   = isJust (Cabal.condLibrary desc)
        pkgHasExplicitSetupDeps   = isJust (Cabal.setupBuildInfo (Cabal.packageDescription desc))
        pkgDescriptionOverride    = descOverride

        pkgVanillaLib    = sharedOptionFlag True  packageConfigVanillaLib
        pkgSharedLib     = sharedOptionFlag False packageConfigSharedLib
        pkgDynExe        = perPkgOptionFlag False packageConfigDynExe
        pkgGHCiLib       = perPkgOptionFlag False packageConfigGHCiLib --TODO: needs to default to enabled on windows still

        pkgProfLib       = sharedOptionFlag False packageConfigProfLib
        pkgProfExe       = perPkgOptionFlag False packageConfigProfExe

        (pkgProfExeDetail,
         pkgProfLibDetail) = perPkgOptionLibExeFlag ProfDetailDefault
                               packageConfigProfDetail
                               packageConfigProfLibDetail
        (pkgLibCoverage,
         pkgExeCoverage)   = perPkgOptionLibExeFlag False
                               packageConfigCoverage
                               packageConfigLibCoverage

        pkgOptimization  = perPkgOptionFlag NormalOptimisation packageConfigOptimization
        pkgSplitObjs     = perPkgOptionFlag False packageConfigSplitObjs
        pkgStripLibs     = perPkgOptionFlag False packageConfigStripLibs
        pkgStripExes     = perPkgOptionFlag False packageConfigStripExes
        pkgDebugInfo     = perPkgOptionFlag NoDebugInfo packageConfigDebugInfo

        pkgConfigureScriptArgs = perPkgOptionList packageConfigConfigureArgs
        pkgExtraLibDirs        = perPkgOptionList packageConfigExtraLibDirs
        pkgExtraIncludeDirs    = perPkgOptionList packageConfigExtraIncludeDirs
        pkgProgPrefix          = perPkgOptionMaybe packageConfigProgPrefix
        pkgProgSuffix          = perPkgOptionMaybe packageConfigProgSuffix

        -- Note: the prefix here is not yet valid, has to be overridden per-pkg.
        -- This is because we've not yet calculated the installed package id
        pkgInstallDirs =
            InstallDirs.substituteInstallDirTemplates templateEnv dirTemplates
          where
            templateEnv  = InstallDirs.initialPathTemplateEnv
                             pkgid
                             (LibraryName (display pkgid))
                             (compilerInfo compiler)
                             platform
            dirTemplates
              | shouldBuildInplaceOnly pkg
              -- use the ordinary default install dirs
              = defaultInstallDirs

              | otherwise 
                -- keep it prefix-relative so we can still substitute that later
              = defaultInstallDirs {
                  InstallDirs.prefix = InstallDirs.toPathTemplate "$prefix"
                }


        pkgname = packageName pkgid

        sharedOptionFlag  :: a -> (PackageConfigShared -> Flag a) -> a
        perPkgOptionFlag  :: a -> (PackageConfig       -> Flag a) -> a
        perPkgOptionMaybe ::      (PackageConfig       -> Flag a) -> Maybe a
        perPkgOptionList  ::      (PackageConfig       -> [a])    -> [a]

        sharedOptionFlag def f = fromFlagOrDefault def shared
          where
            shared = f sharedPackageConfig

        perPkgOptionFlag def f = fromFlagOrDefault def (lookupPerPkgOption f)
        perPkgOptionMaybe    f = flagToMaybe (lookupPerPkgOption f)
        perPkgOptionList     f = lookupPerPkgOption f

        perPkgOptionLibExeFlag def fboth flib = (exe, lib)
          where
            exe = fromFlagOrDefault def bothflag
            lib = fromFlagOrDefault def (bothflag <> libflag)

            bothflag = lookupPerPkgOption fboth
            libflag  = lookupPerPkgOption flib

        lookupPerPkgOption f = common <> perpkg
          where
            common = f localPackagesConfig
            perpkg = maybe mempty f (Map.lookup pkgname perPackageConfig)

        buildAndRegisterDbs
          | shouldBuildInplaceOnly pkg = inplacePackageDbs
          | otherwise                  = storePackageDbs

    inplacePackageDbs =
        storePackageDbs
     ++ [SpecificPackageDB (distPackageDB (compilerId compiler))]

    -- For this local build policy, every package that lives in a local source
    -- dir (as opposed to a tarball), or depends on such a package, will be
    -- built inplace into a shared dist dir. Tarball packages that depend on
    -- source dir packages will also get unpacked locally.
    shouldBuildInplaceOnly :: HasInstalledPackageId pkg => pkg -> Bool
    shouldBuildInplaceOnly pkg = Set.member (installedPackageId pkg)
                                            pkgsToBuildInplaceOnly

    pkgsToBuildInplaceOnly :: Set InstalledPackageId
    pkgsToBuildInplaceOnly =
        Set.fromList
      $ map installedPackageId
      $ InstallPlan.reverseDependencyClosure
          solverPlan
          [ fakeInstalledPackageId (packageId pkg)
          | SpecificSourcePackage pkg <- pkgSpecifiers ]

--TODO: perhaps reorder this code
-- based on the ElaboratedInstallPlan + ElaboratedSharedConfig,
-- make the various Setup.hs {configure,build,copy} flags

setupHsScriptOptions :: ElaboratedReadyPackage
                     -> ElaboratedSharedConfig
                     -> FilePath
                     -> FilePath
                     -> Bool
                     -> Lock
                     -> SetupScriptOptions
setupHsScriptOptions (ReadyPackage ElaboratedConfiguredPackage{..} _)
                     ElaboratedSharedConfig{..} srcdir builddir
                     isParallelBuild cacheLock =
    SetupScriptOptions {
      useCabalVersion          = anyVersion, --TODO
      useCompiler              = Just pkgConfigCompiler,
      usePlatform              = Just pkgConfigPlatform,
      usePackageDB             = pkgSetupPackageDBStack,
      usePackageIndex          = Nothing, --TODO
      useDependencies          = [],    --TODO
      useDependenciesExclusive = False, --TODO
      useProgramConfig         = pkgConfigProgramDb,
      useDistPref              = builddir,
      useLoggingHandle         = Nothing, --TODO
      useWorkingDir            = Just srcdir,
      useWin32CleanHack        = False,   --TODO
      forceExternalSetupMethod = isParallelBuild,
      setupCacheLock           = Just cacheLock
    }

setupHsConfigureFlags :: ElaboratedReadyPackage
                      -> ElaboratedSharedConfig
                      -> CabalDirLayout
                      -> Maybe InstalledPackageId
                      -> Verbosity
                      -> FilePath
                      -> Cabal.ConfigFlags
setupHsConfigureFlags (ReadyPackage
                         ElaboratedConfiguredPackage{..}
                         pkgdeps)
                      ElaboratedSharedConfig{..}
                      CabalDirLayout{cabalStorePackageDirectory}
                      mipkgid
                      verbosity builddir =
    Cabal.ConfigFlags {..}
  where
    configDistPref            = toFlag builddir
    configVerbosity           = toFlag verbosity

    configProgramPaths        = mempty --TODO: get from pkgConfigProgramDb
    configProgramArgs         = mempty --TODO: get from pkgConfigProgramDb
    configProgramPathExtra    = mempty --TODO: get from pkgConfigProgramDb
    configHcFlavor            = toFlag (compilerFlavor pkgConfigCompiler)
    configHcPath              = mempty -- use configProgramPaths instead
    configHcPkg               = mempty -- use configProgramPaths instead

    configVanillaLib          = toFlag pkgVanillaLib
    configSharedLib           = toFlag pkgSharedLib
    configDynExe              = toFlag pkgDynExe
    configGHCiLib             = toFlag pkgGHCiLib
    configProfExe             = toFlag pkgProfExe
    configProfLib             = toFlag pkgProfLib
    configProf                = mempty --TODO: check older Cabal lib

    -- configProfDetail is for exe+lib, but overridden by configProfLibDetail
    -- so we specify both so we can specify independently
    configProfDetail          = toFlag pkgProfExeDetail
    configProfLibDetail       = toFlag pkgProfLibDetail

    configCoverage            = toFlag pkgExeCoverage
    configLibCoverage         = toFlag pkgLibCoverage

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

    configInstallDirs = case mipkgid of
      Nothing     -> fmap toFlag pkgInstallDirs
      Just ipkgid -> fmap toFlag (subst pkgInstallDirs)
        where
          subst  = InstallDirs.substituteInstallDirTemplates
                     [(InstallDirs.PrefixVar, InstallDirs.toPathTemplate prefix)]
          prefix = cabalStorePackageDirectory compid ipkgid
          compid = compilerId pkgConfigCompiler

    -- we only use configDependencies, unless we're talking to an old Cabal
    -- in which case we use configConstraints
    configDependencies        = [ (packageName (Installed.sourcePackageId deppkg),
                                  Installed.installedPackageId deppkg)
                                | deppkg <- ComponentDeps.nonSetupDeps pkgdeps ]
    configConstraints         = [ thisPackageVersion (packageId deppkg)
                                | deppkg <- ComponentDeps.nonSetupDeps pkgdeps ]

    -- explicitly clear, then our package db stack
    -- TODO: have to do this differently for older Cabal versions
    configPackageDBs          = Nothing : map Just pkgBuildPackageDBStack

    configInstantiateWith     = mempty --TODO: unused within cabal-install
    configTests               = toFlag pkgTestsuitesEnable
    configBenchmarks          = toFlag pkgBenchmarksEnable

    configExactConfiguration  = toFlag True --TODO: always use this, if available
    configFlagError           = mempty --TODO: appears not to be implemented
    configRelocatable         = mempty --TODO: ???
    configScratchDir          = mempty -- never use
    configUserInstall         = mempty -- don't rely on defaults
    configPrograms            = error "setupHsConfigureFlags: configPrograms"

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
      buildNumJobs      = mempty, --TODO: sometimes want to use toFlag (Just numBuildJobs),
      buildArgs         = mempty  --TODO: allow building individual components as targets
    }

setupHsCopyFlags :: ElaboratedConfiguredPackage
                 -> ElaboratedSharedConfig
                 -> Verbosity
                 -> FilePath
                 -> Cabal.CopyFlags
setupHsCopyFlags _ _ verbosity builddir =
    Cabal.CopyFlags {
      --TODO: we currently just rely on Setup.hs copy to always do the right
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

{- TODO:
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

-- TODO: for safety of concurrent installs, we must make sure we register but
-- not replace installed packages with ghc-pkg.

elaboratedPackageHashConfigInputs :: ElaboratedSharedConfig
                                  -> ElaboratedConfiguredPackage
                                  -> PackageHashConfigInputs
elaboratedPackageHashConfigInputs
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
      pkgHashLibCoverage         = pkgLibCoverage,
      pkgHashExeCoverage         = pkgExeCoverage,
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


-- | Given the 'InstalledPackageIndex' for a nix-style package store, and
-- enough information to calculate 'InstalledPackageId' for a selection of
-- source packages 
-- 
improveInstallPlanWithPreExistingPackages
  :: forall srcpkg iresult ifailure.
     (HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
  => InstalledPackageIndex
  -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
  -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
improveInstallPlanWithPreExistingPackages installedPkgIndex =

    go []
  where
    -- So here's the strategy:
    --
    --  * Go through each ready package in dependency order. Indeed we
    --    simulate executing the plan, but instead of going from ready to
    --    processing to installed, we go from ready to either pre-existing
    --    or processing.
    --
    --  * Calculate the 'InstalledPackageId' if we can (ie if we've been able
    --    to get the tarball hash)
    --
    --  * Check if that package is already installed and if so, we replace the
    --    ready pacage by the pre-existing package.
    --
    --  * If we cannot calculate the 'InstalledPackageId' or it's not already
    --    installed (ie we would have to build it) then we put it into the
    --    'Processing' state so that it doesn't keep appearing in the ready
    --    list.
    --
    --  * When there are no more packages in the ready state then we're done,
    --    except that we need to reset the packages we put into the processing
    --    state.
    --
    -- When we have ready packages that we cannot replace with pre-existing
    -- packages then none of their dependencies can be replaced either. This
    -- constraint is respected here because we put those packages into the
    -- processing state, and so none of their deps will be able to appear in
    -- the ready list.
    --
    -- We accumulate the packages in the processing state. These are the ones
    -- that will have to be built because they cannot be replaced with
    -- pre-existing installed packages.

    go :: [GenericReadyPackage srcpkg InstalledPackageInfo]
       -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
       -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
    go cannotBeImproved installPlan =
      case InstallPlan.ready installPlan of
        -- no more source packages can be replaced with pre-existing ones,
        --  just need to reset the ones we put into the processing state
        [] -> trace "go done" $
              InstallPlan.reverted
                [ pkg | ReadyPackage pkg _ <- cannotBeImproved ]
                installPlan

        -- we have some to look at
        pkgs -> go (cannotBeImproved' ++ cannotBeImproved)
                   installPlan'
          where
            installPlan' = InstallPlan.processing cannotBeImproved'
                         . replaceWithPreExisting canBeImproved
                         $ installPlan

            (cannotBeImproved', canBeImproved) =
              partitionEithers
                [ case canPackageBeImproved pkg of
                    Nothing   -> Left pkg
                    Just ipkg -> Right (pkg, ipkg)
                | pkg <- pkgs ]

    canPackageBeImproved :: GenericReadyPackage srcpkg InstalledPackageInfo
                         -> Maybe InstalledPackageInfo
    canPackageBeImproved pkg = PackageIndex.lookupInstalledPackageId
                                 installedPkgIndex (installedPackageId pkg)

    replaceWithPreExisting :: [(GenericReadyPackage srcpkg InstalledPackageInfo, InstalledPackageInfo)]
                           -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
                           -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
    replaceWithPreExisting canBeImproved plan0 =
      foldl' (\plan (pkg, ipkg) -> InstallPlan.preexisting (installedPackageId pkg) ipkg plan)
             plan0
             canBeImproved


------------------------------------------------------------------------------
-- * Doing it: executing an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------


rebuildTargets :: Verbosity
               -> DistDirLayout
               -> CabalDirLayout
               -> ElaboratedInstallPlan
               -> ElaboratedSharedConfig
               -> ProjectConfigBuildOnly
               -> [String] --TODO: only build the ones we ask for out of the persistent plan
                           --TODO: probably want to parse these earlier
               -> IO ()
rebuildTargets verbosity
               distDirLayout@DistDirLayout{..} cabalDirLayout
               installPlan
               sharedPackageConfig
               ProjectConfigBuildOnly{
                 projectConfigNumJobs,
                 projectConfigHttpTransport
               }
               _userTargets = do

    putStrLn $ printPlan installPlan

    -- Concurrency control: create the job controler and concurrency limits 
    -- for downloading, building and installing.
    jobControl    <- if isParallelBuild then newParallelJobControl
                                        else newSerialJobControl
    buildLimit    <- newJobLimit numBuildJobs
    installLock   <- newLock -- serialise installation
    cacheLock     <- newLock -- serialise access to setup exe cache
                             --TODO: if we treat setup exe like other build deps
                             -- then we don't need a separate cache lock

    createDirectoryIfMissingVerbose verbosity False distBuildRootDirectory
    createDirectoryIfMissingVerbose verbosity False distTempDirectory

    -- Before traversing the install plan, pre-emptively find all packages that
    -- will need to be downloaded and start downloading them.
    pkgsToDownload <- packagesRequiringDownload
                        pkgSourceLocation
                        installPlan

  --  mapM_ print pkgsToDownload

    residualPlan <- 
      asyncDownloadPackages verbosity mkTransport
                            pkgsToDownload $ \downloadMap ->

      -- For each package in the plan, in dependency order, but in parallel...
      executeInstallPlan verbosity jobControl installPlan
                         $ \rpkg@(ReadyPackage (cpkg :: ElaboratedConfiguredPackage) _) -> do

        -- If necessary, wait for the download of the remote package to finish.
        srcloc <- waitAsyncPackageDownload verbosity downloadMap cpkg
        -- We're not typing up buildLimit-limited resources while downloading,
        -- but we are once we start unpacking and building.
        withJobLimit buildLimit $
          withPackageInLocalDirectory verbosity distDirLayout
                                      srcloc (pkgBuildStyle cpkg)
                                      (packageId rpkg)
                                      $ \srcdir builddir ->
            case pkgBuildStyle cpkg of
              BuildAndInstall ->
                buildAndInstallUnpackedPackage
                  verbosity distDirLayout cabalDirLayout
                  isParallelBuild installLock cacheLock
                  sharedPackageConfig
                  rpkg srcdir builddir'
                where
                  builddir' = makeRelative srcdir builddir
                  --TODO ^^ do this relative stuff better

              BuildInplaceOnly ->
                --TODO: use a relative build dir rather than absolute
                printTiming "=========== buildInplaceUnpackedPackage ============" $
                  buildInplaceUnpackedPackage
                    verbosity distDirLayout cabalDirLayout
                    isParallelBuild cacheLock
                    sharedPackageConfig
                    rpkg srcdir builddir

    _  <- return residualPlan
    --TODO: this result plan should be saved, resetting all failed and
    -- BuildInplaceOnly packages to original Configured state
    -- so effectively only the installed tarball packages get updated
    -- could also replace installed ones to be pre-installed
    return ()

  where
    numBuildJobs    = 1 --TODO: reenable: determineNumJobs projectConfigNumJobs
    isParallelBuild = numBuildJobs >= 2
    mkTransport     = configureTransport verbosity 
                        (flagToMaybe projectConfigHttpTransport)

printTiming :: Show t => t -> IO b -> IO b
printTiming name action = do
  before <- getCurrentTime
  x <- action
  after <- getCurrentTime
  print ("printTiming", name, after `diffUTCTime` before)
  return x

--TODO: do we need to use a with-style for the temp files for downloading http
-- packages, or are we going to cache them persistently?

-- | Given the current 'InstallPlan', work out which packages will require
-- a download step of some form, and return the locations of those packages,
-- in the dependency order (so that the one's we'll need first are the ones
-- we will start downloading first).
--
packagesRequiringDownload :: (srcpkg -> PackageLocation (Maybe FilePath))
                          -> GenericInstallPlan ipkg srcpkg iresult ifailure
                          -> IO [PackageLocation (Maybe FilePath)]
packagesRequiringDownload packageSourceLocation installPlan =
    filterM (fmap not . isFetched)
      [ packageSourceLocation pkg
      | InstallPlan.Configured pkg
         <- InstallPlan.reverseTopologicalOrder installPlan
      ]
      --TODO: check that the reverseTopologicalOrder is more or less the same
      -- order as we get out of the install plan when we run it

type AsyncDownloadMap = Map (PackageLocation (Maybe FilePath))
                            (MVar (PackageLocation FilePath))

-- | Fork off an async action to download all the given packages. The body
-- action is passed a map from those packages (identified by their location)
-- to a completion var for that package. So the body action should lookup the
-- location and use 'waitAsyncPackageDownload' to get the result.
--
asyncDownloadPackages :: Verbosity
                      -> IO HttpTransport
                      -> [PackageLocation (Maybe FilePath)]
                      -> (AsyncDownloadMap -> IO a)
                      -> IO a
asyncDownloadPackages _ _ [] body = body Map.empty
asyncDownloadPackages verbosity mkTransport pkglocs0 body = do
    --TODO: use parallel downloads? if so, use the fetchLimit

    asyncDownloadVars <- mapM (\loc -> (,) loc <$> newEmptyMVar) pkglocs0
    transport         <- mkTransport

    let downloadAction = 
          mapM $ \(pkgloc, var) ->
            fetchPackage transport verbosity pkgloc >>= putMVar var

    withAsync (downloadAction asyncDownloadVars) $ \_ ->
      let !asyncDownloadMap = Map.fromList asyncDownloadVars
      in body asyncDownloadMap


-- | Check if a package needs downloading, and if so expect to find a download
-- in progress in the given 'AsyncDownloadMap' and wait on it to finish.
--
waitAsyncPackageDownload :: Verbosity
                         -> AsyncDownloadMap
                         -> ElaboratedConfiguredPackage
                         -> IO (PackageLocation FilePath)
waitAsyncPackageDownload verbosity downloadMap pkg = do
    mloc <- checkFetched (pkgSourceLocation pkg)
    case mloc of
      Nothing  -> do let Just hnd = Map.lookup (pkgSourceLocation pkg) downloadMap
                     debug verbosity $ "Waiting for download of "
                                    ++ display (packageId pkg)
                                    ++ " to finish"
                     takeMVar hnd
      Just loc -> return loc
--TODO: do the exception handling on download stuff


-- for the build cache we use one of two methods:
--  for build type Simple we assume we do actually know the files in the package
--  for other build types we look at all files, only excluding known boring


data GenericBuildResult ipkg iresult ifailure
                  = BuildFailure ifailure
                  | BuildSuccess (Maybe ipkg) iresult
  deriving (Eq, Show, Generic)

instance (Binary ipkg, Binary iresult, Binary ifailure) =>
         Binary (GenericBuildResult ipkg iresult ifailure)


executeInstallPlan
  :: forall ipkg srcpkg iresult.
     (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
      HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
  => Verbosity
  -> JobControl IO ( GenericReadyPackage srcpkg ipkg
                   , GenericBuildResult ipkg iresult BuildFailure )
  -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
  -> (GenericReadyPackage srcpkg ipkg
       -> IO (GenericBuildResult ipkg iresult BuildFailure))
  -> IO (GenericInstallPlan ipkg srcpkg iresult BuildFailure)
executeInstallPlan verbosity jobCtl plan0 installPkg =
    tryNewTasks 0 plan0
  where
    tryNewTasks taskCount plan = do
      case InstallPlan.ready plan of
        [] | taskCount == 0 -> return plan
           | otherwise      -> waitForTasks taskCount plan
        pkgs                -> do
          sequence_
            [ do info verbosity $ "Ready to install " ++ display pkgid
                 spawnJob jobCtl $ do
                   buildResult <- installPkg pkg
                   return (pkg, buildResult)
            | pkg <- pkgs
            , let pkgid = packageId pkg
            ]

          let taskCount' = taskCount + length pkgs
              plan'      = InstallPlan.processing pkgs plan
          waitForTasks taskCount' plan'

    waitForTasks taskCount plan = do
      info verbosity $ "Waiting for install task to finish..."
      (pkg, buildResult) <- collectJob jobCtl
      let taskCount' = taskCount-1
          plan'      = updatePlan pkg buildResult plan
      tryNewTasks taskCount' plan'

    updatePlan :: GenericReadyPackage srcpkg ipkg
               -> GenericBuildResult ipkg iresult BuildFailure
               -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
               -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
    updatePlan pkg (BuildSuccess mipkg buildSuccess) =
        InstallPlan.completed (installedPackageId pkg) mipkg buildSuccess

    updatePlan pkg (BuildFailure buildFailure) =
        InstallPlan.failed (installedPackageId pkg) buildFailure depsFailure
      where
        depsFailure = DependentFailed (packageId pkg)
        -- So this first pkgid failed for whatever reason (buildFailure).
        -- All the other packages that depended on this pkgid, which we
        -- now cannot build, we mark as failing due to 'DependentFailed'
        -- which kind of means it was not their fault.


-- | Ensure that the package is unpacked in an appropriate directory, either
-- a temporary one or a persistent one under the shared dist directory. 
--
withPackageInLocalDirectory
  :: Verbosity
  -> DistDirLayout
  -> PackageLocation FilePath
  -> BuildStyle
  -> PackageIdentifier
  -> (FilePath -> FilePath -> IO a)
  -> IO a
withPackageInLocalDirectory verbosity DistDirLayout{..}
                            location buildstyle pkgid buildPkg =

    case location of

      -- For the case of a user-managed local dir, irrespective of the build
      -- style, we build from that directory and put build artifacts under the
      -- shared dist directory.
      LocalUnpackedPackage srcdir ->
        buildPkg srcdir (distBuildDirectory pkgid)

      -- The three tarball cases are handled the same as each other,
      -- though depending on the build style.
      LocalTarballPackage    tarball -> withTarballLocalDirectory tarball
      RemoteTarballPackage _ tarball -> withTarballLocalDirectory tarball
      RepoTarballPackage _ _ tarball -> withTarballLocalDirectory tarball

  where
    withTarballLocalDirectory tarball =
      case buildstyle of
        -- In this case we make a temp dir, unpack the tarball to there and
        -- build and install it from that temp dir.
        BuildAndInstall -> do
        -- TODO: this is also a case where we can calculate the installed pkgid
          withTempDirectory verbosity distTempDirectory
                            (display (packageName pkgid)) $ \tmpdir -> do
            extractTarballPackage verbosity tarball tmpdir pkgid 
            let srcdir   = tmpdir </> display pkgid
                builddir = srcdir </> "dist"
            buildPkg srcdir builddir

        -- In this case we make sure the tarball has been unpacked to the
        -- appropriate location under the shared dist dir, and then build it
        -- inplace there
        BuildInplaceOnly -> do
          -- TODO: do a timestamp comparison and re-unpack if the tarball is updated
          -- TODO: re-overwrite the .cabal file if it gets updated in the repo
          let srcrootdir = distUnpackedSrcRootDirectory
              srcdir     = distUnpackedSrcDirectory pkgid
              builddir   = distBuildDirectory pkgid
          exists <- doesDirectoryExist srcdir
          unless exists $ do
            createDirectoryIfMissingVerbose verbosity False srcrootdir
            extractTarballPackage verbosity tarball srcrootdir pkgid
          buildPkg srcdir builddir


extractTarballPackage :: Verbosity -> FilePath -> FilePath -> PackageId -> IO ()
extractTarballPackage verbosity tarball parentdir pkgid = do
    info verbosity $ "Extracting " ++ tarball ++ " to " ++ parentdir ++ "..."
    Tar.extractTarGzFile parentdir pkgsubdir tarball

    -- sanity check:
    exists <- doesFileExist cabalFile
    when (not exists) $
      die $ "Package .cabal file not found: " ++ show cabalFile
    
    --TODO: overwrite the .cabal file with the one from the index, when appropriate
    --TODO: move any pre-shipped dist dir into the right place
  where
    cabalFile = parentdir </> pkgsubdir
                          </> display (packageName pkgid) <.> "cabal"
    pkgsubdir = display pkgid

-- Override the .cabal file if necessary
{-
overridePackageCabalFile ... =
  case pkgoverride of
    Nothing     -> return ()
    Just pkgtxt -> do
      let descFilePath = fromMaybe "." workingDir
                     </> display (packageName pkgid) <.> "cabal"
      info verbosity $
        "Updating " ++ display (packageName pkgid) <.> "cabal"
                    ++ " with the latest revision from the index."
      writeFileAtomic descFilePath pkgtxt
-}

buildAndInstallUnpackedPackage :: Verbosity
                               -> DistDirLayout -> CabalDirLayout
                               -> Bool -> Lock -> Lock
                               -> ElaboratedSharedConfig
                               -> ElaboratedReadyPackage
                               -> FilePath -> FilePath
                               -> IO BuildResult
buildAndInstallUnpackedPackage verbosity
                               DistDirLayout{distTempDirectory}
                               cabalDirLayout
                               isParallelBuild installLock cacheLock
                               pkgshared@ElaboratedSharedConfig {
                                 pkgConfigPlatform  = platform,
                                 pkgConfigCompiler  = compiler,
                                 pkgConfigProgramDb = progdb
                               }
                               rpkg@(ReadyPackage pkg _deps)
                               srcdir builddir = do

    putStrLn $ "buildAndInstallUnpackedPackage: " ++ srcdir ++ " " ++ builddir
    createDirectoryIfMissingVerbose verbosity False builddir

    --TODO: deal consistently with talking to older Setup.hs versions, much like
    --      we do for ghc, with a proper options type and rendering step
    --      which will also let us call directly into the lib, rather than always
    --      going via the lib's command line interface, which would also allow
    --      passing data like installed packages, compiler, and program db for a
    --      quicker configure.

    --TODO: optional logging
    --TODO: exception handling to tag things with the phase they failed in
    --TODO: docs and tests
    --TODO: win32-self-replacement
    --TODO: sudo re-exec

    -- Configure phase
    notice verbosity $ "(cabal-install) Configuring " ++ display pkgid ++ "..."
    setup configureCommand' configureFlags

    -- Build phase
    notice verbosity $ "(cabal-install) Building " ++ display pkgid ++ "..."
    setup buildCommand' buildFlags

    -- Install phase
    criticalSection installLock $ do
      --TODO: do we need the installLock for copying? can we not do that in
      -- parallel? Isn't it just registering that we have to lock for?
      --TODO: need to lock installing this ipkig so other processes don't
      -- stomp on our files, since we don't have ABI compat, not safe to replace

      -- Actual installation
      setup Cabal.copyCommand copyFlags
      -- here's where we could keep track of the installed files ourselves if
      -- we wanted by calling copy to an image dir and then we would make a
      -- manifest and move it to its final location
      --TODO: we should actually have it make an image in store/incomming and
      -- then when it's done, move it to its final location, to reduce problems
      -- with installs failing half-way. Could also register and then move.

      -- For libraries, grab the package configuration file
      -- and register it ourselves
      mipkg <- generateInstalledPackageInfo
      case mipkg of
        Nothing   -> return ()
        Just ipkg -> do
          -- We register ourselves rather than via Setup.hs, because it's a bit
          -- cleaner (Setup.hs isn't allowed to expect to be able to modify the
          -- target system during register, it must be able to make a reg file).
         Cabal.registerPackage verbosity compiler progdb
                               True -- request multi-instance, nix style
                               (pkgRegisterPackageDBStack pkg) ipkg

      let docsResult  = DocsNotTried
          testsResult = TestsNotTried
      return (BuildSuccess mipkg (BuildOk docsResult testsResult))

  where
    pkgid  = packageId rpkg
    ipkgid = installedPackageId rpkg

    --FIXME: this is rediculous:
    pkgdesc = case Cabal.finalizePackageDescription
                     (pkgFlagAssignment pkg)
                     (const True)
                     platform (compilerInfo compiler) []
                     (enableStanzas (pkgEnabledStanzas pkg)
                                    (pkgDescription pkg))
                of Left _ -> error "finalizePackageDescription ReadyPackage failed"
                   Right (desc, _) -> desc


    configureCommand'= Cabal.configureCommand defaultProgramConfiguration
    --TODO: proper set of config options, not just Setup.hs interface flags
    configureFlags _ = setupHsConfigureFlags rpkg pkgshared
                                             cabalDirLayout (Just ipkgid)
                                             verbosity builddir

    buildCommand'    = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = setupHsBuildFlags pkg pkgshared verbosity builddir

    generateInstalledPackageInfo :: IO (Maybe InstalledPackageInfo)
    generateInstalledPackageInfo
      | not shouldRegister = return Nothing
      | otherwise = do
      absTmpDir <- canonicalizePath distTempDirectory --since we change dir
      withTempFile absTmpDir "package-registration-" $ \pkgConfFile hnd -> do
        hClose hnd
        let registerFlags _ = setupHsRegisterFlags pkg pkgshared
                                                   verbosity builddir
                                                   pkgConfFile
        setup Cabal.registerCommand registerFlags

        (warns, ipkg) <- withUTF8FileContents pkgConfFile $ \pkgConfStr ->
          case Installed.parseInstalledPackageInfo pkgConfStr of
            Installed.ParseFailed perror -> pkgConfParseFailed perror
            Installed.ParseOk warns ipkg -> return (warns, ipkg)

        unless (null warns) $
          warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)

        return $ Just ipkg {
            -- Important: we decide what the installed package id is, not
            -- the build system.
            Installed.installedPackageId = ipkgid
            --TODO: re-exported modules can also mention this ipkgid, and we'd need to update that too
          }

      where
        shouldRegister = Cabal.hasLibs pkgdesc
        pkgConfParseFailed :: Installed.PError -> IO a
        pkgConfParseFailed perror =
          die $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
                ++ show perror

    copyFlags _ = setupHsCopyFlags pkg pkgshared verbosity builddir

    scriptOptions = setupHsScriptOptions rpkg pkgshared srcdir builddir
                                         isParallelBuild cacheLock

--    setup :: CommandUI flags -> (Version -> flags) -> IO ()
    setup cmd flags =
      setupWrapper verbosity
                   scriptOptions
                   (Just pkgdesc)
                   cmd flags []


buildInplaceUnpackedPackage :: Verbosity
                            -> DistDirLayout -> CabalDirLayout
                            -> Bool -> Lock
                            -> ElaboratedSharedConfig
                            -> GenericReadyPackage ElaboratedConfiguredPackage
                                                   InstalledPackageInfo
                            -> FilePath -> FilePath
                            -> IO BuildResult
buildInplaceUnpackedPackage verbosity
                            DistDirLayout {
                              distTempDirectory,
                              distPackageCacheFile,
                              distPackageCacheDirectory
                            }
                            cabalDirLayout
                            isParallelBuild cacheLock
                            pkgshared@ElaboratedSharedConfig {
                              pkgConfigPlatform  = platform,
                              pkgConfigCompiler  = compiler,
                              pkgConfigProgramDb = progdb
                            }
                            rpkg@(ReadyPackage pkg _deps)
                            srcdir builddir = do

    putStrLn $ "buildInplaceUnpackedPackage: " ++ srcdir ++ " " ++ builddir

    --TODO: also have to rebuild if any deps changed

    configChanged <- checkFileMonitorChanged configFileMonitor srcdir rpkg
    buildChanged  <- checkFileMonitorChanged buildFileMonitor  srcdir ()
    
    case (configChanged, buildChanged) of
      (Unchanged ((), _), Unchanged (buildResult, _)) -> return buildResult

      _ -> do
        --TODO: there is duplication between the distdirlayout and the builddir here
        --      builddir is not enough, we also need the per-package cachedir
        createDirectoryIfMissingVerbose verbosity False builddir
        createDirectoryIfMissingVerbose verbosity False (distPackageCacheDirectory pkgid)
        createPackageDBIfMissing verbosity compiler progdb (pkgBuildPackageDBStack pkg)

        -- Configure phase
        notice verbosity $ "Configuring " ++ display pkgid ++ "..."
        --TODO: think about these install dirs, we're not planning to install, but
        -- where should we configure them for?
        setup configureCommand' (\_ -> configureFlags)

        -- Build phase
        notice verbosity $ "Building " ++ display pkgid ++ "..."
        setup buildCommand' buildFlags

        -- Register locally
        --TODO: only need to re-register if input package config has changed
        --      (or if not registered in the first place)
        mipkg <- generateInstalledPackageInfo
        case mipkg of
          Nothing   -> return ()
          Just ipkg -> do
            -- We register ourselves rather than via Setup.hs, because it's a bit
            -- cleaner (Setup.hs isn't allowed to expect to be able to modify the
            -- target system during register, it must be able to make a reg file).
            Cabal.registerPackage verbosity compiler progdb False
                                  (pkgRegisterPackageDBStack pkg) ipkg

        let docsResult  = DocsNotTried
            testsResult = TestsNotTried
            
            buildResult :: BuildResult
            buildResult = BuildSuccess mipkg (BuildOk docsResult testsResult)

        updateFileMonitor configFileMonitor srcdir
                          []
                          rpkg ()

        allSrcFiles <- getDirectoryContentsRecursive srcdir

        updateFileMonitor buildFileMonitor srcdir
                          (map MonitorFileHashed allSrcFiles)
                          () buildResult

        return buildResult

  where
    pkgid  = packageId rpkg
    ipkgid = installedPackageId rpkg

    configFileMonitor = FileMonitorName (distPackageCacheFile pkgid "config")
    buildFileMonitor  = FileMonitorName (distPackageCacheFile pkgid "build")

    --FIXME: this is ridiculous, it's only needed for the setupWrapper, and
    -- it only needs a couple little bits from it
    pkgdesc = case Cabal.finalizePackageDescription
                     (pkgFlagAssignment pkg)
                     (const True)
                     platform (compilerInfo compiler) []
                     (enableStanzas (pkgEnabledStanzas pkg)
                                    (pkgDescription pkg))
                of Left _ -> error "finalizePackageDescription ReadyPackage failed"
                   Right (desc, _) -> desc

    configureCommand'= Cabal.configureCommand defaultProgramConfiguration
    --TODO: proper set of config options, not just Setup.hs interface flags
    --      or, get passed proper package config and translate into Setup.hs
    --      interface flags (or intermediate, and only translate if we go via
    --      the CLI)
    configureFlags   = setupHsConfigureFlags rpkg pkgshared
                                             cabalDirLayout Nothing
                                             verbosity builddir

    buildCommand'    = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = setupHsBuildFlags pkg pkgshared
                                         verbosity builddir

    scriptOptions    = setupHsScriptOptions rpkg pkgshared
                                            srcdir builddir
                                            isParallelBuild cacheLock

    setup cmd flags =
      setupWrapper verbosity
                   scriptOptions
                   (Just pkgdesc)
                   cmd flags []

    generateInstalledPackageInfo :: IO (Maybe InstalledPackageInfo)
    generateInstalledPackageInfo
      | not shouldRegister = return Nothing
      | otherwise = do
      withTempFile distTempDirectory "package-registration-" $ \pkgConfFile hnd -> do
        hClose hnd
        let registerFlags _ = setupHsRegisterFlags pkg pkgshared
                                                   verbosity builddir
                                                   pkgConfFile
        setup Cabal.registerCommand registerFlags

        (warns, ipkg) <- withUTF8FileContents pkgConfFile $ \pkgConfStr ->
          case Installed.parseInstalledPackageInfo pkgConfStr of
            Installed.ParseFailed perror -> pkgConfParseFailed perror
            Installed.ParseOk warns ipkg -> return (warns, ipkg)

        unless (null warns) $
          warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)

        return $ Just ipkg {
            -- Important: we decide what the installed package id is, not
            -- the build system.
            Installed.installedPackageId = ipkgid
          }

      where
        shouldRegister = Cabal.hasLibs pkgdesc
        pkgConfParseFailed :: Installed.PError -> IO a
        pkgConfParseFailed perror =
          die $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
                ++ show perror

{-
printPlan :: Verbosity -> [ReadyPackage] -> IO ()
printPlan verbosity pkgs = do
    notice verbosity $ unlines $
        ("In order, the following would be installed:")
      : map showPkgAndReason pkgs
  where

    showPkgAndReason pkg = display (packageId pkg)
                        ++ showStanzas (stanzas pkg)
                        ++ " " ++ showBuildStyle pkg


    stanzas :: ReadyPackage -> [OptionalStanza]
    stanzas (ReadyPackage _ _ sts _ _) = sts

    showStanzas :: [OptionalStanza] -> String
    showStanzas = concatMap ((' ' :) . showStanza)
    showStanza TestStanzas  = "*test"
    showStanza BenchStanzas = "*bench"

    showBuildStyle (ReadyPackage _ _ _ _ buildstyle) = show buildstyle


linearizeInstallPlan :: InstallPlan -> [ReadyPackage]
linearizeInstallPlan plan =
    unfoldr next plan
  where
    next plan' = case InstallPlan.ready plan' of
      []      -> Nothing
      (pkg:_) -> Just (pkg, plan'')
        where
          pkgid  = installedPackageId pkg
          plan'' = InstallPlan.completed pkgid
                     (BuildOk DocsNotTried TestsNotTried
                              (Just $ Installed.emptyInstalledPackageInfo
                              { Installed.sourcePackageId = packageId pkg
                              , Installed.installedPackageId = pkgid }))
                     (InstallPlan.processing [pkg] plan')
          --FIXME: This is a bit of a hack,
          -- pretending that each package is installed
          -- It's doubly a hack because the installed package ID
          -- didn't get updated...
-}

printPlan :: GenericInstallPlan ipkg ElaboratedConfiguredPackage iresult ifailure -> String
printPlan installPlan =
      "These packages are using local inplace builds: " ++
      intercalate ", "
        [ display (packageId pkg)
        | InstallPlan.Configured pkg@ElaboratedConfiguredPackage { pkgBuildStyle = BuildInplaceOnly }
            <- InstallPlan.toList installPlan
        ]

       

-------------------------------
-- Calculating package hashes
--

-- The basic idea is simple, hash the combination of:
--
--   * the package tarball
--   * the ids of all the direct dependencies
--   * other local configuration (flags, profiling, etc)

-- | Calculate a 'InstalledPackageId' for a package using our nix-style
-- inputs hashing method.
--
hashedInstalledPackageId :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageId pkghashinputs@PackageHashInputs{pkgHashPkgId} =
    InstalledPackageId $
         display pkgHashPkgId   -- to be a bit user friendly
      ++ "-"
      ++ showHashValue (hashPackageHashInputs pkghashinputs)

-- | All the information that contribues to a package's hash, and thus its
-- 'InstalledPackageIds'.
--
data PackageHashInputs = PackageHashInputs {
       pkgHashPkgId         :: PackageId,
       pkgHashSourceHash    :: PackageSourceHash,
       pkgHashDirectDeps    :: [InstalledPackageId],
       pkgHashOtherConfig   :: PackageHashConfigInputs
     }

type PackageSourceHash = HashValue

-- | Those parts of the package configuration that contribute to the
-- package hash.
--
data PackageHashConfigInputs = PackageHashConfigInputs {
       pkgHashCompilerId          :: CompilerId,
       pkgHashPlatform            :: Platform,
       pkgHashFlagAssignment      :: FlagAssignment, -- complete not partial
       pkgHashConfigureScriptArgs :: [String], -- just ./configure for build-type Configure
       pkgHashVanillaLib          :: Bool,
       pkgHashSharedLib           :: Bool,
       pkgHashDynExe              :: Bool,
       pkgHashGHCiLib             :: Bool,
       pkgHashProfLib             :: Bool,
       pkgHashProfExe             :: Bool,
       pkgHashProfLibDetail       :: ProfDetailLevel,
       pkgHashProfExeDetail       :: ProfDetailLevel,
       pkgHashLibCoverage         :: Bool,
       pkgHashExeCoverage         :: Bool,
       pkgHashOptimization        :: OptimisationLevel,
       pkgHashSplitObjs           :: Bool,
       pkgHashStripLibs           :: Bool,
       pkgHashStripExes           :: Bool,
       pkgHashDebugInfo           :: DebugInfoLevel,
       pkgHashExtraLibDirs        :: [FilePath],
       pkgHashExtraIncludeDirs    :: [FilePath],
       pkgHashProgPrefix          :: Maybe PathTemplate,
       pkgHashProgSuffix          :: Maybe PathTemplate

--     TODO: pkgHashToolsVersions     ?
--     TODO: pkgHashToolsExtraOptions ?
--     TODO: and what about docs?
     }
  deriving Show

-- | Calculate the overall hash to be used for an 'InstalledPackageId'
--
hashPackageHashInputs :: PackageHashInputs -> HashValue
hashPackageHashInputs = hashValue . renderPackageHashInputs

renderPackageHashInputs :: PackageHashInputs -> LBS.ByteString
renderPackageHashInputs PackageHashInputs{
                          pkgHashPkgId,
                          pkgHashSourceHash,
                          pkgHashDirectDeps,
                          pkgHashOtherConfig =
                            PackageHashConfigInputs{..}
                        } =
    -- The purpose of this somewhat laboured rendering (e.g. why not just
    -- use show?) is so that existing package hashes do not change
    -- unnecessarily when new configuration inputs are added into the hash.

    -- In particular, the assumption is that when a new configuration input
    -- is included into the hash, that existing packages will typically get
    -- the default value for that feature. So if we avoid adding entries with
    -- the default value then most of the time adding new features will not
    -- change the hashes of existing packages and so fewer packages will need
    -- to be rebuilt. 

    --TODO: ultimately we probably want to put this config info into the
    -- ghc-pkg db. At that point this should probably be changed to use the
    -- config file infrastructure so it can be read back in again.
    LBS.Char8.pack $ unlines $ catMaybes
      [ entry "pkgid"       display pkgHashPkgId
      , entry "src"         showHashValue pkgHashSourceHash
      , entry "deps"        (intercalate ", " . map display . sort) pkgHashDirectDeps
        -- and then all the config
      , entry "compilerid"  display pkgHashCompilerId
      , entry "platform"    display pkgHashPlatform
      , opt   "flags" []    showFlagAssignment pkgHashFlagAssignment
      , opt   "configure-script" [] unwords pkgHashConfigureScriptArgs
      , opt   "vanilla-lib" True  display pkgHashVanillaLib
      , opt   "shared-lib"  False display pkgHashSharedLib
      , opt   "dynamic-exe" False display pkgHashDynExe
      , opt   "ghci-lib"    False display pkgHashGHCiLib
      , opt   "prof-lib"    False display pkgHashProfLib
      , opt   "prof-exe"    False display pkgHashProfExe
      , opt   "prof-lib-detail" ProfDetailDefault showProfDetailLevel pkgHashProfLibDetail 
      , opt   "prof-exe-detail" ProfDetailDefault showProfDetailLevel pkgHashProfExeDetail 
      , opt   "hpc-lib"      False display pkgHashLibCoverage
      , opt   "hpc-exe"      False display pkgHashExeCoverage
      , opt   "optimisation" NormalOptimisation (show . fromEnum) pkgHashOptimization
      , opt   "split-objs"   False display pkgHashSplitObjs
      , opt   "stripped-lib" False display pkgHashStripLibs
      , opt   "stripped-exe" True  display pkgHashStripExes
      , opt   "debug-info"   NormalDebugInfo (show . fromEnum) pkgHashDebugInfo
      , opt   "extra-lib-dirs"     [] unwords pkgHashExtraLibDirs
      , opt   "extra-include-dirs" [] unwords pkgHashExtraIncludeDirs
      , opt   "prog-prefix" Nothing (maybe "" InstallDirs.fromPathTemplate) pkgHashProgPrefix
      , opt   "prog-suffix" Nothing (maybe "" InstallDirs.fromPathTemplate) pkgHashProgSuffix
      ]
  where
    entry key     format value = Just (key ++ ": " ++ format value)
    opt   key def format value
         | value == def = Nothing
         | otherwise    = entry key format value

    showFlagAssignment = unwords . map showEntry . sortBy (compare `on` fst)
      where
        showEntry (Cabal.FlagName name, False) = '-' : name
        showEntry (Cabal.FlagName name, True)  = '+' : name

-----------------------------------------------
-- The specific choice of hash implementation
--

-- Is a crypto hash necessary here? One thing to consider is who controls the
-- inputs and what's the result of a hash collision. Obviously we should not
-- install packages we don't trust because they can run all sorts of code, but
-- if I've checked there's no TH, no custom Setup etc, is there still a
-- problem? If someone provided us a tarball that hashed to the same value as
-- some other package and we installed it, we could end up re-using that
-- installed package in place of another one we wanted. So yes, in general
-- there is some value in preventing intentional hash collisions in installed
-- package ids.

newtype HashValue = HashValue (SHA.Digest SHA.SHA256State)
  deriving (Eq, Show, Binary)

hashValue :: LBS.ByteString -> HashValue
hashValue = HashValue . SHA.sha256

showHashValue :: HashValue -> String
showHashValue (HashValue digest) = SHA.showDigest digest

readFileHashValue :: FilePath -> IO HashValue
readFileHashValue tarball =
    withBinaryFile tarball ReadMode $ \hnd ->
      evaluate . hashValue =<< LBS.hGetContents hnd


{-
makeRelativeFully :: FilePath -> FilePath -> IO FilePath
makeRelativeFully base target = do
    cbase   <- canonicalizePath base 
    ctarget <- canonicalizePath target
    go (splitPath cbase)
       (splitPath ctarget)
  where
    go (b:bs) (t:ts) | b == t = go bs ts
    go bs ts = return $ joinPath (replicate (length bs) ".." ++ ts)
-}

