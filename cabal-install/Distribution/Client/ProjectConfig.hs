{-# LANGUAGE RecordWildCards, NamedFieldPuns, DeriveGeneric #-}

-- | Handling project configuration, types and reading.
--
module Distribution.Client.ProjectConfig (

    -- * Types for project config
    ProjectConfig(..),
    ProjectConfigBuildOnly(..),
    ProjectConfigSolver(..),
    PackageConfigShared(..),
    PackageConfig(..),
    projectConfigRepos,

    -- * Reading config
    readProjectConfig,
    convertCommandLineFlags,

    -- * build time settings
    BuildTimeSettings(..),
    resolveBuildTimeSettings,
  ) where

import Distribution.Client.RebuildMonad

import Distribution.Client.Types
import Distribution.Client.Dependency.Types
import Distribution.Client.Targets
import Distribution.Client.DistDirLayout
import Distribution.Client.Glob
import Distribution.Client.Setup
import Distribution.Client.BuildReports.Types (ReportLevel(..))

import Distribution.Package
import Distribution.Version
import Distribution.System
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo, CompilerFlavor, PackageDB
         , OptimisationLevel, ProfDetailLevel, DebugInfoLevel )
import Distribution.Simple.Setup
         ( Flag, toFlag, flagToMaybe, flagToList, fromFlag, HaddockFlags(..) )
import Distribution.Simple.InstallDirs
         ( InstallDirs, PathTemplate, fromPathTemplate
         , toPathTemplate, substPathTemplate, initialPathTemplateEnv )
import Distribution.Client.Utils (determineNumJobs)
import Distribution.Utils.NubList
import Distribution.Verbosity
import Distribution.Text

import Control.Applicative
import Control.Monad.State as State
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Binary
import GHC.Generics (Generic)
import System.FilePath hiding (combine)
import System.Directory


-------------------------------
-- Project config
--

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
       projectConfigSolverCabalVersion      :: Flag Version,  --TODO: [required eventually] unused
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
       packageConfigProf                :: Flag Bool, --TODO: [code cleanup] sort out
       packageConfigProfLib             :: Flag Bool, --      this duplication
       packageConfigProfExe             :: Flag Bool, --      and consistency
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
       packageConfigTests               :: Flag Bool, --TODO: [required eventually] use this
       packageConfigBenchmarks          :: Flag Bool, --TODO: [required eventually] use this
       packageConfigCoverage            :: Flag Bool,
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

instance Binary ProjectConfig
instance Binary ProjectConfigSolver
instance Binary ProjectConfigBuildOnly
instance Binary PackageConfigShared
instance Binary PackageConfig



-- | Reads an explicit @cabal.project@ file in the given project root dir,
-- or returns the default project config for an implicitly defined project.
--
readProjectConfig :: FilePath
                  -> ProjectConfigSolver
                  -> PackageConfigShared
                  -> PackageConfig
                  -> Rebuild ProjectConfig
readProjectConfig projectRootDir
                  cliConfigSolver cliConfigAllPackages
                  cliConfigLocalPackages = do
    let projectFile = projectRootDir</> "cabal.project"
    usesExplicitProjectRoot <- liftIO $ doesFileExist projectFile
    if usesExplicitProjectRoot
      then do monitorFiles [MonitorFileHashed projectFile]
              parseProjectConfig <$> liftIO (readFile projectFile)

      else do monitorFiles [MonitorNonExistantFile projectFile]
              return defaultImplicitProjectConfig
  where
    --TODO: [required feature] this is just a placeholder, need to actually read these settings
    -- from the project file and combine them with the user-wide config and cli
    parseProjectConfig content =
      ProjectConfig
        projectRootDir
        (catMaybes (map simpleParse (lines content)))
        mempty
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
        mempty
        cliConfigSolver
        cliConfigAllPackages
        cliConfigLocalPackages                     
        Map.empty

convertCommandLineFlags :: GlobalFlags
                        -> ConfigFlags  -> ConfigExFlags
                        -> InstallFlags -> HaddockFlags
                        -> ( ( ProjectConfigSolver
                             , PackageConfigShared
                             , PackageConfig
                             )
                           , ProjectConfigBuildOnly
                           )
convertCommandLineFlags globalFlags configFlags configExFlags
                        installFlags haddockFlags =
    ( ( ProjectConfigSolver{..}
      , PackageConfigShared{..}
      , PackageConfig{..}
      )
    , ProjectConfigBuildOnly{..}
    )
  where
    GlobalFlags {
      globalVersion           = _,
      globalNumericVersion    = _,
      globalConfigFile        = _, -- TODO: [required feature]
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
      configCoverage            = coverage,
      configLibCoverage         = libcoverage, --deprecated
      configExactConfiguration  = _,
      configFlagError           = _,
      configRelocatable         = packageConfigRelocatable,
      configDebugInfo           = packageConfigDebugInfo
    } = configFlags

    packageConfigCoverage       = coverage <> libcoverage

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


data BuildTimeSettings
   = BuildTimeSettings {
       buildSettingDryRun                :: Bool,
       buildSettingOnlyDeps              :: Bool,
       buildSettingSummaryFile           :: [PathTemplate],
       buildSettingLogFile               :: Maybe (Compiler  -> Platform
                                                -> PackageId -> LibraryName
                                                             -> FilePath),
       buildSettingLogVerbosity          :: Verbosity,
       buildSettingBuildReports          :: ReportLevel,
       buildSettingReportPlanningFailure :: Bool,
       buildSettingSymlinkBinDir         :: [FilePath],
       buildSettingOneShot               :: Bool,
       buildSettingNumJobs               :: Int,
       buildSettingOfflineMode           :: Bool,
       buildSettingKeepTempFiles         :: Bool,
       buildSettingHttpTransport         :: Maybe String
     }

instance Monoid ProjectConfigBuildOnly where
  mempty = 
    ProjectConfigBuildOnly {
      projectConfigVerbosity             = mempty,
      projectConfigDryRun                = mempty,
      projectConfigOnlyDeps              = mempty,
      projectConfigSummaryFile           = mempty,
      projectConfigLogFile               = mempty,
      projectConfigBuildReports          = mempty,
      projectConfigReportPlanningFailure = mempty,
      projectConfigSymlinkBinDir         = mempty,
      projectConfigOneShot               = mempty,
      projectConfigNumJobs               = mempty,
      projectConfigOfflineMode           = mempty,
      projectConfigKeepTempFiles         = mempty,
      projectConfigHttpTransport         = mempty,
      projectConfigCacheDir              = mempty,
      projectConfigLogsDir               = mempty,
      projectConfigWorldFile             = mempty
    }
  mappend a b =
    ProjectConfigBuildOnly {
      projectConfigVerbosity             = combine projectConfigVerbosity,
      projectConfigDryRun                = combine projectConfigDryRun,
      projectConfigOnlyDeps              = combine projectConfigOnlyDeps,
      projectConfigSummaryFile           = combine projectConfigSummaryFile,
      projectConfigLogFile               = combine projectConfigLogFile,
      projectConfigBuildReports          = combine projectConfigBuildReports,
      projectConfigReportPlanningFailure = combine projectConfigReportPlanningFailure,
      projectConfigSymlinkBinDir         = combine projectConfigSymlinkBinDir,
      projectConfigOneShot               = combine projectConfigOneShot,
      projectConfigNumJobs               = combine projectConfigNumJobs,
      projectConfigOfflineMode           = combine projectConfigOfflineMode,
      projectConfigKeepTempFiles         = combine projectConfigKeepTempFiles,
      projectConfigHttpTransport         = combine projectConfigHttpTransport,
      projectConfigCacheDir              = combine projectConfigCacheDir,
      projectConfigLogsDir               = combine projectConfigLogsDir,
      projectConfigWorldFile             = combine projectConfigWorldFile
    }
    where combine field = field a `mappend` field b


resolveBuildTimeSettings :: Verbosity
                         -> CabalDirLayout
                         -> ProjectConfigBuildOnly
                         -> ProjectConfigBuildOnly
                         -> BuildTimeSettings
resolveBuildTimeSettings verbosity CabalDirLayout{cabalLogsDirectory}
                         fromProjectFile
                         fromCommandLine =
    BuildTimeSettings {..}
  where
    buildSettingDryRun        = fromFlag    projectConfigDryRun
    buildSettingOnlyDeps      = fromFlag    projectConfigOnlyDeps
    buildSettingSummaryFile   = fromNubList projectConfigSummaryFile
    --buildSettingLogFile       -- defined below, more complicated 
    --buildSettingLogVerbosity  -- defined below, more complicated
    buildSettingBuildReports  = fromFlag    projectConfigBuildReports
    buildSettingSymlinkBinDir = flagToList  projectConfigSymlinkBinDir
    buildSettingOneShot       = fromFlag    projectConfigOneShot
    buildSettingNumJobs       = determineNumJobs projectConfigNumJobs
    buildSettingOfflineMode   = fromFlag    projectConfigOfflineMode
    buildSettingKeepTempFiles = fromFlag    projectConfigKeepTempFiles
    buildSettingHttpTransport = flagToMaybe projectConfigHttpTransport
    buildSettingReportPlanningFailure
                              = fromFlag projectConfigReportPlanningFailure

    ProjectConfigBuildOnly{..} = defaults
                       `mappend` fromProjectFile
                       `mappend` fromCommandLine

    defaults = mempty {
      projectConfigDryRun                = toFlag False,
      projectConfigOnlyDeps              = toFlag False,
      projectConfigBuildReports          = toFlag NoReports,
      projectConfigReportPlanningFailure = toFlag False,
      projectConfigOneShot               = toFlag False,
      projectConfigOfflineMode           = toFlag False,
      projectConfigKeepTempFiles         = toFlag False
    }

    -- The logging logic: what log file to use and what verbosity.
    --
    -- If the user has specified --remote-build-reporting=detailed, use the
    -- default log file location. If the --build-log option is set, use the
    -- provided location. Otherwise don't use logging, unless building in
    -- parallel (in which case the default location is used).
    --
    buildSettingLogFile :: Maybe (Compiler -> Platform
                               -> PackageId -> LibraryName -> FilePath)
    buildSettingLogFile
      | useDefaultTemplate = Just (substLogFileName defaultTemplate)
      | otherwise          = fmap  substLogFileName givenTemplate

    defaultTemplate = toPathTemplate $
                        cabalLogsDirectory </> "$pkgid" <.> "log"
    givenTemplate   = flagToMaybe projectConfigLogFile

    useDefaultTemplate
      | buildSettingBuildReports == DetailedReports = True
      | isJust givenTemplate                        = False
      | isParallelBuild                             = True
      | otherwise                                   = False

    isParallelBuild = buildSettingNumJobs >= 2

    substLogFileName :: PathTemplate
                     -> Compiler -> Platform
                     -> PackageId -> LibraryName -> FilePath
    substLogFileName template compiler platform pkgid libname =
        fromPathTemplate (substPathTemplate env template)
      where
        env = initialPathTemplateEnv
                pkgid libname (compilerInfo compiler) platform

    -- If the user has specified --remote-build-reporting=detailed or
    -- --build-log, use more verbose logging.
    --
    buildSettingLogVerbosity
      | overrideVerbosity = max verbose verbosity
      | otherwise         = verbosity

    overrideVerbosity
      | buildSettingBuildReports == DetailedReports = True
      | isJust givenTemplate                        = True
      | isParallelBuild                             = False
      | otherwise                                   = False

