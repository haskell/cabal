{-# LANGUAGE RecordWildCards, NamedFieldPuns, DeriveGeneric,
             ExistentialQuantification, ScopedTypeVariables #-}

-- | Handling project configuration, types and reading.
--
module Distribution.Client.ProjectConfig (

    -- * Types for project config
    ProjectConfig(..),
    ProjectConfigBuildOnly(..),
    ProjectConfigSolver(..),
    PackageConfigShared(..),
    PackageConfig(..),
    projectConfigWithBuilderRepoContext,
    projectConfigWithSolverRepoContext,
    lookupLocalPackageConfig,
    lookupNonLocalPackageConfig,

    -- * Project config files
    findProjectRoot,
    readProjectConfig,
    writeProjectLocalExtraConfig,
    commandLineFlagsToProjectConfig,
    convertLegacyPerPackageFlags,
    convertLegacyAllPackageFlags,
    convertLegacyBuildOnlyFlags,
    convertLegacyCommandLineFlags,
    commandLineFlagsToLegacyProjectConfig,

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
import Distribution.Client.GlobalFlags (RepoContext(..), withRepoContext')
import Distribution.Client.BuildReports.Types (ReportLevel(..))
import Distribution.Client.Config (SavedConfig(..), loadConfig, defaultConfigFile)

import Distribution.Package
import Distribution.Version
import Distribution.System
import Distribution.PackageDescription (FlagAssignment, SourceRepo)
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo, CompilerFlavor, PackageDB
         , OptimisationLevel(..), ProfDetailLevel, DebugInfoLevel(..) )
import Distribution.Simple.Setup
         ( Flag(Flag), toFlag, flagToMaybe, flagToList
         , fromFlag, fromFlagOrDefault
         , ConfigFlags(..), configureOptions
         , HaddockFlags(..), haddockOptions, defaultHaddockFlags
         , programConfigurationPaths', splitArgs )
import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand
         , ConfigExFlags(..), configureExOptions, defaultConfigExFlags
         , InstallFlags(..), installOptions, defaultInstallFlags )
import Distribution.Simple.InstallDirs
         ( InstallDirs, PathTemplate, fromPathTemplate
         , toPathTemplate, substPathTemplate, initialPathTemplateEnv )
import Distribution.Simple.Program
         ( programName, knownPrograms )
import Distribution.Simple.Program.Db
         ( ProgramDb, defaultProgramDb )
import Distribution.Simple.Utils
         ( die, warn, lowercase )
import Distribution.Client.Utils (determineNumJobs)
import Distribution.Utils.NubList
import Distribution.Verbosity

import Distribution.Text
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
         ( Doc, render, text, vcat, nest, empty, isEmpty )
import Text.PrettyPrint
         ( (<+>), ($+$) )

import Distribution.ParseUtils
         ( FieldDescr(..), liftField, Field(..), readFieldsFlat
         , LineNo, ParseResult(..)
         , PError(..), syntaxError, locatedErrorMsg
         , PWarning(..), warning, showPWarning
         , simpleField, commaListField )
import Distribution.Client.ParseUtils
         ( ppFields )
import Distribution.Simple.Command
         ( CommandUI(commandOptions), ShowOrParseArgs(..)
         , OptionField, viewAsFieldDescr, option, reqArg' )

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isSpace)
import Distribution.Compat.Binary (Binary)
import Distribution.Compat.Semigroup
import GHC.Generics (Generic)
import System.FilePath hiding (combine)
import System.Directory


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

    -- Search upwards. If we get to the users home dir or the filesystem root,
    -- then use the current dir
    let probe dir | isDrive dir || dir == homedir
                  = return curdir -- implicit project root
        probe dir = do
          exists <- doesFileExist (dir </> "cabal.project")
          if exists
            then return dir       -- explicit project root
            else probe (takeDirectory dir)

    probe curdir
   --TODO: [nice to have] add compat support for old style sandboxes


-- | Read all the config relevant for a project. This includes the project
-- file if any, plus other global config.
--
readProjectConfig :: Verbosity -> FilePath
                  -> Rebuild ProjectConfig
readProjectConfig verbosity projectRootDir = do
    global <- readGlobalConfig verbosity
    local  <- readProjectLocalConfig      verbosity projectRootDir
    extra  <- readProjectLocalExtraConfig verbosity projectRootDir
    return (global <> local <> extra)


-- | Reads an explicit @cabal.project@ file in the given project root dir,
-- or returns the default project config for an implicitly defined project.
--
readProjectLocalConfig :: Verbosity -> FilePath -> Rebuild ProjectConfig
readProjectLocalConfig verbosity projectRootDir = do
  usesExplicitProjectRoot <- liftIO $ doesFileExist projectFile
  if usesExplicitProjectRoot
    then do
      monitorFiles [MonitorFileHashed projectFile]
      liftIO readProjectFile
    else do
      monitorFiles [MonitorNonExistentFile projectFile]
      return defaultImplicitProjectConfig

  where
    projectFile = projectRootDir </> "cabal.project"
    readProjectFile =
          reportParseResult verbosity "project file" projectFile
        . parseProjectConfig
      =<< readFile projectFile

    defaultImplicitProjectConfig :: ProjectConfig
    defaultImplicitProjectConfig =
      ProjectConfig
        [ GlobFile (Glob [WildCard, Literal ".cabal"])
        , GlobDir  (Glob [WildCard]) $
          GlobFile (Glob [WildCard, Literal ".cabal"])
        ]
        mempty mempty mempty mempty mempty


-- | Reads a @cabal.project.extra@ file in the given project root dir,
-- or returns empty. This file gets written by @cabal configure@, or in
-- principle can be edited manually or by other tools.
--
readProjectLocalExtraConfig :: Verbosity -> FilePath -> Rebuild ProjectConfig
readProjectLocalExtraConfig verbosity projectRootDir = do
    hasExtraConfig <- liftIO $ doesFileExist projectExtraConfigFile
    if hasExtraConfig
      then do monitorFiles [MonitorFileHashed projectExtraConfigFile]
              liftIO readProjectExtraConfigFile
      else do monitorFiles [MonitorNonExistentFile projectExtraConfigFile]
              return mempty
  where
    projectExtraConfigFile = projectRootDir </> "cabal.project.extra"

    readProjectExtraConfigFile =
          reportParseResult verbosity "project extra configuration file"
                            projectExtraConfigFile
        . parseProjectConfig
      =<< readFile projectExtraConfigFile

-- | Write a @cabal.project.extra@ file in the given project root dir.
--
writeProjectLocalExtraConfig :: FilePath -> LegacyProjectConfig -> IO ()
writeProjectLocalExtraConfig projectRootDir config =
    writeFile projectExtraConfigFile
              (showLegacyProjectConfig config)
  where
    projectExtraConfigFile = projectRootDir </> "cabal.project.extra"


-- | Read the user's @~/.cabal/config@ file.
--
readGlobalConfig :: Verbosity -> Rebuild ProjectConfig
readGlobalConfig verbosity = do
    config     <- liftIO (loadConfig verbosity mempty)
    configFile <- liftIO defaultConfigFile
    monitorFiles [MonitorFileHashed configFile]
    return (globalConfigToProjectConfig config)
    --TODO: do this properly, there's several possible locations
    -- and env vars, and flags for selecting the global config


globalConfigToProjectConfig :: SavedConfig
                            -> ProjectConfig
globalConfigToProjectConfig
    SavedConfig {
      savedGlobalFlags       = globalFlags,
      savedInstallFlags      = installFlags,
      savedConfigureFlags    = configFlags,
      savedConfigureExFlags  = configExFlags,
      savedUserInstallDirs   = _,
      savedGlobalInstallDirs = _,
      savedUploadFlags       = _,
      savedReportFlags       = _,
      savedHaddockFlags      = haddockFlags
    } =
    mempty {
      projectConfigSolver        = configSolver,
      projectConfigAllPackages   = configAllPackages,
      projectConfigLocalPackages = configLocalPackages,
      projectConfigBuildOnly     = configBuildOnly
    }
  where
    configExFlags' = defaultConfigExFlags <> configExFlags
    installFlags'  = defaultInstallFlags  <> installFlags
    haddockFlags'  = defaultHaddockFlags  <> haddockFlags

    configLocalPackages = convertLegacyPerPackageFlags
                            configFlags installFlags' haddockFlags'
    (configSolver,
     configAllPackages) = convertLegacyAllPackageFlags
                            globalFlags configFlags
                            configExFlags' installFlags'
    configBuildOnly     = convertLegacyBuildOnlyFlags
                            globalFlags configFlags
                            installFlags' haddockFlags'


-- | Convert configuration from the @cabal configure@ or @cabal build@ command
-- line into a 'ProjectConfig' value that can combined with configuration from
-- other sources.
--
commandLineFlagsToProjectConfig :: ProjectConfigSolver
                                -> PackageConfigShared
                                -> PackageConfig
                                -> ProjectConfig
commandLineFlagsToProjectConfig cliConfigSolver
                                cliConfigAllPackages
                                cliConfigLocalPackages =
    mempty {
      projectConfigSolver        = cliConfigSolver,
      projectConfigAllPackages   = cliConfigAllPackages,
      projectConfigLocalPackages = cliConfigLocalPackages
    }



-------------------------------
-- Project config types
--

data ProjectConfig
   = ProjectConfig {
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
       projectConfigIgnoreExpiry          :: Flag Bool,
       projectConfigCacheDir              :: Flag FilePath,
       projectConfigLogsDir               :: Flag FilePath,
       projectConfigWorldFile             :: Flag FilePath,
       projectConfigRootCmd               :: Flag String
     }
  deriving (Eq, Show, Generic)

data ProjectConfigSolver
   = ProjectConfigSolver {
       projectConfigSolverConstraints       :: [(UserConstraint, ConstraintSource)],
       projectConfigSolverPreferences       :: [Dependency],
       projectConfigConfigurationsFlags     :: FlagAssignment, --TODO: [required eventually] must be per-package, not global
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
       packageConfigRelocatable      :: Flag Bool
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
       packageConfigTests               :: Flag Bool,
       packageConfigBenchmarks          :: Flag Bool,
       packageConfigCoverage            :: Flag Bool,
       packageConfigDebugInfo           :: Flag DebugInfoLevel,
       packageConfigRunTests            :: Flag Bool, --TODO: [required eventually] use this
       packageConfigDocumentation       :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockHoogle       :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockHtml         :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockHtmlLocation :: Flag String, --TODO: [required eventually] use this
       packageConfigHaddockExecutables  :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockTestSuites   :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockBenchmarks   :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockInternal     :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockCss          :: Flag FilePath, --TODO: [required eventually] use this
       packageConfigHaddockHscolour     :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockHscolourCss  :: Flag FilePath, --TODO: [required eventually] use this
       packageConfigHaddockContents     :: Flag PathTemplate --TODO: [required eventually] use this
     }
  deriving (Eq, Show, Generic)

instance Binary ProjectConfig
instance Binary ProjectConfigSolver
instance Binary ProjectConfigBuildOnly
instance Binary PackageConfigShared
instance Binary PackageConfig


instance Monoid ProjectConfig where
  mempty =
    ProjectConfig {
      projectConfigPackageGlobs    = mempty,
      projectConfigBuildOnly       = mempty,
      projectConfigSolver          = mempty,
      projectConfigAllPackages     = mempty,
      projectConfigLocalPackages   = mempty,
      projectConfigSpecificPackage = mempty
    }
  mappend = (<>)

instance Semigroup ProjectConfig where
  a <> b =
    ProjectConfig {
      projectConfigPackageGlobs    = combine projectConfigPackageGlobs,
      projectConfigBuildOnly       = combine projectConfigBuildOnly,
      projectConfigSolver          = combine projectConfigSolver,
      projectConfigAllPackages     = combine projectConfigAllPackages,
      projectConfigLocalPackages   = combine projectConfigLocalPackages,
      projectConfigSpecificPackage = combine projectConfigSpecificPackage
    }
    where combine field = field a `mappend` field b


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
      projectConfigIgnoreExpiry          = mempty,
      projectConfigCacheDir              = mempty,
      projectConfigLogsDir               = mempty,
      projectConfigWorldFile             = mempty,
      projectConfigRootCmd               = mempty
    }
  mappend = (<>)

instance Semigroup ProjectConfigBuildOnly where
  a <> b =
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
      projectConfigIgnoreExpiry          = combine projectConfigIgnoreExpiry,
      projectConfigCacheDir              = combine projectConfigCacheDir,
      projectConfigLogsDir               = combine projectConfigLogsDir,
      projectConfigWorldFile             = combine projectConfigWorldFile,
      projectConfigRootCmd               = combine projectConfigRootCmd
    }
    where combine field = field a `mappend` field b


instance Monoid ProjectConfigSolver where
  mempty =
    ProjectConfigSolver {
      projectConfigSolverConstraints       = mempty,
      projectConfigSolverPreferences       = mempty,
      projectConfigConfigurationsFlags     = mempty,
      projectConfigSolverCabalVersion      = mempty,
      projectConfigSolverSolver            = mempty,
      projectConfigSolverAllowNewer        = mempty,
      projectConfigRemoteRepos             = mempty,
      projectConfigLocalRepos              = mempty,
      projectConfigSolverMaxBackjumps      = mempty,
      projectConfigSolverReorderGoals      = mempty,
      projectConfigSolverStrongFlags       = mempty,
      projectConfigSolverIndependentGoals  = mempty,
      projectConfigSolverShadowPkgs        = mempty,
      projectConfigSolverReinstall         = mempty,
      projectConfigSolverAvoidReinstalls   = mempty,
      projectConfigSolverOverrideReinstall = mempty,
      projectConfigSolverUpgradeDeps       = mempty
    }
  mappend = (<>)

instance Semigroup ProjectConfigSolver where
  a <> b =
    ProjectConfigSolver {
      projectConfigSolverConstraints       = combine projectConfigSolverConstraints,
      projectConfigSolverPreferences       = combine projectConfigSolverPreferences,
      projectConfigConfigurationsFlags     = combine projectConfigConfigurationsFlags,
      projectConfigSolverCabalVersion      = combine projectConfigSolverCabalVersion,
      projectConfigSolverSolver            = combine projectConfigSolverSolver,
      projectConfigSolverAllowNewer        = combine projectConfigSolverAllowNewer,
      projectConfigRemoteRepos             = combine projectConfigRemoteRepos,
      projectConfigLocalRepos              = combine projectConfigLocalRepos,
      projectConfigSolverMaxBackjumps      = combine projectConfigSolverMaxBackjumps,
      projectConfigSolverReorderGoals      = combine projectConfigSolverReorderGoals,
      projectConfigSolverStrongFlags       = combine projectConfigSolverStrongFlags,
      projectConfigSolverIndependentGoals  = combine projectConfigSolverIndependentGoals,
      projectConfigSolverShadowPkgs        = combine projectConfigSolverShadowPkgs,
      projectConfigSolverReinstall         = combine projectConfigSolverReinstall,
      projectConfigSolverAvoidReinstalls   = combine projectConfigSolverAvoidReinstalls,
      projectConfigSolverOverrideReinstall = combine projectConfigSolverOverrideReinstall,
      projectConfigSolverUpgradeDeps       = combine projectConfigSolverUpgradeDeps
    }
    where combine field = field a `mappend` field b


instance Monoid PackageConfigShared where
  mempty =
    PackageConfigShared {
      packageConfigProgramPaths     = mempty,
      packageConfigProgramArgs      = mempty,
      packageConfigProgramPathExtra = mempty,
      packageConfigHcFlavor         = mempty,
      packageConfigHcPath           = mempty,
      packageConfigHcPkg            = mempty,
      packageConfigVanillaLib       = mempty,
      packageConfigSharedLib        = mempty,
      packageConfigHaddockIndex     = mempty,
      packageConfigUserInstall      = mempty,
      packageConfigInstallDirs      = mempty,
      packageConfigPackageDBs       = mempty,
      packageConfigRelocatable      = mempty
    }
  mappend = (<>)

instance Semigroup PackageConfigShared where
  a <> b = PackageConfigShared {
      packageConfigProgramPaths     = combine packageConfigProgramPaths,
      packageConfigProgramArgs      = combine packageConfigProgramArgs,
      packageConfigProgramPathExtra = combine packageConfigProgramPathExtra,
      packageConfigHcFlavor         = combine packageConfigHcFlavor,
      packageConfigHcPath           = combine packageConfigHcPath,
      packageConfigHcPkg            = combine packageConfigHcPkg,
      packageConfigVanillaLib       = combine packageConfigVanillaLib,
      packageConfigSharedLib        = combine packageConfigSharedLib,
      packageConfigHaddockIndex     = combine packageConfigHaddockIndex,
      packageConfigUserInstall      = combine packageConfigUserInstall,
      packageConfigInstallDirs      = combine packageConfigInstallDirs,
      packageConfigPackageDBs       = combine packageConfigPackageDBs,
      packageConfigRelocatable      = combine packageConfigRelocatable
    }
    where combine field = field a `mappend` field b


instance Monoid PackageConfig where
  mempty =
    PackageConfig {
      packageConfigDynExe              = mempty,
      packageConfigProf                = mempty,
      packageConfigProfLib             = mempty,
      packageConfigProfExe             = mempty,
      packageConfigProfDetail          = mempty,
      packageConfigProfLibDetail       = mempty,
      packageConfigConfigureArgs       = mempty,
      packageConfigOptimization        = mempty,
      packageConfigProgPrefix          = mempty,
      packageConfigProgSuffix          = mempty,
      packageConfigExtraLibDirs        = mempty,
      packageConfigExtraIncludeDirs    = mempty,
      packageConfigGHCiLib             = mempty,
      packageConfigSplitObjs           = mempty,
      packageConfigStripExes           = mempty,
      packageConfigStripLibs           = mempty,
      packageConfigTests               = mempty,
      packageConfigBenchmarks          = mempty,
      packageConfigCoverage            = mempty,
      packageConfigDebugInfo           = mempty,
      packageConfigRunTests            = mempty,
      packageConfigDocumentation       = mempty,
      packageConfigHaddockHoogle       = mempty,
      packageConfigHaddockHtml         = mempty,
      packageConfigHaddockHtmlLocation = mempty,
      packageConfigHaddockExecutables  = mempty,
      packageConfigHaddockTestSuites   = mempty,
      packageConfigHaddockBenchmarks   = mempty,
      packageConfigHaddockInternal     = mempty,
      packageConfigHaddockCss          = mempty,
      packageConfigHaddockHscolour     = mempty,
      packageConfigHaddockHscolourCss  = mempty,
      packageConfigHaddockContents     = mempty
    }
  mappend = (<>)

instance Semigroup PackageConfig where
  a <> b = PackageConfig {
      packageConfigDynExe              = combine packageConfigDynExe,
      packageConfigProf                = combine packageConfigProf,
      packageConfigProfLib             = combine packageConfigProfLib,
      packageConfigProfExe             = combine packageConfigProfExe,
      packageConfigProfDetail          = combine packageConfigProfDetail,
      packageConfigProfLibDetail       = combine packageConfigProfLibDetail,
      packageConfigConfigureArgs       = combine packageConfigConfigureArgs,
      packageConfigOptimization        = combine packageConfigOptimization,
      packageConfigProgPrefix          = combine packageConfigProgPrefix,
      packageConfigProgSuffix          = combine packageConfigProgSuffix,
      packageConfigExtraLibDirs        = combine packageConfigExtraLibDirs,
      packageConfigExtraIncludeDirs    = combine packageConfigExtraIncludeDirs,
      packageConfigGHCiLib             = combine packageConfigGHCiLib,
      packageConfigSplitObjs           = combine packageConfigSplitObjs,
      packageConfigStripExes           = combine packageConfigStripExes,
      packageConfigStripLibs           = combine packageConfigStripLibs,
      packageConfigTests               = combine packageConfigTests,
      packageConfigBenchmarks          = combine packageConfigBenchmarks,
      packageConfigCoverage            = combine packageConfigCoverage,
      packageConfigDebugInfo           = combine packageConfigDebugInfo,
      packageConfigRunTests            = combine packageConfigRunTests,
      packageConfigDocumentation       = combine packageConfigDocumentation,
      packageConfigHaddockHoogle       = combine packageConfigHaddockHoogle,
      packageConfigHaddockHtml         = combine packageConfigHaddockHtml,
      packageConfigHaddockHtmlLocation = combine packageConfigHaddockHtmlLocation,
      packageConfigHaddockExecutables  = combine packageConfigHaddockExecutables,
      packageConfigHaddockTestSuites   = combine packageConfigHaddockTestSuites,
      packageConfigHaddockBenchmarks   = combine packageConfigHaddockBenchmarks,
      packageConfigHaddockInternal     = combine packageConfigHaddockInternal,
      packageConfigHaddockCss          = combine packageConfigHaddockCss,
      packageConfigHaddockHscolour     = combine packageConfigHaddockHscolour,
      packageConfigHaddockHscolourCss  = combine packageConfigHaddockHscolourCss,
      packageConfigHaddockContents     = combine packageConfigHaddockContents
    }
    where combine field = field a `mappend` field b


lookupLocalPackageConfig :: (Semigroup a, Monoid a)
                         => (PackageConfig -> a)
                         -> ProjectConfig
                         -> PackageName -> a
lookupLocalPackageConfig field ProjectConfig {
                           projectConfigLocalPackages,
                           projectConfigSpecificPackage
                         } pkgname =
    field projectConfigLocalPackages
 <> maybe mempty field (Map.lookup pkgname projectConfigSpecificPackage)

lookupNonLocalPackageConfig :: Monoid a
                            => (PackageConfig -> a)
                            -> ProjectConfig
                            -> PackageName -> a
lookupNonLocalPackageConfig field ProjectConfig {
                              projectConfigSpecificPackage
                            } pkgname =
    maybe mempty field (Map.lookup pkgname projectConfigSpecificPackage)


reportParseResult :: Verbosity -> String -> FilePath -> ParseResult a -> IO a
reportParseResult verbosity _filetype filename (ParseOk warnings x) = do
    unless (null warnings) $
      let msg = unlines (map (showPWarning filename) warnings)
       in warn verbosity msg
    return x
reportParseResult _verbosity filetype filename (ParseFailed err) =
    let (line, msg) = locatedErrorMsg err
     in die $ "Error parsing " ++ filetype ++ " " ++ filename
           ++ maybe "" (\n -> ':' : show n) line ++ ":\n" ++ msg


parseProjectConfig :: String -> ParseResult ProjectConfig
parseProjectConfig content =
    convertFromLegacyProjectConfig <$>
      parseLegacyProjectConfig content


convertFromLegacyProjectConfig :: LegacyProjectConfig -> ProjectConfig
convertFromLegacyProjectConfig
  (LegacyProjectConfig
    localPackageGlobs _repoPackages
    (LegacySharedConfig globalFlags configExFlags installFlags)
    (LegacyPackageConfig configFlags haddockFlags)
    specificConfig) =

    ProjectConfig {
      projectConfigPackageGlobs = localPackageGlobs,

      projectConfigBuildOnly       = configBuildOnly,
      projectConfigSolver          = configSolver,
      projectConfigAllPackages     = configAllPackages,
      projectConfigLocalPackages   = configLocalPackages,
      projectConfigSpecificPackage = fmap perPackage specificConfig
    }
  where
    configLocalPackages = convertLegacyPerPackageFlags
                            configFlags installFlags haddockFlags
    (configSolver,
     configAllPackages) = convertLegacyAllPackageFlags
                            globalFlags configFlags
                            configExFlags installFlags
    configBuildOnly     = convertLegacyBuildOnlyFlags
                            globalFlags configFlags
                            installFlags haddockFlags

    perPackage (LegacyPackageConfig perPkgConfigFlags perPkgHaddockFlags) =
      convertLegacyPerPackageFlags
        perPkgConfigFlags perPkgInstallFlags perPkgHaddockFlags
      where
        perPkgInstallFlags = mempty --TODO


commandLineFlagsToLegacyProjectConfig :: GlobalFlags
                                      -> ConfigFlags  -> ConfigExFlags
                                      -> InstallFlags -> HaddockFlags
                                      -> LegacyProjectConfig
commandLineFlagsToLegacyProjectConfig globalFlags
                                      configFlags configExFlags
                                      installFlags haddockFlags =
    LegacyProjectConfig {
      legacyLocalPackgeGlobs   = [],
      legacyRepoPackges        = [],
      legacySharedConfig = LegacySharedConfig {
        legacyGlobalFlags      = globalFlags,
        legacyConfigureExFlags = configExFlags,
        legacyInstallFlags     = installFlags
      },
      legacyLocalConfig = LegacyPackageConfig {
        legacyConfigureFlags   = configFlags,
        legacyHaddockFlags     = haddockFlags
      },
      legacySpecificConfig = Map.empty
    }

convertLegacyCommandLineFlags :: GlobalFlags
                              -> ConfigFlags  -> ConfigExFlags
                              -> InstallFlags -> HaddockFlags
                              -> ( ( ProjectConfigSolver
                                   , PackageConfigShared
                                   , PackageConfig
                                   )
                                 , ProjectConfigBuildOnly
                                 )
convertLegacyCommandLineFlags globalFlags configFlags configExFlags
                              installFlags haddockFlags =
    ( ( configSolver
      , configAllPackages
      , configLocalPackages
      )
    , configBuildOnly
    )
  where
    configLocalPackages = convertLegacyPerPackageFlags
                            configFlags installFlags haddockFlags
    (configSolver,
     configAllPackages) = convertLegacyAllPackageFlags
                            globalFlags configFlags
                            configExFlags installFlags
    configBuildOnly     = convertLegacyBuildOnlyFlags
                            globalFlags configFlags
                            installFlags haddockFlags


convertLegacyAllPackageFlags :: GlobalFlags -> ConfigFlags
                             -> ConfigExFlags -> InstallFlags
                             -> (ProjectConfigSolver, PackageConfigShared)
convertLegacyAllPackageFlags globalFlags configFlags configExFlags installFlags =
    ( ProjectConfigSolver{..}
    , PackageConfigShared{..}
    )
  where
    GlobalFlags {
      globalConfigFile        = _, -- TODO: [required feature]
      globalSandboxConfigFile = _, -- ??
      globalRemoteRepos       = projectConfigRemoteRepos,
      globalLocalRepos        = projectConfigLocalRepos
    } = globalFlags

    ConfigFlags {
      configProgramPaths        = packageConfigProgramPaths,
      configProgramArgs         = packageConfigProgramArgs,
      configProgramPathExtra    = packageConfigProgramPathExtra,
      configHcFlavor            = packageConfigHcFlavor,
      configHcPath              = packageConfigHcPath,
      configHcPkg               = packageConfigHcPkg,
      configVanillaLib          = packageConfigVanillaLib,
      configSharedLib           = packageConfigSharedLib,
      configInstallDirs         = packageConfigInstallDirs,
      configUserInstall         = packageConfigUserInstall,
      configPackageDBs          = packageConfigPackageDBs,
      configConfigurationsFlags = projectConfigConfigurationsFlags,
      configRelocatable         = packageConfigRelocatable
    } = configFlags

    ConfigExFlags {
      configCabalVersion        = projectConfigSolverCabalVersion,
      configExConstraints       = projectConfigSolverConstraints,
      configPreferences         = projectConfigSolverPreferences,
      configSolver              = projectConfigSolverSolver,
      configAllowNewer          = projectConfigSolverAllowNewer
    } = configExFlags

    InstallFlags {
      installHaddockIndex       = packageConfigHaddockIndex,
      installReinstall          = projectConfigSolverReinstall,
      installAvoidReinstalls    = projectConfigSolverAvoidReinstalls,
      installOverrideReinstall  = projectConfigSolverOverrideReinstall,
      installMaxBackjumps       = projectConfigSolverMaxBackjumps,
      installUpgradeDeps        = projectConfigSolverUpgradeDeps,
      installReorderGoals       = projectConfigSolverReorderGoals,
      installIndependentGoals   = projectConfigSolverIndependentGoals,
      installShadowPkgs         = projectConfigSolverShadowPkgs,
      installStrongFlags        = projectConfigSolverStrongFlags
    } = installFlags



convertLegacyPerPackageFlags :: ConfigFlags -> InstallFlags -> HaddockFlags
                             -> PackageConfig
convertLegacyPerPackageFlags configFlags installFlags haddockFlags =
    PackageConfig{..}
  where
    ConfigFlags {
      configVanillaLib          = _packageConfigVanillaLib, --TODO: hmm, not per pkg?
      configProfLib             = packageConfigProfLib,
      configSharedLib           = _packageConfigSharedLib, --TODO hmm, not per pkg?
      configDynExe              = packageConfigDynExe,
      configProfExe             = packageConfigProfExe,
      configProf                = packageConfigProf,
      configProfDetail          = packageConfigProfDetail,
      configProfLibDetail       = packageConfigProfLibDetail,
      configConfigureArgs       = packageConfigConfigureArgs,
      configOptimization        = packageConfigOptimization,
      configProgPrefix          = packageConfigProgPrefix,
      configProgSuffix          = packageConfigProgSuffix,
      configGHCiLib             = packageConfigGHCiLib,
      configSplitObjs           = packageConfigSplitObjs,
      configStripExes           = packageConfigStripExes,
      configStripLibs           = packageConfigStripLibs,
      configExtraLibDirs        = packageConfigExtraLibDirs,
      configExtraIncludeDirs    = packageConfigExtraIncludeDirs,
      configConfigurationsFlags = _projectConfigConfigurationsFlags, --TODO: should be per pkg
      configTests               = packageConfigTests,
      configBenchmarks          = packageConfigBenchmarks,
      configCoverage            = coverage,
      configLibCoverage         = libcoverage, --deprecated
      configDebugInfo           = packageConfigDebugInfo
    } = configFlags

    packageConfigCoverage       = coverage <> libcoverage

    InstallFlags {
      installDocumentation      = packageConfigDocumentation,
      installRunTests           = packageConfigRunTests
    } = installFlags

    HaddockFlags {
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
      haddockContents           = packageConfigHaddockContents
    } = haddockFlags


convertLegacyBuildOnlyFlags :: GlobalFlags -> ConfigFlags
                              -> InstallFlags -> HaddockFlags
                              -> ProjectConfigBuildOnly
convertLegacyBuildOnlyFlags globalFlags configFlags
                              installFlags haddockFlags =
    ProjectConfigBuildOnly{..}
  where
    GlobalFlags {
      globalCacheDir          = projectConfigCacheDir,
      globalLogsDir           = projectConfigLogsDir,
      globalWorldFile         = projectConfigWorldFile,
      globalHttpTransport     = projectConfigHttpTransport,
      globalIgnoreExpiry      = projectConfigIgnoreExpiry
    } = globalFlags

    ConfigFlags {
      configVerbosity           = projectConfigVerbosity
    } = configFlags

    InstallFlags {
      installDryRun             = projectConfigDryRun,
      installOnly               = _,
      installOnlyDeps           = projectConfigOnlyDeps,
      installRootCmd            = projectConfigRootCmd,
      installSummaryFile        = projectConfigSummaryFile,
      installLogFile            = projectConfigLogFile,
      installBuildReports       = projectConfigBuildReports,
      installReportPlanningFailure = projectConfigReportPlanningFailure,
      installSymlinkBinDir      = projectConfigSymlinkBinDir,
      installOneShot            = projectConfigOneShot,
      installNumJobs            = projectConfigNumJobs,
      installOfflineMode        = projectConfigOfflineMode
    } = installFlags

    HaddockFlags {
      haddockKeepTempFiles      = projectConfigKeepTempFiles --TODO: this ought to live elsewhere
    } = haddockFlags


-- | Use a 'RepoContext' based on the 'BuildTimeSettings'.
--
projectConfigWithBuilderRepoContext :: Verbosity
                                    -> BuildTimeSettings
                                    -> (RepoContext -> IO a) -> IO a
projectConfigWithBuilderRepoContext verbosity BuildTimeSettings{..} =
    withRepoContext'
      verbosity
      buildSettingRemoteRepos
      buildSettingLocalRepos
      buildSettingCacheDir
      buildSettingHttpTransport
      (Just buildSettingIgnoreExpiry)


-- | Use a 'RepoContext', but only for the solver. The solver does not use the
-- full facilities of the 'RepoContext' so we can get away with making one
-- that doesn't have an http transport. And that avoids having to have access
-- to the 'BuildTimeSettings'
--
projectConfigWithSolverRepoContext :: Verbosity
                                   -> FilePath
                                   -> ProjectConfigSolver
                                   -> ProjectConfigBuildOnly
                                   -> (RepoContext -> IO a) -> IO a
projectConfigWithSolverRepoContext verbosity downloadCacheRootDir
                                   ProjectConfigSolver{..}
                                   ProjectConfigBuildOnly{..} =
    withRepoContext'
      verbosity
      (fromNubList projectConfigRemoteRepos)
      (fromNubList projectConfigLocalRepos)
      downloadCacheRootDir
      (flagToMaybe projectConfigHttpTransport)
      (flagToMaybe projectConfigIgnoreExpiry)


data BuildTimeSettings
   = BuildTimeSettings {
       buildSettingDryRun                :: Bool,
       buildSettingOnlyDeps              :: Bool,
       buildSettingSummaryFile           :: [PathTemplate],
       buildSettingLogFile               :: Maybe (Compiler  -> Platform
                                                -> PackageId -> UnitId
                                                             -> FilePath),
       buildSettingLogVerbosity          :: Verbosity,
       buildSettingBuildReports          :: ReportLevel,
       buildSettingReportPlanningFailure :: Bool,
       buildSettingSymlinkBinDir         :: [FilePath],
       buildSettingOneShot               :: Bool,
       buildSettingNumJobs               :: Int,
       buildSettingOfflineMode           :: Bool,
       buildSettingKeepTempFiles         :: Bool,
       buildSettingRemoteRepos           :: [RemoteRepo],
       buildSettingLocalRepos            :: [FilePath],
       buildSettingCacheDir              :: FilePath,
       buildSettingHttpTransport         :: Maybe String,
       buildSettingIgnoreExpiry          :: Bool,
       buildSettingRootCmd               :: Maybe String
     }


resolveBuildTimeSettings :: Verbosity
                         -> CabalDirLayout
                         -> ProjectConfigSolver
                         -> ProjectConfigBuildOnly
                         -> ProjectConfigBuildOnly
                         -> BuildTimeSettings
resolveBuildTimeSettings verbosity
                         CabalDirLayout {
                           cabalLogsDirectory,
                           cabalPackageCacheDirectory
                         }
                         ProjectConfigSolver {
                           projectConfigRemoteRepos,
                           projectConfigLocalRepos
                         }
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
    buildSettingRemoteRepos   = fromNubList projectConfigRemoteRepos
    buildSettingLocalRepos    = fromNubList projectConfigLocalRepos
    buildSettingCacheDir      = cabalPackageCacheDirectory
    buildSettingHttpTransport = flagToMaybe projectConfigHttpTransport
    buildSettingIgnoreExpiry  = fromFlag    projectConfigIgnoreExpiry
    buildSettingReportPlanningFailure
                              = fromFlag projectConfigReportPlanningFailure
    buildSettingRootCmd       = flagToMaybe projectConfigRootCmd

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
      projectConfigKeepTempFiles         = toFlag False,
      projectConfigIgnoreExpiry          = toFlag False
    }

    -- The logging logic: what log file to use and what verbosity.
    --
    -- If the user has specified --remote-build-reporting=detailed, use the
    -- default log file location. If the --build-log option is set, use the
    -- provided location. Otherwise don't use logging, unless building in
    -- parallel (in which case the default location is used).
    --
    buildSettingLogFile :: Maybe (Compiler -> Platform
                               -> PackageId -> UnitId -> FilePath)
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
                     -> PackageId -> UnitId -> FilePath
    substLogFileName template compiler platform pkgid uid =
        fromPathTemplate (substPathTemplate env template)
      where
        env = initialPathTemplateEnv
                pkgid uid (compilerInfo compiler) platform

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


------------------------------------
-- Reading the project config file
--

-- But for the moment we use the old types and will convert later.

data LegacyProjectConfig = LegacyProjectConfig {
       -- local packages, including dirs, .cabal files and tarballs
       legacyLocalPackgeGlobs  :: [FilePathGlob],
       -- packages from source repos
       legacyRepoPackges       :: [SourceRepo], --TODO: [nice to have] use this

       legacySharedConfig      :: LegacySharedConfig,
       legacyLocalConfig       :: LegacyPackageConfig,
       legacySpecificConfig    :: Map PackageName LegacyPackageConfig
     }

instance Monoid LegacyProjectConfig where
  mempty =
    LegacyProjectConfig mempty mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup LegacyProjectConfig where
  a <> b =
    LegacyProjectConfig {
      legacyLocalPackgeGlobs   = combine legacyLocalPackgeGlobs,
      legacyRepoPackges        = combine legacyRepoPackges,
      legacySharedConfig       = combine legacySharedConfig,
      legacyLocalConfig        = combine legacyLocalConfig,
      legacySpecificConfig     = combine legacySpecificConfig
    }
    where combine field = field a `mappend` field b

data LegacyPackageConfig = LegacyPackageConfig {
       legacyConfigureFlags    :: ConfigFlags,
       legacyHaddockFlags      :: HaddockFlags
     }

instance Monoid LegacyPackageConfig where
  mempty =
    LegacyPackageConfig mempty mempty
  mappend = (<>)

instance Semigroup LegacyPackageConfig where
  a <> b =
    LegacyPackageConfig {
      legacyConfigureFlags     = combine legacyConfigureFlags,
      legacyHaddockFlags       = combine legacyHaddockFlags
    }
    where combine field = field a `mappend` field b

data LegacySharedConfig = LegacySharedConfig {
       legacyGlobalFlags       :: GlobalFlags,
       legacyConfigureExFlags  :: ConfigExFlags,
       legacyInstallFlags      :: InstallFlags
     }

instance Monoid LegacySharedConfig where
  mempty =
    LegacySharedConfig mempty mempty mempty
  mappend = (<>)

instance Semigroup LegacySharedConfig where
  a <> b =
    LegacySharedConfig {
      legacyGlobalFlags        = combine legacyGlobalFlags,
      legacyConfigureExFlags   = combine legacyConfigureExFlags,
      legacyInstallFlags       = combine legacyInstallFlags
    }
    where combine field = field a `mappend` field b

parseLegacyProjectConfig :: String -> ParseResult LegacyProjectConfig
parseLegacyProjectConfig =
    parseConfig legacyProjectConfigFieldDescrs
                legacyPackageConfigSectionDescrs

showLegacyProjectConfig :: LegacyProjectConfig -> String
showLegacyProjectConfig config =
    Disp.render $
    showConfig  legacyProjectConfigFieldDescrs
                legacyPackageConfigSectionDescrs
                config
  $+$
    Disp.text ""


legacyProjectConfigFieldDescrs :: [FieldDescr LegacyProjectConfig]
legacyProjectConfigFieldDescrs =

    [ commaListField "packages"  --TODO: [code cleanup] ought to be listField, but ReadP introduces ambiguity
        disp parse
        legacyLocalPackgeGlobs
        (\v flags -> flags { legacyLocalPackgeGlobs = v })
    ]

 ++ map (liftField
           legacySharedConfig
           (\flags conf -> conf { legacySharedConfig = flags }))
        legacySharedConfigFieldDescrs

 ++ map (liftField
           legacyLocalConfig
           (\flags conf -> conf { legacyLocalConfig = flags }))
        legacyPackageConfigFieldDescrs


legacySharedConfigFieldDescrs :: [FieldDescr LegacySharedConfig]
legacySharedConfigFieldDescrs =

  ( liftFields
      legacyGlobalFlags
      (\flags conf -> conf { legacyGlobalFlags = flags })
  . filterFields
      [ "remote-repo", "remote-repo-cache", "local-repo"
      , "logs-dir", "world-file", "http-transport"
      ]
  . commandOptionsToFields
  ) (commandOptions (globalCommand []) ParseArgs)
 ++
  ( liftFields
      legacyConfigureExFlags
      (\flags conf -> conf { legacyConfigureExFlags = flags })
  . commandOptionsToFields
  ) (configureExOptions ParseArgs (ConstraintSourceProjectConfig "TODO"))
 ++
  ( liftFields
      legacyInstallFlags
      (\flags conf -> conf { legacyInstallFlags = flags })
  . filterFields
      [ "documentation", "doc-index-file"
      , "root-cmd", "symlink-bindir"
      , "build-summary", "build-log"
      , "remote-build-reporting", "report-planning-failure"
      , "run-tests", "offline"
        -- solver flags:
      , "max-backjumps", "reorder-goals", "strong-flags"
      ]
  . commandOptionsToFields
  ) (installOptions ParseArgs)


legacyPackageConfigFieldDescrs :: [FieldDescr LegacyPackageConfig]
legacyPackageConfigFieldDescrs =
  ( liftFields
      legacyConfigureFlags
      (\flags conf -> conf { legacyConfigureFlags = flags })
  . filterFields
      [ "compiler", "with-compiler", "with-hc-pkg"
      , "program-prefix", "program-suffix"
      , "library-vanilla", "library-profiling"
      , "shared", "executable-dynamic"
      , "profiling", "executable-profiling"
      , "profiling-detail", "library-profiling-detail"
      , "optimization", "debug-info", "library-for-ghci", "split-objs"
      , "executable-stripping", "library-stripping"
      , "configure-option", "flags"
      , "extra-include-dirs", "extra-lib-dirs", "extra-prog-path"
      , "tests", "benchmarks"
      , "coverage", "library-coverage"
      ]
  . commandOptionsToFields
  ) (configureOptions ParseArgs)
 ++
    liftFields
      legacyConfigureFlags
      (\flags conf -> conf { legacyConfigureFlags = flags })
    [ overrideFieldCompiler
    , overrideFieldOptimization
    , overrideFieldDebugInfo
    ]
 ++
  ( liftFields
      legacyHaddockFlags
      (\flags conf -> conf { legacyHaddockFlags = flags })
  . mapFieldNames
      ("haddock-"++)
  . filterFields
      [ "hoogle", "html", "html-location"
      , "executables", "tests", "benchmarks", "all", "internal", "css"
      , "hyperlink-source", "hscolour-css"
      , "contents-location"
      ]
  . commandOptionsToFields
  ) (haddockOptions ParseArgs)

  where
    overrideFieldCompiler =
      simpleField "compiler"
        (fromFlagOrDefault Disp.empty . fmap disp)
        (Parse.option mempty (fmap toFlag parse))
        configHcFlavor (\v flags -> flags { configHcFlavor = v })


    -- TODO: [code cleanup] The following is a hack. The "optimization" and
    -- "debug-info" fields are OptArg, and viewAsFieldDescr fails on that.
    -- Instead of a hand-written parser and printer, we should handle this case
    -- properly in the library.

    overrideFieldOptimization =
      liftField configOptimization
                (\v flags -> flags { configOptimization = v }) $
      let name = "optimization" in
      FieldDescr name
        (\f -> case f of
                 Flag NoOptimisation      -> Disp.text "False"
                 Flag NormalOptimisation  -> Disp.text "True"
                 Flag MaximumOptimisation -> Disp.text "2"
                 _                        -> Disp.empty)
        (\line str _ -> case () of
         _ |  str == "False" -> ParseOk [] (Flag NoOptimisation)
           |  str == "True"  -> ParseOk [] (Flag NormalOptimisation)
           |  str == "0"     -> ParseOk [] (Flag NoOptimisation)
           |  str == "1"     -> ParseOk [] (Flag NormalOptimisation)
           |  str == "2"     -> ParseOk [] (Flag MaximumOptimisation)
           | lstr == "false" -> ParseOk [caseWarning] (Flag NoOptimisation)
           | lstr == "true"  -> ParseOk [caseWarning] (Flag NormalOptimisation)
           | otherwise       -> ParseFailed (NoParse name line)
           where
             lstr = lowercase str
             caseWarning = PWarning $
               "The '" ++ name ++ "' field is case sensitive, use 'True' or 'False'.")

    overrideFieldDebugInfo =
      liftField configDebugInfo (\v flags -> flags { configDebugInfo = v }) $
      let name = "debug-info" in
      FieldDescr name
        (\f -> case f of
                 Flag NoDebugInfo      -> Disp.text "False"
                 Flag MinimalDebugInfo -> Disp.text "1"
                 Flag NormalDebugInfo  -> Disp.text "True"
                 Flag MaximalDebugInfo -> Disp.text "3"
                 _                     -> Disp.empty)
        (\line str _ -> case () of
         _ |  str == "False" -> ParseOk [] (Flag NoDebugInfo)
           |  str == "True"  -> ParseOk [] (Flag NormalDebugInfo)
           |  str == "0"     -> ParseOk [] (Flag NoDebugInfo)
           |  str == "1"     -> ParseOk [] (Flag MinimalDebugInfo)
           |  str == "2"     -> ParseOk [] (Flag NormalDebugInfo)
           |  str == "3"     -> ParseOk [] (Flag MaximalDebugInfo)
           | lstr == "false" -> ParseOk [caseWarning] (Flag NoDebugInfo)
           | lstr == "true"  -> ParseOk [caseWarning] (Flag NormalDebugInfo)
           | otherwise       -> ParseFailed (NoParse name line)
           where
             lstr = lowercase str
             caseWarning = PWarning $
               "The '" ++ name ++ "' field is case sensitive, use 'True' or 'False'.")


legacyPackageConfigSectionDescrs :: [SectionDescr LegacyProjectConfig]
legacyPackageConfigSectionDescrs =
    [ liftSection
        legacyLocalConfig
        (\flags conf -> conf { legacyLocalConfig = flags })
        programOptionsSectionDescr
    , liftSection
        legacyLocalConfig
        (\flags conf -> conf { legacyLocalConfig = flags })
        programLocationsSectionDescr
    , programSpecificOptions
    ]

programSpecificOptions :: SectionDescr LegacyProjectConfig
programSpecificOptions =
    SectionDescr {
      sectionName        = "package",
      sectionFields      = legacyPackageConfigFieldDescrs
                        ++ liftFields
                             (configProgramArgs . legacyConfigureFlags)
                             (\args pkgconf -> pkgconf {
                                 legacyConfigureFlags = (legacyConfigureFlags pkgconf) {
                                   configProgramArgs  = args
                                 }
                               }
                             )
                             programOptionsFieldDescrs,
      sectionSubsections = [],
      sectionGet         = \projconf ->
                             [ (display pkgname, pkgconf)
                             | (pkgname, pkgconf) <- Map.toList (legacySpecificConfig projconf) ],
      sectionSet         =
        \_lineno pkgname pkgconf projconf ->
          return projconf {
            legacySpecificConfig =
              Map.insertWith mappend (PackageName pkgname) pkgconf
                             (legacySpecificConfig projconf)
          }
    }

programOptionsFieldDescrs :: [FieldDescr [(String, [String])]]
programOptionsFieldDescrs =
    commandOptionsToFields
  $ programConfigurationOptions
      defaultProgramDb
      ParseArgs id (++)

programOptionsSectionDescr :: SectionDescr LegacyPackageConfig
programOptionsSectionDescr =
    SectionDescr {
      sectionName        = "program-options",
      sectionFields      = programOptionsFieldDescrs,
      sectionSubsections = [],
      sectionGet         = (\x->[("", x)])
                         . configProgramArgs
                         . legacyConfigureFlags,
      sectionSet         =
        \lineno unused args pkgconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'program-options' takes no arguments"
          return pkgconf {
              legacyConfigureFlags = (legacyConfigureFlags pkgconf) {
                configProgramArgs = args
            }
          }
    }

programLocationsFieldDescrs :: [FieldDescr ConfigFlags]
programLocationsFieldDescrs =
    liftFields
      configProgramPaths
      (\paths conf -> conf { configProgramPaths = paths })
   . commandOptionsToFields
   $ programConfigurationPaths'
       (++ "-location")
       defaultProgramDb
       ParseArgs id (++)

programLocationsSectionDescr :: SectionDescr LegacyPackageConfig
programLocationsSectionDescr =
    SectionDescr {
      sectionName        = "program-locations",
      sectionFields      = programLocationsFieldDescrs,
      sectionSubsections = [],
      sectionGet         = (\x->[("", x)])
                         . legacyConfigureFlags,
      sectionSet         =
        \lineno unused confflags pkgconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'program-locations' takes no arguments"
          return pkgconf { legacyConfigureFlags = confflags }
    }


-- | For each known program @PROG@ in 'progConf', produce a @PROG-options@
-- 'OptionField'.
programConfigurationOptions
  :: ProgramDb
  -> ShowOrParseArgs
  -> (flags -> [(String, [String])])
  -> ([(String, [String])] -> (flags -> flags))
  -> [OptionField flags]
programConfigurationOptions progConf showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [programOptions  "PROG"]
    ParseArgs -> map (programOptions . programName . fst)
                 (knownPrograms progConf)
  where
    programOptions prog =
      option "" [prog ++ "-options"]
        ("give extra options to " ++ prog)
        get set
        (reqArg' "OPTS" (\args -> [(prog, splitArgs args)])
           (\progArgs -> [ joinsArgs args
                         | (prog', args) <- progArgs, prog==prog' ]))


    joinsArgs = unwords . map escape
    escape arg | any isSpace arg = "\"" ++ arg ++ "\""
               | otherwise       = arg


liftFields :: (b -> a)
           -> (a -> b -> b)
           -> [FieldDescr a]
           -> [FieldDescr b]
liftFields get set = map (liftField get set)


filterFields :: [String] -> [FieldDescr a] -> [FieldDescr a]
filterFields includeFields = filter ((`elem` includeFields) . fieldName)

mapFieldNames :: (String -> String) -> [FieldDescr a] -> [FieldDescr a]
mapFieldNames mangleName =
    map (\descr -> descr { fieldName = mangleName (fieldName descr) })

commandOptionsToFields :: [OptionField a] -> [FieldDescr a]
commandOptionsToFields = map viewAsFieldDescr


data SectionDescr a = forall b. Monoid b => SectionDescr {
       sectionName        :: String,
       sectionFields      :: [FieldDescr b],
       sectionSubsections :: [SectionDescr b],
       sectionGet         :: a -> [(String, b)],
       sectionSet         :: LineNo -> String -> b -> a -> ParseResult a
     }

liftSection :: (b -> a)
            -> (a -> b -> b)
            -> SectionDescr a
            -> SectionDescr b
liftSection get' set' (SectionDescr name fields sections get set) =
    let sectionGet' = get . get'
        sectionSet' lineno param x y = do x' <- set lineno param x (get' y)
                                          return (set' x' y)

     in SectionDescr name fields sections sectionGet' sectionSet'

parseConfig :: Monoid a => [FieldDescr a] -> [SectionDescr a]
            -> String -> ParseResult a
parseConfig fields sections str =
    accumFieldsAndSections fields sections =<< readFieldsFlat str


accumFieldsAndSections :: Monoid a => [FieldDescr a] -> [SectionDescr a]
              -> [Field] -> ParseResult a
accumFieldsAndSections fieldDescrs sectionDescrs =
    foldM accumField mempty
  where
    fieldMap   = Map.fromList [ (fieldName   f, f) | f <- fieldDescrs   ]
    sectionMap = Map.fromList [ (sectionName s, s) | s <- sectionDescrs ]

    accumField a (F line name value) =
      case Map.lookup name fieldMap of
        Just (FieldDescr _ _ set) -> set line value a
        Nothing -> do
          warning $ "Unrecognized field '" ++ name
                 ++ "' on line " ++ show line
          return a

    accumField a (Section line name param fields) =
      case Map.lookup name sectionMap of
        Just (SectionDescr _ fieldDescrs' sectionDescrs' _ set) -> do
          b <- accumFieldsAndSections fieldDescrs' sectionDescrs' fields
          set line param b a
        Nothing -> do
          warning $ "Unrecognized section '" ++ name
                 ++ "' on line " ++ show line
          return a

    accumField _ (IfBlock {}) = error "accumFieldsAndSections: impossible"

showConfig :: [FieldDescr a] -> [SectionDescr a] -> a -> Disp.Doc
showConfig fields sections val =
      ppFields fields Nothing val
        $+$
      Disp.vcat
      [ Disp.text "" $+$ sectionDoc
      | SectionDescr {
          sectionName, sectionGet,
          sectionFields, sectionSubsections
        } <- sections
      , (param, x) <- sectionGet val
      , let sectionDoc = ppSection' sectionName param sectionFields sectionSubsections x
      , not (Disp.isEmpty sectionDoc) ]

ppSection' :: String -> String -> [FieldDescr a] -> [SectionDescr a] -> a -> Disp.Doc
ppSection' name arg fields sections cur
  | Disp.isEmpty fieldsDoc = Disp.empty
  | otherwise              = Disp.text name <+> argDoc
                             $+$ (Disp.nest 2 fieldsDoc)
  where
    fieldsDoc = showConfig fields sections cur
    argDoc | arg == "" = Disp.empty
           | otherwise = Disp.text arg
