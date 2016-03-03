{-# LANGUAGE RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable,
             ExistentialQuantification, ScopedTypeVariables #-}

-- | Handling project configuration, types and reading.
--
module Distribution.Client.ProjectConfig (

    -- * Types for project config
    ProjectConfig(..),
    ProjectConfigBuildOnly(..),
    ProjectConfigShared(..),
    PackageConfig(..),

    -- * Project config files
    findProjectRoot,
    readProjectConfig,
    writeProjectLocalExtraConfig,
    writeProjectConfigFile,
    commandLineFlagsToProjectConfig,

    -- * Packages within projects
    ProjectPackageLocation(..),
    BadPackageLocation(..),
    BadPackageLocationMatch(..),
    findProjectPackages,
    readSourcePackage,

    -- * Resolving configuration
    lookupLocalPackageConfig,
    projectConfigWithBuilderRepoContext,
    projectConfigWithSolverRepoContext,
    SolverSettings(..),
    resolveSolverSettings,
    BuildTimeSettings(..),
    resolveBuildTimeSettings,
  ) where

import Distribution.Client.RebuildMonad
import Distribution.Client.FileMonitor (isTrivialFilePathGlob)

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
import Distribution.PackageDescription
         ( FlagAssignment, SourceRepo(..), RepoKind(..) )
import Distribution.PackageDescription.Parse
         ( readPackageDescription, sourceRepoFieldDescrs )
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
         , InstallFlags(..), installOptions, defaultInstallFlags
         , defaultSolver, defaultMaxBackjumps )
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
import Distribution.Compat.ReadP
         ( ReadP, (+++) )
import qualified Text.PrettyPrint as Disp
         ( render, text, empty, sep )
import Text.PrettyPrint
         ( Doc, ($+$) )

import qualified Distribution.ParseUtils as ParseUtils (field)
import Distribution.ParseUtils
         ( ParseResult(..), PError(..), syntaxError, locatedErrorMsg
         , PWarning(..), showPWarning
         , simpleField, commaNewLineListField )
import Distribution.Client.ParseUtils
import Distribution.Simple.Command
         ( CommandUI(commandOptions), ShowOrParseArgs(..)
         , OptionField, option, reqArg' )

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Exception
import Data.Typeable
import Data.Maybe
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isSpace)
import Distribution.Compat.Binary (Binary)
import Distribution.Compat.Semigroup
import GHC.Generics (Generic)
import System.FilePath hiding (combine)
import System.Directory
import Network.URI (URI(..), URIAuth(..), parseAbsoluteURI)


-------------------------------
-- Project config types
--

-- | This type corresponds directly to what can be written in the
-- @cabal.project@ file. Other sources of configuration can also be injected
-- into this type, such as the user-wide @~/.cabal/config@ file and the
-- command line of @cabal configure@ or @cabal build@.
--
-- Since it corresponds to the external project file it is an instance of
-- 'Monoid' and all the fields can be empty. This also means there has to
-- be a step where we resolve configuration. At a minimum resolving means
-- applying defaults but it can also mean merging information from multiple
-- sources. For example for package-specific configuration the project file
-- can specify configuration that applies to all local packages, and then
-- additional configuration for a specific package.
--
-- Future directions: multiple profiles, conditionals. If we add these
-- features then the gap between configuration as written in the config file
-- and resolved settings we actually use will become even bigger.
--
data ProjectConfig
   = ProjectConfig {

       -- | Packages in this project, including local dirs, local .cabal files
       -- local and remote tarballs. Where these are file globs, they must
       -- match something.
       projectPackages              :: [String],

       -- | Like 'projectConfigPackageGlobs' but /optional/ in the sense that
       -- file globs are allowed to match nothing. The primary use case for
       -- this is to be able to say @optional-packages: */@ to automagically
       -- pick up deps that we unpack locally.
       projectPackagesOptional      :: [String],

       -- | Packages in this project from remote source repositories.
       projectPackagesRepo          :: [SourceRepo],

       -- | Packages in this project from hackage repositories.
       projectPackagesNamed         :: [Dependency],

       projectConfigBuildOnly       :: ProjectConfigBuildOnly,
       projectConfigShared          :: ProjectConfigShared,
       projectConfigLocalPackages   :: PackageConfig,
       projectConfigSpecificPackage :: Map PackageName PackageConfig
     }
  deriving (Eq, Show, Generic)

-- | That part of the project configuration that only affects /how/ we build
-- and not the /value/ of the things we build. This means this information
-- does not need to be tracked for changes since it does not affect the
-- outcome.
--
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


-- | Project configuration that is shared between all packages in the project.
-- In particular this includes configuration that affects the solver.
--
data ProjectConfigShared
   = ProjectConfigShared {
       projectConfigProgramPaths      :: [(String, FilePath)],
       projectConfigProgramArgs       :: [(String, [String])],
       projectConfigProgramPathExtra  :: NubList FilePath,
       projectConfigHcFlavor          :: Flag CompilerFlavor,
       projectConfigHcPath            :: Flag FilePath,
       projectConfigHcPkg             :: Flag FilePath,
       projectConfigVanillaLib        :: Flag Bool,
       projectConfigSharedLib         :: Flag Bool,
       projectConfigHaddockIndex      :: Flag PathTemplate,

       -- Things that only make sense for manual mode, not --local mode
       -- too much control!
       projectConfigUserInstall       :: Flag Bool,
       projectConfigInstallDirs       :: InstallDirs (Flag PathTemplate),
       projectConfigPackageDBs        :: [Maybe PackageDB],
       projectConfigRelocatable       :: Flag Bool,

       -- configuration used both by the solver and other phases
       projectConfigRemoteRepos       :: NubList RemoteRepo,     -- ^ Available Hackage servers.
       projectConfigLocalRepos        :: NubList FilePath,

       -- solver configuration
       projectConfigConstraints       :: [(UserConstraint, ConstraintSource)],
       projectConfigPreferences       :: [Dependency],
       projectConfigFlagAssignment    :: FlagAssignment, --TODO: [required eventually] must be per-package, not global
       projectConfigCabalVersion      :: Flag Version,  --TODO: [required eventually] unused
       projectConfigSolver            :: Flag PreSolver,
       projectConfigAllowNewer        :: Flag AllowNewer,
       projectConfigMaxBackjumps      :: Flag Int,
       projectConfigReorderGoals      :: Flag Bool,
       projectConfigStrongFlags       :: Flag Bool,

       -- More things that only make sense for manual mode, not --local mode
       -- too much control!
       projectConfigIndependentGoals  :: Flag Bool,
       projectConfigShadowPkgs        :: Flag Bool,
       projectConfigReinstall         :: Flag Bool,
       projectConfigAvoidReinstalls   :: Flag Bool,
       projectConfigOverrideReinstall :: Flag Bool,
       projectConfigUpgradeDeps       :: Flag Bool
     }
  deriving (Eq, Show, Generic)


-- | Project configuration that is specific to each package, that is where we
-- can in principle have different values for different packages in the same
-- project.
--
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
instance Binary ProjectConfigBuildOnly
instance Binary ProjectConfigShared
instance Binary PackageConfig


instance Monoid ProjectConfig where
  mempty =
    ProjectConfig {
      projectPackages              = mempty,
      projectPackagesOptional      = mempty,
      projectPackagesRepo          = mempty,
      projectPackagesNamed         = mempty,
      projectConfigBuildOnly       = mempty,
      projectConfigShared          = mempty,
      projectConfigLocalPackages   = mempty,
      projectConfigSpecificPackage = mempty
    }
  mappend = (<>)

instance Semigroup ProjectConfig where
  a <> b =
    ProjectConfig {
      projectPackages              = combine projectPackages,
      projectPackagesOptional      = combine projectPackagesOptional,
      projectPackagesRepo          = combine projectPackagesRepo,
      projectPackagesNamed         = combine projectPackagesNamed,
      projectConfigBuildOnly       = combine projectConfigBuildOnly,
      projectConfigShared          = combine projectConfigShared,
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


instance Monoid ProjectConfigShared where
  mempty =
    ProjectConfigShared {
      projectConfigProgramPaths     = mempty,
      projectConfigProgramArgs      = mempty,
      projectConfigProgramPathExtra = mempty,
      projectConfigHcFlavor         = mempty,
      projectConfigHcPath           = mempty,
      projectConfigHcPkg            = mempty,
      projectConfigVanillaLib       = mempty,
      projectConfigSharedLib        = mempty,
      projectConfigHaddockIndex     = mempty,
      projectConfigUserInstall      = mempty,
      projectConfigInstallDirs      = mempty,
      projectConfigPackageDBs       = mempty,
      projectConfigRelocatable      = mempty,

      projectConfigConstraints       = mempty,
      projectConfigPreferences       = mempty,
      projectConfigFlagAssignment    = mempty,
      projectConfigCabalVersion      = mempty,
      projectConfigSolver            = mempty,
      projectConfigAllowNewer        = mempty,
      projectConfigRemoteRepos       = mempty,
      projectConfigLocalRepos        = mempty,
      projectConfigMaxBackjumps      = mempty,
      projectConfigReorderGoals      = mempty,
      projectConfigStrongFlags       = mempty,
      projectConfigIndependentGoals  = mempty,
      projectConfigShadowPkgs        = mempty,
      projectConfigReinstall         = mempty,
      projectConfigAvoidReinstalls   = mempty,
      projectConfigOverrideReinstall = mempty,
      projectConfigUpgradeDeps       = mempty
    }
  mappend = (<>)

instance Semigroup ProjectConfigShared where
  a <> b =
    ProjectConfigShared {
      projectConfigProgramPaths     = combine projectConfigProgramPaths,
      projectConfigProgramArgs      = combine projectConfigProgramArgs,
      projectConfigProgramPathExtra = combine projectConfigProgramPathExtra,
      projectConfigHcFlavor         = combine projectConfigHcFlavor,
      projectConfigHcPath           = combine projectConfigHcPath,
      projectConfigHcPkg            = combine projectConfigHcPkg,
      projectConfigVanillaLib       = combine projectConfigVanillaLib,
      projectConfigSharedLib        = combine projectConfigSharedLib,
      projectConfigHaddockIndex     = combine projectConfigHaddockIndex,
      projectConfigUserInstall      = combine projectConfigUserInstall,
      projectConfigInstallDirs      = combine projectConfigInstallDirs,
      projectConfigPackageDBs       = combine projectConfigPackageDBs,
      projectConfigRelocatable      = combine projectConfigRelocatable,

      projectConfigConstraints       = combine projectConfigConstraints,
      projectConfigPreferences       = combine projectConfigPreferences,
      projectConfigFlagAssignment    = combine projectConfigFlagAssignment,
      projectConfigCabalVersion      = combine projectConfigCabalVersion,
      projectConfigSolver            = combine projectConfigSolver,
      projectConfigAllowNewer        = combine projectConfigAllowNewer,
      projectConfigRemoteRepos       = combine projectConfigRemoteRepos,
      projectConfigLocalRepos        = combine projectConfigLocalRepos,
      projectConfigMaxBackjumps      = combine projectConfigMaxBackjumps,
      projectConfigReorderGoals      = combine projectConfigReorderGoals,
      projectConfigStrongFlags       = combine projectConfigStrongFlags,
      projectConfigIndependentGoals  = combine projectConfigIndependentGoals,
      projectConfigShadowPkgs        = combine projectConfigShadowPkgs,
      projectConfigReinstall         = combine projectConfigReinstall,
      projectConfigAvoidReinstalls   = combine projectConfigAvoidReinstalls,
      projectConfigOverrideReinstall = combine projectConfigOverrideReinstall,
      projectConfigUpgradeDeps       = combine projectConfigUpgradeDeps
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
  a <> b =
    PackageConfig {
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


----------------------------------------
-- Resolving configuration to settings
--

-- | Look up a 'PackageConfig' field in the 'ProjectConfig' for a specific
-- 'PackageName'. This returns the configuration that applies to all local
-- packages plus any package-specific configuration for this package.
--
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
                                   -> ProjectConfigShared
                                   -> ProjectConfigBuildOnly
                                   -> (RepoContext -> IO a) -> IO a
projectConfigWithSolverRepoContext verbosity downloadCacheRootDir
                                   ProjectConfigShared{..}
                                   ProjectConfigBuildOnly{..} =
    withRepoContext'
      verbosity
      (fromNubList projectConfigRemoteRepos)
      (fromNubList projectConfigLocalRepos)
      downloadCacheRootDir
      (flagToMaybe projectConfigHttpTransport)
      (flagToMaybe projectConfigIgnoreExpiry)


-- | Resolved configuration for the solver. The idea is that this is easier to
-- use than the raw configuration because in the raw configuration everything
-- is optional (monoidial). In the 'BuildTimeSettings' every field is filled
-- in, if only with the defaults.
--
-- Use 'resolveSolverSettings' to make one from the project config (by
-- applying defaults etc).
--
data SolverSettings
   = SolverSettings {
       solverSettingRemoteRepos       :: [RemoteRepo],     -- ^ Available Hackage servers.
       solverSettingLocalRepos        :: [FilePath],
       solverSettingConstraints       :: [(UserConstraint, ConstraintSource)],
       solverSettingPreferences       :: [Dependency],
       solverSettingFlagAssignment    :: FlagAssignment, --TODO: [required eventually] must be per-package, not global
       solverSettingCabalVersion      :: Maybe Version,  --TODO: [required eventually] unused
       solverSettingSolver            :: PreSolver,
       solverSettingAllowNewer        :: AllowNewer,
       solverSettingMaxBackjumps      :: Maybe Int,
       solverSettingReorderGoals      :: Bool,
       solverSettingStrongFlags       :: Bool,
       -- Things that only make sense for manual mode, not --local mode
       -- too much control!
       solverSettingIndependentGoals  :: Bool,
       solverSettingShadowPkgs        :: Bool,
       solverSettingReinstall         :: Bool,
       solverSettingAvoidReinstalls   :: Bool,
       solverSettingOverrideReinstall :: Bool,
       solverSettingUpgradeDeps       :: Bool
     }
  deriving (Eq, Show, Generic)

instance Binary SolverSettings


-- | Resolve the project configuration, with all its optional fields, into
-- 'SolverSettings' with no optional fields (by applying defaults).
--
resolveSolverSettings :: ProjectConfigShared -> SolverSettings
resolveSolverSettings projectConfig =
    SolverSettings {..}
  where
    solverSettingRemoteRepos       = fromNubList projectConfigRemoteRepos
    solverSettingLocalRepos        = fromNubList projectConfigLocalRepos
    solverSettingConstraints       = projectConfigConstraints
    solverSettingPreferences       = projectConfigPreferences
    solverSettingFlagAssignment    = projectConfigFlagAssignment
    solverSettingCabalVersion      = flagToMaybe projectConfigCabalVersion
    solverSettingSolver            = fromFlag projectConfigSolver
    solverSettingAllowNewer        = fromFlag projectConfigAllowNewer
    solverSettingMaxBackjumps      = case fromFlag projectConfigMaxBackjumps of
                                       n | n < 0     -> Nothing
                                         | otherwise -> Just n
    solverSettingReorderGoals      = fromFlag projectConfigReorderGoals
    solverSettingStrongFlags       = fromFlag projectConfigStrongFlags
    solverSettingIndependentGoals  = fromFlag projectConfigIndependentGoals
    solverSettingShadowPkgs        = fromFlag projectConfigShadowPkgs
    solverSettingReinstall         = fromFlag projectConfigReinstall
    solverSettingAvoidReinstalls   = fromFlag projectConfigAvoidReinstalls
    solverSettingOverrideReinstall = fromFlag projectConfigOverrideReinstall
    solverSettingUpgradeDeps       = fromFlag projectConfigUpgradeDeps

    ProjectConfigShared {..} = defaults <> projectConfig

    defaults = mempty {
       projectConfigSolver            = Flag defaultSolver,
       projectConfigAllowNewer        = Flag AllowNewerNone,
       projectConfigMaxBackjumps      = Flag defaultMaxBackjumps,
       projectConfigReorderGoals      = Flag False,
       projectConfigStrongFlags       = Flag False,
       projectConfigIndependentGoals  = Flag False,
       projectConfigShadowPkgs        = Flag False,
       projectConfigReinstall         = Flag False,
       projectConfigAvoidReinstalls   = Flag False,
       projectConfigOverrideReinstall = Flag False,
       projectConfigUpgradeDeps       = Flag False
    }


-- | Resolved configuration for things that affect how we build and not the
-- value of the things we build. The idea is that this is easier to use than
-- the raw configuration because in the raw configuration everything is
-- optional (monoidial). In the 'BuildTimeSettings' every field is filled in,
-- if only with the defaults.
--
-- Use 'resolveBuildTimeSettings' to make one from the project config (by
-- applying defaults etc).
--
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


-- | Resolve the project configuration, with all its optional fields, into
-- 'BuildTimeSettings' with no optional fields (by applying defaults).
--
resolveBuildTimeSettings :: Verbosity
                         -> CabalDirLayout
                         -> ProjectConfigShared
                         -> ProjectConfigBuildOnly
                         -> ProjectConfigBuildOnly
                         -> BuildTimeSettings
resolveBuildTimeSettings verbosity
                         CabalDirLayout {
                           cabalLogsDirectory,
                           cabalPackageCacheDirectory
                         }
                         ProjectConfigShared {
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
                              <> fromProjectFile
                              <> fromCommandLine

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


---------------------------------------------
-- Reading and writing project config files
--

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
readProjectConfig :: Verbosity -> FilePath -> Rebuild ProjectConfig
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
      mempty {
        -- We expect a package in the current directory.
        projectPackages         = [ "./*.cabal" ],

        -- This is to automatically pick up deps that we unpack locally.
        projectPackagesOptional = [ "./*/*.cabal" ]
      }


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


-- | Parse the 'ProjectConfig' format.
--
-- For the moment this is implemented in terms of parsers for legacy
-- configuration types, plus a conversion.
--
parseProjectConfig :: String -> ParseResult ProjectConfig
parseProjectConfig content =
    convertLegacyProjectConfig <$>
      parseLegacyProjectConfig content


-- | Render the 'ProjectConfig' format.
--
-- For the moment this is implemented in terms of a pretty printer for the
-- legacy configuration types, plus a conversion.
--
showProjectConfig :: ProjectConfig -> String
showProjectConfig =
    showLegacyProjectConfig . convertToLegacyProjectConfig


-- | Write a @cabal.project.extra@ file in the given project root dir.
--
writeProjectLocalExtraConfig :: FilePath -> ProjectConfig -> IO ()
writeProjectLocalExtraConfig projectRootDir =
    writeProjectConfigFile projectExtraConfigFile
  where
    projectExtraConfigFile = projectRootDir </> "cabal.project.extra"


-- | Write in the @cabal.project@ format to the given file.
--
writeProjectConfigFile :: FilePath -> ProjectConfig -> IO ()
writeProjectConfigFile file =
    writeFile file . showProjectConfig


-- | Read the user's @~/.cabal/config@ file.
--
readGlobalConfig :: Verbosity -> Rebuild ProjectConfig
readGlobalConfig verbosity = do
    config     <- liftIO (loadConfig verbosity mempty)
    configFile <- liftIO defaultConfigFile
    monitorFiles [MonitorFileHashed configFile]
    return (convertLegacyGlobalConfig config)
    --TODO: do this properly, there's several possible locations
    -- and env vars, and flags for selecting the global config


-- | Convert configuration from the @cabal configure@ or @cabal build@ command
-- line into a 'ProjectConfig' value that can combined with configuration from
-- other sources.
--
-- At the moment this uses the legacy command line flag types. See
-- 'LegacyProjectConfig' for an explanation.
--
commandLineFlagsToProjectConfig :: GlobalFlags
                                -> ConfigFlags  -> ConfigExFlags
                                -> InstallFlags -> HaddockFlags
                                -> ProjectConfig
commandLineFlagsToProjectConfig globalFlags configFlags configExFlags
                                installFlags haddockFlags =
    mempty {
      projectConfigBuildOnly     = convertLegacyBuildOnlyFlags
                                     globalFlags configFlags
                                     installFlags haddockFlags,
      projectConfigShared        = convertLegacyAllPackageFlags
                                     globalFlags configFlags
                                     configExFlags installFlags,
      projectConfigLocalPackages = convertLegacyPerPackageFlags
                                     configFlags installFlags haddockFlags
    }


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


---------------------------------------------
-- Reading packages in the project
--

data ProjectPackageLocation =
     ProjectPackageLocalCabalFile FilePath
   | ProjectPackageLocalDirectory FilePath FilePath -- dir and .cabal file
   | ProjectPackageLocalTarball   FilePath
   | ProjectPackageRemoteTarball  URI
   | ProjectPackageRemoteRepo     SourceRepo
   | ProjectPackageNamed          Dependency
  deriving Show


-- | Exception thrown by 'findProjectPackages'.
--
newtype BadPackageLocations = BadPackageLocations [BadPackageLocation]
  deriving (Show, Typeable)

instance Exception BadPackageLocations
--TODO: [required eventually] displayException for nice rendering
--TODO: [nice to have] custom exception subclass for Doc rendering, colour etc

data BadPackageLocation
   = BadPackageLocationFile    BadPackageLocationMatch
   | BadLocGlobEmptyMatch      String
   | BadLocGlobBadMatches      String [BadPackageLocationMatch]
   | BadLocUnexpectedUriScheme String
   | BadLocUnrecognisedUri     String
   | BadLocUnrecognised        String
  deriving Show

data BadPackageLocationMatch
   = BadLocUnexpectedFile      String
   | BadLocNonexistantFile     String
   | BadLocDirNoCabalFile      String
   | BadLocDirManyCabalFiles   String
  deriving Show


-- | Given the project config, 
--
-- Throws 'BadPackageLocations'.
--
findProjectPackages :: FilePath -> ProjectConfig
                    -> Rebuild [ProjectPackageLocation]
findProjectPackages projectRootDir ProjectConfig{..} = do

    requiredPkgs <- findPackageLocations True    projectPackages
    optionalPkgs <- findPackageLocations False   projectPackagesOptional
    let repoPkgs  = map ProjectPackageRemoteRepo projectPackagesRepo
        namedPkgs = map ProjectPackageNamed      projectPackagesNamed

    return (concat [requiredPkgs, optionalPkgs, repoPkgs, namedPkgs])
  where
    findPackageLocations required pkglocstr = do
      (problems, pkglocs) <-
        partitionEithers <$> mapM (findPackageLocation required) pkglocstr
      unless (null problems) $
        liftIO $ throwIO $ BadPackageLocations problems
      return (concat pkglocs)


    findPackageLocation :: Bool -> String
                        -> Rebuild (Either BadPackageLocation
                                          [ProjectPackageLocation])
    findPackageLocation _required@True pkglocstr =
      -- strategy: try first as a file:// or http(s):// URL.
      -- then as a file glob (usually encompassing single file)
      -- finally as a single file, for files that fail to parse as globs
                    checkIsUriPackage pkglocstr
      `mplusMaybeT` checkIsFileGlobPackage pkglocstr
      `mplusMaybeT` checkIsSingleFilePackage pkglocstr
      >>= maybe (return (Left (BadLocUnrecognised pkglocstr))) return


    findPackageLocation _required@False pkglocstr = do
      -- just globs for optional case
      res <- checkIsFileGlobPackage pkglocstr
      case res of
        Nothing              -> return (Left (BadLocUnrecognised pkglocstr))
        Just (Left _)        -> return (Right []) -- it's optional
        Just (Right pkglocs) -> return (Right pkglocs)


    checkIsUriPackage, checkIsFileGlobPackage, checkIsSingleFilePackage
      :: String -> Rebuild (Maybe (Either BadPackageLocation
                                         [ProjectPackageLocation]))
    checkIsUriPackage pkglocstr =
      return $!
      case parseAbsoluteURI pkglocstr of
        Just uri@URI {
            uriScheme    = scheme,
            uriAuthority = Just URIAuth { uriRegName = host }
          }
          | recognisedScheme && not (null host) ->
            Just (Right [ProjectPackageRemoteTarball uri])

          | not recognisedScheme && not (null host) ->
            Just (Left (BadLocUnexpectedUriScheme pkglocstr))

          | recognisedScheme && null host ->
            Just (Left (BadLocUnrecognisedUri pkglocstr))
          where
            recognisedScheme = scheme == "http:" || scheme == "https:"
                            || scheme == "file:"

        _ -> Nothing


    checkIsFileGlobPackage pkglocstr =
      case simpleParse pkglocstr of
        Nothing   -> return Nothing
        Just glob -> liftM Just $ do
          matches <- matchFileGlob projectRootDir glob
          case matches of
            [] | isJust (isTrivialFilePathGlob glob)
               -> return (Left (BadPackageLocationFile 
                                  (BadLocNonexistantFile pkglocstr)))

            [] -> return (Left (BadLocGlobEmptyMatch pkglocstr))

            _  -> do
              (failures, pkglocs) <- partitionEithers <$>
                                     mapM checkFilePackageMatch matches
              if null pkglocs
                then return (Left (BadLocGlobBadMatches pkglocstr failures))
                else return (Right pkglocs)


    checkIsSingleFilePackage pkglocstr = do
      let filename = projectRootDir </> pkglocstr
      isFile <- liftIO $ doesFileExist filename
      isDir  <- liftIO $ doesDirectoryExist filename
      if isFile || isDir
        then checkFilePackageMatch pkglocstr
         >>= either (return . Just . Left  . BadPackageLocationFile)
                    (return . Just . Right . (\x->[x]))
        else return Nothing


    checkFilePackageMatch :: String -> Rebuild (Either BadPackageLocationMatch
                                                       ProjectPackageLocation)
    checkFilePackageMatch pkglocstr = do
      let filename = projectRootDir </> pkglocstr
      isDir  <- liftIO $ doesDirectoryExist filename
      parentDirExists <- case takeDirectory filename of
                           []  -> return False
                           dir -> liftIO $ doesDirectoryExist dir
      case () of
        _ | isDir
         -> do let dirname = filename -- now we know its a dir
                   glob    = globStarDotCabal pkglocstr
               matches <- matchFileGlob projectRootDir glob
               case matches of
                 [match]
                     -> return (Right (ProjectPackageLocalDirectory
                                         dirname cabalFile))
                   where
                     cabalFile = dirname </> match
                 []  -> return (Left (BadLocDirNoCabalFile pkglocstr))
                 _   -> return (Left (BadLocDirManyCabalFiles pkglocstr))

          | extensionIsTarGz filename
         -> return (Right (ProjectPackageLocalTarball filename))

          | takeExtension filename == ".cabal"
         -> return (Right (ProjectPackageLocalCabalFile filename))

          | parentDirExists
         -> return (Left (BadLocNonexistantFile pkglocstr))

          | otherwise
         -> return (Left (BadLocUnexpectedFile pkglocstr))


    extensionIsTarGz f = takeExtension f                 == ".gz"
                      && takeExtension (dropExtension f) == ".tar"


-- | The glob @$dir/*.cabal@
globStarDotCabal :: FilePath -> FilePathGlob
globStarDotCabal =
    foldr (\dirpart -> GlobDir (Glob [Literal dirpart]))
          (GlobFile (Glob [WildCard, Literal ".cabal"]))
  . splitDirectories


--TODO: [code cleanup] use sufficiently recent transformers package
mplusMaybeT :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
mplusMaybeT ma mb = do
  mx <- ma
  case mx of
    Nothing -> mb
    Just x  -> return (Just x)


readSourcePackage :: Verbosity -> ProjectPackageLocation
                  -> Rebuild SourcePackage
readSourcePackage verbosity (ProjectPackageLocalCabalFile cabalFile) =
    readSourcePackage verbosity (ProjectPackageLocalDirectory dir cabalFile)
  where
    dir = takeDirectory cabalFile

readSourcePackage verbosity (ProjectPackageLocalDirectory dir cabalFile) = do
    -- no need to monitorFiles because findProjectCabalFiles did it already
    pkgdesc <- liftIO $ readPackageDescription verbosity cabalFile
    return SourcePackage {
      packageInfoId        = packageId pkgdesc,
      packageDescription   = pkgdesc,
      packageSource        = LocalUnpackedPackage dir,
      packageDescrOverride = Nothing
    }
readSourcePackage _verbosity _ =
    fail $ "TODO: add support for fetching and reading local tarballs, remote "
        ++ "tarballs, remote repos and passing named packages through"


------------------------------------------------------------------
-- Representing the project config file in terms of legacy types
--

-- | We already have parsers\/pretty-printers for almost all the fields in the
-- project config file, but they're in terms of the types used for the command
-- line flags for Setup.hs or cabal commands. We don't want to redefine them
-- all, at least not yet so for the moment we use the parsers at the old types
-- and use conversion functions.
--
-- Ultimately if\/when this project-based approach becomes the default then we
-- can redefine the parsers directly for the new types.
--
data LegacyProjectConfig = LegacyProjectConfig {
       legacyPackages          :: [String],
       legacyPackagesOptional  :: [String],
       legacyPackagesRepo      :: [SourceRepo],
       legacyPackagesNamed     :: [Dependency],

       legacySharedConfig      :: LegacySharedConfig,
       legacyLocalConfig       :: LegacyPackageConfig,
       legacySpecificConfig    :: Map PackageName LegacyPackageConfig
     }

instance Monoid LegacyProjectConfig where
  mempty  = LegacyProjectConfig mempty mempty mempty mempty
                                mempty mempty mempty
  mappend = (<>)

instance Semigroup LegacyProjectConfig where
  a <> b =
    LegacyProjectConfig {
      legacyPackages           = combine legacyPackages,
      legacyPackagesOptional   = combine legacyPackagesOptional,
      legacyPackagesRepo       = combine legacyPackagesRepo,
      legacyPackagesNamed      = combine legacyPackagesNamed,
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
  mempty  = LegacyPackageConfig mempty mempty
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
  mempty  = LegacySharedConfig mempty mempty mempty
  mappend = (<>)

instance Semigroup LegacySharedConfig where
  a <> b =
    LegacySharedConfig {
      legacyGlobalFlags        = combine legacyGlobalFlags,
      legacyConfigureExFlags   = combine legacyConfigureExFlags,
      legacyInstallFlags       = combine legacyInstallFlags
    }
    where combine field = field a `mappend` field b


------------------------------------------------------------------
-- Converting from and to the legacy types
--

-- | Convert from the types currently used for the user-wide @~/.cabal/config@
-- file into the 'ProjectConfig' type.
--
-- Only a subset of the 'ProjectConfig' can be represented in the user-wide
-- config. In particular it does not include packages that are in the project,
-- and it also doesn't support package-specific configuration (only
-- configuration that applies to all packages).
--
convertLegacyGlobalConfig :: SavedConfig -> ProjectConfig
convertLegacyGlobalConfig
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
      projectConfigShared        = configAllPackages,
      projectConfigLocalPackages = configLocalPackages,
      projectConfigBuildOnly     = configBuildOnly
    }
  where
    --TODO: [code cleanup] eliminate use of default*Flags here and specify the
    -- defaults in the various resolve functions in terms of the new types.
    configExFlags' = defaultConfigExFlags <> configExFlags
    installFlags'  = defaultInstallFlags  <> installFlags
    haddockFlags'  = defaultHaddockFlags  <> haddockFlags

    configLocalPackages = convertLegacyPerPackageFlags
                            configFlags installFlags' haddockFlags'
    configAllPackages   = convertLegacyAllPackageFlags
                            globalFlags configFlags
                            configExFlags' installFlags'
    configBuildOnly     = convertLegacyBuildOnlyFlags
                            globalFlags configFlags
                            installFlags' haddockFlags'


-- | Convert the project config from the legacy types to the 'ProjectConfig'
-- and associated types. See 'LegacyProjectConfig' for an explanation of the
-- approach.
--
convertLegacyProjectConfig :: LegacyProjectConfig -> ProjectConfig
convertLegacyProjectConfig
  LegacyProjectConfig {
    legacyPackages,
    legacyPackagesOptional,
    legacyPackagesRepo,
    legacyPackagesNamed,
    legacySharedConfig = LegacySharedConfig globalFlags configExFlags
                                            installFlags,
    legacyLocalConfig  = LegacyPackageConfig configFlags haddockFlags,
    legacySpecificConfig
  } =

    ProjectConfig {
      projectPackages              = legacyPackages,
      projectPackagesOptional      = legacyPackagesOptional,
      projectPackagesRepo          = legacyPackagesRepo,
      projectPackagesNamed         = legacyPackagesNamed,

      projectConfigBuildOnly       = configBuildOnly,
      projectConfigShared          = configAllPackages,
      projectConfigLocalPackages   = configLocalPackages,
      projectConfigSpecificPackage = fmap perPackage legacySpecificConfig
    }
  where
    configLocalPackages = convertLegacyPerPackageFlags
                            configFlags installFlags haddockFlags
    configAllPackages   = convertLegacyAllPackageFlags
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


-- | Helper used by other conversion functions that returns the
-- 'ProjectConfigShared' subset of the 'ProjectConfig'.
--
convertLegacyAllPackageFlags :: GlobalFlags -> ConfigFlags
                             -> ConfigExFlags -> InstallFlags
                             -> ProjectConfigShared
convertLegacyAllPackageFlags globalFlags configFlags
                             configExFlags installFlags =
    ProjectConfigShared{..}
  where
    GlobalFlags {
      globalConfigFile        = _, -- TODO: [required feature]
      globalSandboxConfigFile = _, -- ??
      globalRemoteRepos       = projectConfigRemoteRepos,
      globalLocalRepos        = projectConfigLocalRepos
    } = globalFlags

    ConfigFlags {
      configProgramPaths        = projectConfigProgramPaths,
      configProgramArgs         = projectConfigProgramArgs,
      configProgramPathExtra    = projectConfigProgramPathExtra,
      configHcFlavor            = projectConfigHcFlavor,
      configHcPath              = projectConfigHcPath,
      configHcPkg               = projectConfigHcPkg,
      configVanillaLib          = projectConfigVanillaLib,
      configSharedLib           = projectConfigSharedLib,
      configInstallDirs         = projectConfigInstallDirs,
      configUserInstall         = projectConfigUserInstall,
      configPackageDBs          = projectConfigPackageDBs,
      configConfigurationsFlags = projectConfigFlagAssignment,
      configRelocatable         = projectConfigRelocatable
    } = configFlags

    ConfigExFlags {
      configCabalVersion        = projectConfigCabalVersion,
      configExConstraints       = projectConfigConstraints,
      configPreferences         = projectConfigPreferences,
      configSolver              = projectConfigSolver,
      configAllowNewer          = projectConfigAllowNewer
    } = configExFlags

    InstallFlags {
      installHaddockIndex       = projectConfigHaddockIndex,
      installReinstall          = projectConfigReinstall,
      installAvoidReinstalls    = projectConfigAvoidReinstalls,
      installOverrideReinstall  = projectConfigOverrideReinstall,
      installMaxBackjumps       = projectConfigMaxBackjumps,
      installUpgradeDeps        = projectConfigUpgradeDeps,
      installReorderGoals       = projectConfigReorderGoals,
      installIndependentGoals   = projectConfigIndependentGoals,
      installShadowPkgs         = projectConfigShadowPkgs,
      installStrongFlags        = projectConfigStrongFlags
    } = installFlags



-- | Helper used by other conversion functions that returns the
-- 'PackageConfig' subset of the 'ProjectConfig'.
--
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
      configConfigurationsFlags = _projectConfigFlagAssignment, --TODO: should be per pkg
      configTests               = packageConfigTests,
      configBenchmarks          = packageConfigBenchmarks,
      configCoverage            = coverage,
      configLibCoverage         = libcoverage, --deprecated
      configDebugInfo           = packageConfigDebugInfo
    } = configFlags

    packageConfigCoverage       = coverage <> libcoverage
    --TODO: defer this merging to the resolve phase

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



-- | Helper used by other conversion functions that returns the
-- 'ProjectConfigBuildOnly' subset of the 'ProjectConfig'.
--
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


convertToLegacyProjectConfig :: ProjectConfig -> LegacyProjectConfig
convertToLegacyProjectConfig
    projectConfig@ProjectConfig {
      projectPackages,
      projectPackagesOptional,
      projectPackagesRepo,
      projectPackagesNamed,
      projectConfigLocalPackages,
      projectConfigSpecificPackage
    } =
    LegacyProjectConfig {
      legacyPackages         = projectPackages,
      legacyPackagesOptional = projectPackagesOptional,
      legacyPackagesRepo     = projectPackagesRepo,
      legacyPackagesNamed    = projectPackagesNamed,
      legacySharedConfig     = convertToLegacySharedConfig projectConfig,
      legacyLocalConfig      = convertToLegacyAllPackageConfig projectConfig
                            <> convertToLegacyPerPackageConfig
                                 projectConfigLocalPackages,
      legacySpecificConfig   = fmap convertToLegacyPerPackageConfig
                                    projectConfigSpecificPackage
    }

convertToLegacySharedConfig :: ProjectConfig -> LegacySharedConfig
convertToLegacySharedConfig
    ProjectConfig {
      projectConfigBuildOnly     = ProjectConfigBuildOnly {..},
      projectConfigShared        = ProjectConfigShared {..},
      projectConfigLocalPackages = PackageConfig {..}
    } =

    LegacySharedConfig {
      legacyGlobalFlags      = globalFlags,
      legacyConfigureExFlags = configExFlags,
      legacyInstallFlags     = installFlags
    }
  where
    globalFlags = GlobalFlags {
      globalVersion           = mempty,
      globalNumericVersion    = mempty,
      globalConfigFile        = mempty,
      globalSandboxConfigFile = mempty,
      globalConstraintsFile   = mempty,
      globalRemoteRepos       = projectConfigRemoteRepos,
      globalCacheDir          = projectConfigCacheDir,
      globalLocalRepos        = projectConfigLocalRepos,
      globalLogsDir           = projectConfigLogsDir,
      globalWorldFile         = projectConfigWorldFile,
      globalRequireSandbox    = mempty,
      globalIgnoreSandbox     = mempty,
      globalIgnoreExpiry      = projectConfigIgnoreExpiry,
      globalHttpTransport     = projectConfigHttpTransport
    }

    configExFlags = ConfigExFlags {
      configCabalVersion  = projectConfigCabalVersion,
      configExConstraints = projectConfigConstraints,
      configPreferences   = projectConfigPreferences,
      configSolver        = projectConfigSolver,
      configAllowNewer    = projectConfigAllowNewer
    }

    installFlags = InstallFlags {
      installDocumentation     = packageConfigDocumentation,
      installHaddockIndex      = projectConfigHaddockIndex,
      installDryRun            = projectConfigDryRun,
      installReinstall         = projectConfigReinstall,
      installAvoidReinstalls   = projectConfigAvoidReinstalls,
      installOverrideReinstall = projectConfigOverrideReinstall,
      installMaxBackjumps      = projectConfigMaxBackjumps,
      installUpgradeDeps       = projectConfigUpgradeDeps,
      installReorderGoals      = projectConfigReorderGoals,
      installIndependentGoals  = projectConfigIndependentGoals,
      installShadowPkgs        = projectConfigShadowPkgs,
      installStrongFlags       = projectConfigStrongFlags,
      installOnly              = mempty,
      installOnlyDeps          = projectConfigOnlyDeps,
      installRootCmd           = projectConfigRootCmd,
      installSummaryFile       = projectConfigSummaryFile,
      installLogFile           = projectConfigLogFile,
      installBuildReports      = projectConfigBuildReports,
      installReportPlanningFailure = projectConfigReportPlanningFailure,
      installSymlinkBinDir     = projectConfigSymlinkBinDir,
      installOneShot           = projectConfigOneShot,
      installNumJobs           = projectConfigNumJobs,
      installRunTests          = packageConfigRunTests,
      installOfflineMode       = projectConfigOfflineMode
    }


convertToLegacyAllPackageConfig :: ProjectConfig -> LegacyPackageConfig
convertToLegacyAllPackageConfig 
    ProjectConfig {
      projectConfigBuildOnly = ProjectConfigBuildOnly {..},
      projectConfigShared    = ProjectConfigShared {..}
    } =

    LegacyPackageConfig {
      legacyConfigureFlags = configFlags,
      legacyHaddockFlags   = haddockFlags
    }
  where
    configFlags = ConfigFlags {
      configPrograms            = configPrograms mempty,
      configProgramPaths        = projectConfigProgramPaths,
      configProgramArgs         = projectConfigProgramArgs,
      configProgramPathExtra    = projectConfigProgramPathExtra,
      configHcFlavor            = projectConfigHcFlavor,
      configHcPath              = projectConfigHcPath,
      configHcPkg               = projectConfigHcPkg,
      configVanillaLib          = projectConfigVanillaLib,
      configProfLib             = mempty,
      configSharedLib           = projectConfigSharedLib,
      configDynExe              = mempty,
      configProfExe             = mempty,
      configProf                = mempty,
      configProfDetail          = mempty,
      configProfLibDetail       = mempty,
      configConfigureArgs       = mempty,
      configOptimization        = mempty,
      configProgPrefix          = mempty,
      configProgSuffix          = mempty,
      configInstallDirs         = mempty,
      configScratchDir          = mempty,
      configDistPref            = mempty,
      configVerbosity           = projectConfigVerbosity,
      configUserInstall         = projectConfigUserInstall,
      configPackageDBs          = projectConfigPackageDBs,
      configGHCiLib             = mempty,
      configSplitObjs           = mempty,
      configStripExes           = mempty,
      configStripLibs           = mempty,
      configExtraLibDirs        = mempty,
      configConstraints         = mempty,
      configDependencies        = mempty,
      configExtraIncludeDirs    = mempty,
      configIPID                = mempty,
      configConfigurationsFlags = projectConfigFlagAssignment,
      configTests               = mempty,
      configCoverage            = mempty, --TODO: don't merge
      configLibCoverage         = mempty, --TODO: don't merge
      configExactConfiguration  = mempty,
      configBenchmarks          = mempty,
      configFlagError           = mempty,                --TODO: ???
      configRelocatable         = projectConfigRelocatable,
      configDebugInfo           = mempty
    }

    haddockFlags = mempty {
      haddockKeepTempFiles = projectConfigKeepTempFiles
    }


convertToLegacyPerPackageConfig :: PackageConfig -> LegacyPackageConfig
convertToLegacyPerPackageConfig PackageConfig {..} =
    LegacyPackageConfig {
      legacyConfigureFlags = configFlags,
      legacyHaddockFlags   = haddockFlags
    }
  where
    configFlags = ConfigFlags {
      configPrograms            = configPrograms mempty,
      configProgramPaths        = mempty,
      configProgramArgs         = mempty,
      configProgramPathExtra    = mempty,
      configHcFlavor            = mempty,
      configHcPath              = mempty,
      configHcPkg               = mempty,
      configVanillaLib          = mempty,
      configProfLib             = packageConfigProfLib,
      configSharedLib           = mempty,
      configDynExe              = packageConfigDynExe,
      configProfExe             = packageConfigProfExe,
      configProf                = packageConfigProf,
      configProfDetail          = packageConfigProfDetail,
      configProfLibDetail       = packageConfigProfLibDetail,
      configConfigureArgs       = packageConfigConfigureArgs,
      configOptimization        = packageConfigOptimization,
      configProgPrefix          = packageConfigProgPrefix,
      configProgSuffix          = packageConfigProgSuffix,
      configInstallDirs         = mempty,
      configScratchDir          = mempty,
      configDistPref            = mempty,
      configVerbosity           = mempty,
      configUserInstall         = mempty,
      configPackageDBs          = mempty,
      configGHCiLib             = packageConfigGHCiLib,
      configSplitObjs           = packageConfigSplitObjs,
      configStripExes           = packageConfigStripExes,
      configStripLibs           = packageConfigStripLibs,
      configExtraLibDirs        = packageConfigExtraLibDirs,
      configConstraints         = mempty,
      configDependencies        = mempty,
      configExtraIncludeDirs    = packageConfigExtraIncludeDirs,
      configIPID                = mempty,
      configConfigurationsFlags = mempty,
      configTests               = packageConfigTests,
      configCoverage            = packageConfigCoverage, --TODO: don't merge
      configLibCoverage         = packageConfigCoverage, --TODO: don't merge
      configExactConfiguration  = mempty,
      configBenchmarks          = packageConfigBenchmarks,
      configFlagError           = mempty,                --TODO: ???
      configRelocatable         = mempty,
      configDebugInfo           = packageConfigDebugInfo
    }
    haddockFlags = HaddockFlags {
      haddockProgramPaths  = mempty,
      haddockProgramArgs   = mempty,
      haddockHoogle        = packageConfigHaddockHoogle,
      haddockHtml          = packageConfigHaddockHtml,
      haddockHtmlLocation  = packageConfigHaddockHtmlLocation,
      haddockForHackage    = mempty, --TODO: added recently
      haddockExecutables   = packageConfigHaddockExecutables,
      haddockTestSuites    = packageConfigHaddockTestSuites,
      haddockBenchmarks    = packageConfigHaddockBenchmarks,
      haddockInternal      = packageConfigHaddockInternal,
      haddockCss           = packageConfigHaddockCss,
      haddockHscolour      = packageConfigHaddockHscolour,
      haddockHscolourCss   = packageConfigHaddockHscolourCss,
      haddockContents      = packageConfigHaddockContents,
      haddockDistPref      = mempty,
      haddockKeepTempFiles = mempty,
      haddockVerbosity     = mempty
    }


------------------------------------------------
-- Parsing and showing the project config file
--

parseLegacyProjectConfig :: String -> ParseResult LegacyProjectConfig
parseLegacyProjectConfig =
    parseConfig legacyProjectConfigFieldDescrs
                legacyPackageConfigSectionDescrs
                mempty

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

    [ newLineListField "packages"
        Disp.text parsePackageLocationTokenQ
        legacyPackages
        (\v flags -> flags { legacyPackages = v })
    , newLineListField "optional-packages"
        Disp.text parsePackageLocationTokenQ
        legacyPackagesOptional
        (\v flags -> flags { legacyPackagesOptional = v })
    , newLineListField "extra-packages"
        disp parse
        legacyPackagesNamed
        (\v flags -> flags { legacyPackagesNamed = v })
    ]

 ++ map (liftField
           legacySharedConfig
           (\flags conf -> conf { legacySharedConfig = flags }))
        legacySharedConfigFieldDescrs

 ++ map (liftField
           legacyLocalConfig
           (\flags conf -> conf { legacyLocalConfig = flags }))
        legacyPackageConfigFieldDescrs

-- | This is a bit tricky since it has to cover globs which have embedded @,@
-- chars. But we don't just want to parse strictly as a glob since we want to
-- allow http urls which don't parse as globs, and possibly some
-- system-dependent file paths. So we parse fairly liberally as a token, but
-- we allow @,@ inside matched @{}@ braces.
--
parsePackageLocationTokenQ :: ReadP r String
parsePackageLocationTokenQ = parseHaskellString
                   Parse.<++ parsePackageLocationToken
  where
    parseHaskellString :: ReadP r String
    parseHaskellString = Parse.readS_to_P reads

    parsePackageLocationToken :: ReadP r String
    parsePackageLocationToken = fmap fst (Parse.gather outerTerm)
      where
        outerTerm   = alternateEither1 outerToken (braces innerTerm)
        innerTerm   = alternateEither innerToken (braces innerTerm)
        outerToken  = Parse.munch1 outerChar >> return ()
        innerToken  = Parse.munch1 innerChar >> return ()
        outerChar c = not (isSpace c || c == '{' || c == '}' || c == ',')
        innerChar c = not (isSpace c || c == '{' || c == '}')
        braces      = Parse.between (Parse.char '{') (Parse.char '}')

    alternateEither, alternateEither1, alternate, alternate1
      :: ReadP r () -> ReadP r () -> ReadP r ()

    alternateEither  p q = alternateEither1 p q +++ return ()
    alternateEither1 p q = alternate1 p q +++ alternate1 q p
    alternate1       p q = p >> alternate q p
    alternate        p q = alternate1 p q +++ return ()


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
  . addFields
      [ commaNewLineListField "constraints"
        (disp . fst) (fmap (\constraint -> (constraint, constraintSrc)) parse)
        configExConstraints (\v conf -> conf { configExConstraints = v })

      , commaNewLineListField "preferences"
        disp parse
        configPreferences (\v conf -> conf { configPreferences = v })
      ]
  . filterFields
      [ "cabal-lib-version", "solver", "allow-newer"
        -- not "constraint" or "preference", we use our own plural ones above
      ]
  . commandOptionsToFields
  ) (configureExOptions ParseArgs constraintSrc)
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
  where
    constraintSrc = ConstraintSourceProjectConfig "TODO"
    addFields     = (++)

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
    [ packageRepoSectionDescr
    , packageSpecificOptionsSectionDescr
    , liftSection
        legacyLocalConfig
        (\flags conf -> conf { legacyLocalConfig = flags })
        programOptionsSectionDescr
    , liftSection
        legacyLocalConfig
        (\flags conf -> conf { legacyLocalConfig = flags })
        programLocationsSectionDescr
    ]

packageRepoSectionDescr :: SectionDescr LegacyProjectConfig
packageRepoSectionDescr =
    SectionDescr {
      sectionName        = "source-repository-package",
      sectionFields      = sourceRepoFieldDescrs,
      sectionSubsections = [],
      sectionGet         = map (\x->("", x))
                         . legacyPackagesRepo,
      sectionSet         =
        \lineno unused pkgrepo projconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'source-repository-package' takes no arguments"
          return projconf {
            legacyPackagesRepo = legacyPackagesRepo projconf ++ [pkgrepo]
          },
      sectionEmpty       = SourceRepo {
                             repoKind     = RepoThis, -- hopefully unused
                             repoType     = Nothing,
                             repoLocation = Nothing,
                             repoModule   = Nothing,
                             repoBranch   = Nothing,
                             repoTag      = Nothing,
                             repoSubdir   = Nothing
                           }
    }

packageSpecificOptionsSectionDescr :: SectionDescr LegacyProjectConfig
packageSpecificOptionsSectionDescr =
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
        \lineno pkgnamestr pkgconf projconf -> do
          pkgname <- case simpleParse pkgnamestr of
            Just pkgname -> return pkgname
            Nothing      -> syntaxError lineno $
                                "a 'package' section requires a package name "
                             ++ "as an argument"
          return projconf {
            legacySpecificConfig =
              Map.insertWith mappend pkgname pkgconf
                             (legacySpecificConfig projconf)
          },
      sectionEmpty       = mempty
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
          },
      sectionEmpty       = mempty
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
          return pkgconf { legacyConfigureFlags = confflags },
      sectionEmpty       = mempty
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


-------------------------------
-- Local field utils
--

--TODO: [code cleanup] all these utils should move to Distribution.ParseUtils
-- either augmenting or replacing the ones there

--TODO: [code cleanup] this is a different definition from listField, like
-- commaNewLineListField it pretty prints on multiple lines
newLineListField :: String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
newLineListField = listFieldWithSep Disp.sep

--TODO: [code cleanup] local copy purely so we can use the fixed version
-- of parseOptCommaList below
listFieldWithSep :: ([Doc] -> Doc) -> String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listFieldWithSep separator name showF readF get set =
  liftField get set' $
    ParseUtils.field name showF' (parseOptCommaList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF'    = separator . map showF

--TODO: [code cleanup] local redefinition that should replace the version in
-- D.ParseUtils. This version avoid parse ambiguity for list element parsers
-- that have multiple valid parses of prefixes.
parseOptCommaList :: ReadP r a -> ReadP r [a]
parseOptCommaList p = Parse.sepBy p sep
  where
    -- The separator must not be empty or it introduces ambiguity
    sep = (Parse.skipSpaces >> Parse.char ',' >> Parse.skipSpaces)
      +++ (Parse.satisfy isSpace >> Parse.skipSpaces)

