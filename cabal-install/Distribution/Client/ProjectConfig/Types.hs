{-# LANGUAGE RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable,
             ExistentialQuantification, ScopedTypeVariables #-}

-- | Handling project configuration, types and reading.
--
module Distribution.Client.ProjectConfig.Types (

    -- * Types for project config
    ProjectConfig(..),
    ProjectConfigBuildOnly(..),
    ProjectConfigShared(..),
    PackageConfig(..),

    -- * Resolving configuration
    SolverSettings(..),
    BuildTimeSettings(..),

  ) where

import Distribution.Client.Types
         ( RemoteRepo )
import Distribution.Client.Dependency.Types
         ( PreSolver, ConstraintSource )
import Distribution.Client.Targets
         ( UserConstraint )
import Distribution.Client.BuildReports.Types 
         ( ReportLevel(..) )

import Distribution.Package
         ( PackageName, PackageId, UnitId, Dependency )
import Distribution.Version
         ( Version )
import Distribution.System
         ( Platform )
import Distribution.PackageDescription
         ( FlagAssignment, SourceRepo(..) )
import Distribution.Simple.Compiler
         ( Compiler, CompilerFlavor, PackageDB
         , OptimisationLevel(..), ProfDetailLevel, DebugInfoLevel(..) )
import Distribution.Simple.Setup
         ( Flag, AllowNewer(..) )
import Distribution.Simple.InstallDirs
         ( InstallDirs, PathTemplate )
import Distribution.Utils.NubList
         ( NubList )
import Distribution.Verbosity
         ( Verbosity )

import Data.Map (Map)
import Distribution.Compat.Binary (Binary)
import Distribution.Compat.Semigroup
import GHC.Generics (Generic)


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
       packageConfigExtraFrameworkDirs  :: [FilePath],
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
      packageConfigExtraFrameworkDirs  = mempty,
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
      packageConfigExtraFrameworkDirs  = combine packageConfigExtraFrameworkDirs,
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

