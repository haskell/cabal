module Distribution.Client.ProjectConfig.Lens where

import Distribution.Client.BuildReports.Types (ReportLevel (..))
import Distribution.Client.CmdInstall.ClientInstallFlags (ClientInstallFlags (..))
import Distribution.Client.Dependency.Types (PreSolver (..))
import Distribution.Client.IndexUtils.ActiveRepos
  ( ActiveRepos
  )
import Distribution.Client.IndexUtils.IndexState (TotalIndexState)
import Distribution.Client.ProjectConfig.Types (MapMappend, PackageConfig, ProjectConfig (..), ProjectConfigBuildOnly (..), ProjectConfigProvenance, ProjectConfigShared)
import qualified Distribution.Client.ProjectConfig.Types as T
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types.AllowNewer (AllowNewer, AllowOlder)
import Distribution.Client.Types.SourceRepo (SourceRepoList)
import Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy (WriteGhcEnvironmentFilesPolicy)
import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Package
  ( PackageName
  )
import Distribution.PackageDescription
  ( FlagAssignment
  )
import Distribution.Simple.Compiler
  ( DebugInfoLevel (..)
  , OptimisationLevel (..)
  , PackageDB
  , ProfDetailLevel
  )
import Distribution.Simple.InstallDirs
  ( PathTemplate
  )
import Distribution.Simple.Setup
  ( DumpBuildInfo (..)
  , Flag
  , HaddockTarget (..)
  , TestShowDetails (..)
  )
import Distribution.Solver.Types.ConstraintSource (ConstraintSource)
import Distribution.Solver.Types.Settings
  ( AllowBootLibInstalls (..)
  , CountConflicts (..)
  , FineGrainedConflicts (..)
  , IndependentGoals (..)
  , MinimizeConflictSet (..)
  , OnlyConstrained (..)
  , PreferOldest (..)
  , ReorderGoals (..)
  , StrongFlags (..)
  )
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint
  )
import Distribution.Types.Version (Version)
import Distribution.Utils.NubList
  ( NubList
  )
import Distribution.Verbosity

projectPackages :: Lens' ProjectConfig [String]
projectPackages f s = fmap (\x -> s{T.projectPackages = x}) (f (T.projectPackages s))
{-# INLINEABLE projectPackages #-}

projectPackagesOptional :: Lens' ProjectConfig [String]
projectPackagesOptional f s = fmap (\x -> s{T.projectPackagesOptional = x}) (f (T.projectPackagesOptional s))
{-# INLINEABLE projectPackagesOptional #-}

projectPackagesRepo :: Lens' ProjectConfig [SourceRepoList]
projectPackagesRepo f s = fmap (\x -> s{T.projectPackagesRepo = x}) (f (T.projectPackagesRepo s))
{-# INLINEABLE projectPackagesRepo #-}

projectPackagesNamed :: Lens' ProjectConfig [PackageVersionConstraint]
projectPackagesNamed f s = fmap (\x -> s{T.projectPackagesNamed = x}) (f (T.projectPackagesNamed s))
{-# INLINEABLE projectPackagesNamed #-}

projectConfigBuildOnly :: Lens' ProjectConfig ProjectConfigBuildOnly
projectConfigBuildOnly f s = fmap (\x -> s{T.projectConfigBuildOnly = x}) (f (T.projectConfigBuildOnly s))
{-# INLINEABLE projectConfigBuildOnly #-}

projectConfigShared :: Lens' ProjectConfig ProjectConfigShared
projectConfigShared f s = fmap (\x -> s{T.projectConfigShared = x}) (f (T.projectConfigShared s))
{-# INLINEABLE projectConfigShared #-}

projectConfigProvenance :: Lens' ProjectConfig (Set ProjectConfigProvenance)
projectConfigProvenance f s = fmap (\x -> s{T.projectConfigProvenance = x}) (f (T.projectConfigProvenance s))
{-# INLINEABLE projectConfigProvenance #-}

projectConfigAllPackages :: Lens' ProjectConfig PackageConfig
projectConfigAllPackages f s = fmap (\x -> s{T.projectConfigAllPackages = x}) (f (T.projectConfigAllPackages s))
{-# INLINEABLE projectConfigAllPackages #-}

projectConfigLocalPackages :: Lens' ProjectConfig PackageConfig
projectConfigLocalPackages f s = fmap (\x -> s{T.projectConfigLocalPackages = x}) (f (T.projectConfigLocalPackages s))
{-# INLINEABLE projectConfigLocalPackages #-}

projectConfigSpecificPackage :: Lens' ProjectConfig (MapMappend PackageName PackageConfig)
projectConfigSpecificPackage f s = fmap (\x -> s{T.projectConfigSpecificPackage = x}) (f (T.projectConfigSpecificPackage s))
{-# INLINEABLE projectConfigSpecificPackage #-}

projectConfigVerbosity :: Lens' ProjectConfigBuildOnly (Flag Verbosity)
projectConfigVerbosity f s = fmap (\x -> s{T.projectConfigVerbosity = x}) (f (T.projectConfigVerbosity s))
{-# INLINEABLE projectConfigVerbosity #-}

projectConfigSummaryFile :: Lens' ProjectConfigBuildOnly (NubList PathTemplate)
projectConfigSummaryFile f s = fmap (\x -> s{T.projectConfigSummaryFile = x}) (f (T.projectConfigSummaryFile s))
{-# INLINEABLE projectConfigSummaryFile #-}

projectConfigLogFile :: Lens' ProjectConfigBuildOnly (Flag PathTemplate)
projectConfigLogFile f s = fmap (\x -> s{T.projectConfigLogFile = x}) (f (T.projectConfigLogFile s))
{-# INLINEABLE projectConfigLogFile #-}

projectConfigBuildReports :: Lens' ProjectConfigBuildOnly (Flag ReportLevel)
projectConfigBuildReports f s = fmap (\x -> s{T.projectConfigBuildReports = x}) (f (T.projectConfigBuildReports s))
{-# INLINEABLE projectConfigBuildReports #-}

projectConfigReportPlanningFailure :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigReportPlanningFailure f s = fmap (\x -> s{T.projectConfigReportPlanningFailure = x}) (f (T.projectConfigReportPlanningFailure s))
{-# INLINEABLE projectConfigReportPlanningFailure #-}

projectConfigSymlinkBinDir :: Lens' ProjectConfigBuildOnly (Flag FilePath)
projectConfigSymlinkBinDir f s = fmap (\x -> s{T.projectConfigSymlinkBinDir = x}) (f (T.projectConfigSymlinkBinDir s))
{-# INLINEABLE projectConfigSymlinkBinDir #-}

projectConfigNumJobs :: Lens' ProjectConfigBuildOnly (Flag (Maybe Int))
projectConfigNumJobs f s = fmap (\x -> s{T.projectConfigNumJobs = x}) (f (T.projectConfigNumJobs s))
{-# INLINEABLE projectConfigNumJobs #-}

projectConfigUseSemaphore :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigUseSemaphore f s = fmap (\x -> s{T.projectConfigUseSemaphore = x}) (f (T.projectConfigUseSemaphore s))
{-# INLINEABLE projectConfigUseSemaphore #-}

projectConfigKeepGoing :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigKeepGoing f s = fmap (\x -> s{T.projectConfigKeepGoing = x}) (f (T.projectConfigKeepGoing s))
{-# INLINEABLE projectConfigKeepGoing #-}

projectConfigOfflineMode :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigOfflineMode f s = fmap (\x -> s{T.projectConfigOfflineMode = x}) (f (T.projectConfigOfflineMode s))
{-# INLINEABLE projectConfigOfflineMode #-}

projectConfigKeepTempFiles :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigKeepTempFiles f s = fmap (\x -> s{T.projectConfigKeepTempFiles = x}) (f (T.projectConfigKeepTempFiles s))
{-# INLINEABLE projectConfigKeepTempFiles #-}

projectConfigHttpTransport :: Lens' ProjectConfigBuildOnly (Flag String)
projectConfigHttpTransport f s = fmap (\x -> s{T.projectConfigHttpTransport = x}) (f (T.projectConfigHttpTransport s))
{-# INLINEABLE projectConfigHttpTransport #-}

projectConfigIgnoreExpiry :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigIgnoreExpiry f s = fmap (\x -> s{T.projectConfigIgnoreExpiry = x}) (f (T.projectConfigIgnoreExpiry s))
{-# INLINEABLE projectConfigIgnoreExpiry #-}

projectConfigCacheDir :: Lens' ProjectConfigBuildOnly (Flag FilePath)
projectConfigCacheDir f s = fmap (\x -> s{T.projectConfigCacheDir = x}) (f (T.projectConfigCacheDir s))
{-# INLINEABLE projectConfigCacheDir #-}

projectConfigLogsDir :: Lens' ProjectConfigBuildOnly (Flag FilePath)
projectConfigLogsDir f s = fmap (\x -> s{T.projectConfigLogsDir = x}) (f (T.projectConfigLogsDir s))
{-# INLINEABLE projectConfigLogsDir #-}

projectConfigClientInstallFlags :: Lens' ProjectConfigBuildOnly (ClientInstallFlags)
projectConfigClientInstallFlags f s = fmap (\x -> s{T.projectConfigClientInstallFlags = x}) (f (T.projectConfigClientInstallFlags s))
{-# INLINEABLE projectConfigClientInstallFlags #-}

projectConfigDistDir :: Lens' ProjectConfigShared (Flag FilePath)
projectConfigDistDir f s = fmap (\x -> s{T.projectConfigDistDir = x}) (f (T.projectConfigDistDir s))
{-# INLINEABLE projectConfigDistDir #-}

projectConfigProjectDir :: Lens' ProjectConfigShared (Flag FilePath)
projectConfigProjectDir f s = fmap (\x -> s{T.projectConfigProjectDir = x}) (f (T.projectConfigProjectDir s))
{-# INLINEABLE projectConfigProjectDir #-}

projectConfigStoreDir :: Lens' ProjectConfigShared (Flag FilePath)
projectConfigStoreDir f s = fmap (\x -> s{T.projectConfigStoreDir = x}) (f (T.projectConfigStoreDir s))
{-# INLINEABLE projectConfigStoreDir #-}

projectConfigPerComponent :: Lens' ProjectConfigShared (Flag Bool)
projectConfigPerComponent f s = fmap (\x -> s{T.projectConfigPerComponent = x}) (f (T.projectConfigPerComponent s))
{-# INLINEABLE projectConfigPerComponent #-}

projectConfigIndependentGoals :: Lens' ProjectConfigShared (Flag IndependentGoals)
projectConfigIndependentGoals f s = fmap (\x -> s{T.projectConfigIndependentGoals = x}) (f (T.projectConfigIndependentGoals s))
{-# INLINEABLE projectConfigIndependentGoals #-}

projectConfigProjectFile :: Lens' ProjectConfigShared (Flag FilePath)
projectConfigProjectFile f s = fmap (\x -> s{T.projectConfigProjectFile = x}) (f (T.projectConfigProjectFile s))
{-# INLINEABLE projectConfigProjectFile #-}

projectConfigIgnoreProject :: Lens' ProjectConfigShared (Flag Bool)
projectConfigIgnoreProject f s = fmap (\x -> s{T.projectConfigIgnoreProject = x}) (f (T.projectConfigIgnoreProject s))
{-# INLINEABLE projectConfigIgnoreProject #-}

projectConfigHcFlavor :: Lens' ProjectConfigShared (Flag CompilerFlavor)
projectConfigHcFlavor f s = fmap (\x -> s{T.projectConfigHcFlavor = x}) (f (T.projectConfigHcFlavor s))
{-# INLINEABLE projectConfigHcFlavor #-}

projectConfigHcPath :: Lens' ProjectConfigShared (Flag FilePath)
projectConfigHcPath f s = fmap (\x -> s{T.projectConfigHcPath = x}) (f (T.projectConfigHcPath s))
{-# INLINEABLE projectConfigHcPath #-}

projectConfigHcPkg :: Lens' ProjectConfigShared (Flag FilePath)
projectConfigHcPkg f s = fmap (\x -> s{T.projectConfigHcPkg = x}) (f (T.projectConfigHcPkg s))
{-# INLINEABLE projectConfigHcPkg #-}

projectConfigHaddockIndex :: Lens' ProjectConfigShared (Flag PathTemplate)
projectConfigHaddockIndex f s = fmap (\x -> s{T.projectConfigHaddockIndex = x}) (f (T.projectConfigHaddockIndex s))
{-# INLINEABLE projectConfigHaddockIndex #-}

projectConfigPackageDBs :: Lens' ProjectConfigShared [Maybe PackageDB]
projectConfigPackageDBs f s = fmap (\x -> s{T.projectConfigPackageDBs = x}) (f (T.projectConfigPackageDBs s))
{-# INLINEABLE projectConfigPackageDBs #-}

projectConfigActiveRepos :: Lens' ProjectConfigShared (Flag ActiveRepos)
projectConfigActiveRepos f s = fmap (\x -> s{T.projectConfigActiveRepos = x}) (f (T.projectConfigActiveRepos s))
{-# INLINEABLE projectConfigActiveRepos #-}

projectConfigIndexState :: Lens' ProjectConfigShared (Flag TotalIndexState)
projectConfigIndexState f s = fmap (\x -> s{T.projectConfigIndexState = x}) (f (T.projectConfigIndexState s))
{-# INLINEABLE projectConfigIndexState #-}

projectConfigConstraints :: Lens' ProjectConfigShared [(UserConstraint, ConstraintSource)]
projectConfigConstraints f s = fmap (\x -> s{T.projectConfigConstraints = x}) (f (T.projectConfigConstraints s))
{-# INLINEABLE projectConfigConstraints #-}

projectConfigPreferences :: Lens' ProjectConfigShared [PackageVersionConstraint]
projectConfigPreferences f s = fmap (\x -> s{T.projectConfigPreferences = x}) (f (T.projectConfigPreferences s))
{-# INLINEABLE projectConfigPreferences #-}

projectConfigCabalVersion :: Lens' ProjectConfigShared (Flag Version)
projectConfigCabalVersion f s = fmap (\x -> s{T.projectConfigCabalVersion = x}) (f (T.projectConfigCabalVersion s))
{-# INLINEABLE projectConfigCabalVersion #-}

projectConfigSolver :: Lens' ProjectConfigShared (Flag PreSolver)
projectConfigSolver f s = fmap (\x -> s{T.projectConfigSolver = x}) (f (T.projectConfigSolver s))
{-# INLINEABLE projectConfigSolver #-}

projectConfigAllowOlder :: Lens' ProjectConfigShared (Maybe AllowOlder)
projectConfigAllowOlder f s = fmap (\x -> s{T.projectConfigAllowOlder = x}) (f (T.projectConfigAllowOlder s))
{-# INLINEABLE projectConfigAllowOlder #-}

projectConfigAllowNewer :: Lens' ProjectConfigShared (Maybe AllowNewer)
projectConfigAllowNewer f s = fmap (\x -> s{T.projectConfigAllowNewer = x}) (f (T.projectConfigAllowNewer s))
{-# INLINEABLE projectConfigAllowNewer #-}

projectConfigWriteGhcEnvironmentFilesPolicy :: Lens' ProjectConfigShared (Flag WriteGhcEnvironmentFilesPolicy)
projectConfigWriteGhcEnvironmentFilesPolicy f s = fmap (\x -> s{T.projectConfigWriteGhcEnvironmentFilesPolicy = x}) (f (T.projectConfigWriteGhcEnvironmentFilesPolicy s))
{-# INLINEABLE projectConfigWriteGhcEnvironmentFilesPolicy #-}

projectConfigMaxBackjumps :: Lens' ProjectConfigShared (Flag Int)
projectConfigMaxBackjumps f s = fmap (\x -> s{T.projectConfigMaxBackjumps = x}) (f (T.projectConfigMaxBackjumps s))
{-# INLINEABLE projectConfigMaxBackjumps #-}

projectConfigReorderGoals :: Lens' ProjectConfigShared (Flag ReorderGoals)
projectConfigReorderGoals f s = fmap (\x -> s{T.projectConfigReorderGoals = x}) (f (T.projectConfigReorderGoals s))
{-# INLINEABLE projectConfigReorderGoals #-}

projectConfigCountConflicts :: Lens' ProjectConfigShared (Flag CountConflicts)
projectConfigCountConflicts f s = fmap (\x -> s{T.projectConfigCountConflicts = x}) (f (T.projectConfigCountConflicts s))
{-# INLINEABLE projectConfigCountConflicts #-}

projectConfigFineGrainedConflicts :: Lens' ProjectConfigShared (Flag FineGrainedConflicts)
projectConfigFineGrainedConflicts f s = fmap (\x -> s{T.projectConfigFineGrainedConflicts = x}) (f (T.projectConfigFineGrainedConflicts s))
{-# INLINEABLE projectConfigFineGrainedConflicts #-}

projectConfigMinimizeConflictSet :: Lens' ProjectConfigShared (Flag MinimizeConflictSet)
projectConfigMinimizeConflictSet f s = fmap (\x -> s{T.projectConfigMinimizeConflictSet = x}) (f (T.projectConfigMinimizeConflictSet s))
{-# INLINEABLE projectConfigMinimizeConflictSet #-}

projectConfigStrongFlags :: Lens' ProjectConfigShared (Flag StrongFlags)
projectConfigStrongFlags f s = fmap (\x -> s{T.projectConfigStrongFlags = x}) (f (T.projectConfigStrongFlags s))
{-# INLINEABLE projectConfigStrongFlags #-}

projectConfigAllowBootLibInstalls :: Lens' ProjectConfigShared (Flag AllowBootLibInstalls)
projectConfigAllowBootLibInstalls f s = fmap (\x -> s{T.projectConfigAllowBootLibInstalls = x}) (f (T.projectConfigAllowBootLibInstalls s))
{-# INLINEABLE projectConfigAllowBootLibInstalls #-}

projectConfigOnlyConstrained :: Lens' ProjectConfigShared (Flag OnlyConstrained)
projectConfigOnlyConstrained f s = fmap (\x -> s{T.projectConfigOnlyConstrained = x}) (f (T.projectConfigOnlyConstrained s))
{-# INLINEABLE projectConfigOnlyConstrained #-}

projectConfigPreferOldest :: Lens' ProjectConfigShared (Flag PreferOldest)
projectConfigPreferOldest f s = fmap (\x -> s{T.projectConfigPreferOldest = x}) (f (T.projectConfigPreferOldest s))
{-# INLINEABLE projectConfigPreferOldest #-}

projectConfigProgPathExtra :: Lens' ProjectConfigShared (NubList FilePath)
projectConfigProgPathExtra f s = fmap (\x -> s{T.projectConfigProgPathExtra = x}) (f (T.projectConfigProgPathExtra s))
{-# INLINEABLE projectConfigProgPathExtra #-}

projectConfigMultiRepl :: Lens' ProjectConfigShared (Flag Bool)
projectConfigMultiRepl f s = fmap (\x -> s{T.projectConfigMultiRepl = x}) (f (T.projectConfigMultiRepl s))
{-# INLINEABLE projectConfigMultiRepl #-}

packageConfigProgramPathExtra :: Lens' PackageConfig (NubList FilePath)
packageConfigProgramPathExtra f s = fmap (\x -> s{T.packageConfigProgramPathExtra = x}) (f (T.packageConfigProgramPathExtra s))
{-# INLINEABLE packageConfigProgramPathExtra #-}

packageConfigFlagAssignment :: Lens' PackageConfig (FlagAssignment)
packageConfigFlagAssignment f s = fmap (\x -> s{T.packageConfigFlagAssignment = x}) (f (T.packageConfigFlagAssignment s))
{-# INLINEABLE packageConfigFlagAssignment #-}

packageConfigVanillaLib :: Lens' PackageConfig (Flag Bool)
packageConfigVanillaLib f s = fmap (\x -> s{T.packageConfigVanillaLib = x}) (f (T.packageConfigVanillaLib s))
{-# INLINEABLE packageConfigVanillaLib #-}

packageConfigSharedLib :: Lens' PackageConfig (Flag Bool)
packageConfigSharedLib f s = fmap (\x -> s{T.packageConfigSharedLib = x}) (f (T.packageConfigSharedLib s))
{-# INLINEABLE packageConfigSharedLib #-}

packageConfigStaticLib :: Lens' PackageConfig (Flag Bool)
packageConfigStaticLib f s = fmap (\x -> s{T.packageConfigStaticLib = x}) (f (T.packageConfigStaticLib s))
{-# INLINEABLE packageConfigStaticLib #-}

packageConfigDynExe :: Lens' PackageConfig (Flag Bool)
packageConfigDynExe f s = fmap (\x -> s{T.packageConfigDynExe = x}) (f (T.packageConfigDynExe s))
{-# INLINEABLE packageConfigDynExe #-}

packageConfigFullyStaticExe :: Lens' PackageConfig (Flag Bool)
packageConfigFullyStaticExe f s = fmap (\x -> s{T.packageConfigFullyStaticExe = x}) (f (T.packageConfigFullyStaticExe s))
{-# INLINEABLE packageConfigFullyStaticExe #-}

packageConfigProf :: Lens' PackageConfig (Flag Bool)
packageConfigProf f s = fmap (\x -> s{T.packageConfigProf = x}) (f (T.packageConfigProf s))
{-# INLINEABLE packageConfigProf #-}

packageConfigProfLib :: Lens' PackageConfig (Flag Bool)
packageConfigProfLib f s = fmap (\x -> s{T.packageConfigProfLib = x}) (f (T.packageConfigProfLib s))
{-# INLINEABLE packageConfigProfLib #-}

packageConfigProfExe :: Lens' PackageConfig (Flag Bool)
packageConfigProfExe f s = fmap (\x -> s{T.packageConfigProfExe = x}) (f (T.packageConfigProfExe s))
{-# INLINEABLE packageConfigProfExe #-}

packageConfigProfDetail :: Lens' PackageConfig (Flag ProfDetailLevel)
packageConfigProfDetail f s = fmap (\x -> s{T.packageConfigProfDetail = x}) (f (T.packageConfigProfDetail s))
{-# INLINEABLE packageConfigProfDetail #-}

packageConfigProfLibDetail :: Lens' PackageConfig (Flag ProfDetailLevel)
packageConfigProfLibDetail f s = fmap (\x -> s{T.packageConfigProfLibDetail = x}) (f (T.packageConfigProfLibDetail s))
{-# INLINEABLE packageConfigProfLibDetail #-}

packageConfigConfigureArgs :: Lens' PackageConfig [String]
packageConfigConfigureArgs f s = fmap (\x -> s{T.packageConfigConfigureArgs = x}) (f (T.packageConfigConfigureArgs s))
{-# INLINEABLE packageConfigConfigureArgs #-}

packageConfigOptimization :: Lens' PackageConfig (Flag OptimisationLevel)
packageConfigOptimization f s = fmap (\x -> s{T.packageConfigOptimization = x}) (f (T.packageConfigOptimization s))
{-# INLINEABLE packageConfigOptimization #-}

packageConfigProgPrefix :: Lens' PackageConfig (Flag PathTemplate)
packageConfigProgPrefix f s = fmap (\x -> s{T.packageConfigProgPrefix = x}) (f (T.packageConfigProgPrefix s))
{-# INLINEABLE packageConfigProgPrefix #-}

packageConfigProgSuffix :: Lens' PackageConfig (Flag PathTemplate)
packageConfigProgSuffix f s = fmap (\x -> s{T.packageConfigProgSuffix = x}) (f (T.packageConfigProgSuffix s))
{-# INLINEABLE packageConfigProgSuffix #-}

packageConfigExtraLibDirs :: Lens' PackageConfig [FilePath]
packageConfigExtraLibDirs f s = fmap (\x -> s{T.packageConfigExtraLibDirs = x}) (f (T.packageConfigExtraLibDirs s))
{-# INLINEABLE packageConfigExtraLibDirs #-}

packageConfigExtraLibDirsStatic :: Lens' PackageConfig [FilePath]
packageConfigExtraLibDirsStatic f s = fmap (\x -> s{T.packageConfigExtraLibDirsStatic = x}) (f (T.packageConfigExtraLibDirsStatic s))
{-# INLINEABLE packageConfigExtraLibDirsStatic #-}

packageConfigExtraFrameworkDirs :: Lens' PackageConfig [FilePath]
packageConfigExtraFrameworkDirs f s = fmap (\x -> s{T.packageConfigExtraFrameworkDirs = x}) (f (T.packageConfigExtraFrameworkDirs s))
{-# INLINEABLE packageConfigExtraFrameworkDirs #-}

packageConfigExtraIncludeDirs :: Lens' PackageConfig [FilePath]
packageConfigExtraIncludeDirs f s = fmap (\x -> s{T.packageConfigExtraIncludeDirs = x}) (f (T.packageConfigExtraIncludeDirs s))
{-# INLINEABLE packageConfigExtraIncludeDirs #-}

packageConfigGHCiLib :: Lens' PackageConfig (Flag Bool)
packageConfigGHCiLib f s = fmap (\x -> s{T.packageConfigGHCiLib = x}) (f (T.packageConfigGHCiLib s))
{-# INLINEABLE packageConfigGHCiLib #-}

packageConfigSplitSections :: Lens' PackageConfig (Flag Bool)
packageConfigSplitSections f s = fmap (\x -> s{T.packageConfigSplitSections = x}) (f (T.packageConfigSplitSections s))
{-# INLINEABLE packageConfigSplitSections #-}

packageConfigSplitObjs :: Lens' PackageConfig (Flag Bool)
packageConfigSplitObjs f s = fmap (\x -> s{T.packageConfigSplitObjs = x}) (f (T.packageConfigSplitObjs s))
{-# INLINEABLE packageConfigSplitObjs #-}

packageConfigStripExes :: Lens' PackageConfig (Flag Bool)
packageConfigStripExes f s = fmap (\x -> s{T.packageConfigStripExes = x}) (f (T.packageConfigStripExes s))
{-# INLINEABLE packageConfigStripExes #-}

packageConfigStripLibs :: Lens' PackageConfig (Flag Bool)
packageConfigStripLibs f s = fmap (\x -> s{T.packageConfigStripLibs = x}) (f (T.packageConfigStripLibs s))
{-# INLINEABLE packageConfigStripLibs #-}

packageConfigTests :: Lens' PackageConfig (Flag Bool)
packageConfigTests f s = fmap (\x -> s{T.packageConfigTests = x}) (f (T.packageConfigTests s))
{-# INLINEABLE packageConfigTests #-}

packageConfigBenchmarks :: Lens' PackageConfig (Flag Bool)
packageConfigBenchmarks f s = fmap (\x -> s{T.packageConfigBenchmarks = x}) (f (T.packageConfigBenchmarks s))
{-# INLINEABLE packageConfigBenchmarks #-}

packageConfigCoverage :: Lens' PackageConfig (Flag Bool)
packageConfigCoverage f s = fmap (\x -> s{T.packageConfigCoverage = x}) (f (T.packageConfigCoverage s))
{-# INLINEABLE packageConfigCoverage #-}

packageConfigRelocatable :: Lens' PackageConfig (Flag Bool)
packageConfigRelocatable f s = fmap (\x -> s{T.packageConfigRelocatable = x}) (f (T.packageConfigRelocatable s))
{-# INLINEABLE packageConfigRelocatable #-}

packageConfigDebugInfo :: Lens' PackageConfig (Flag DebugInfoLevel)
packageConfigDebugInfo f s = fmap (\x -> s{T.packageConfigDebugInfo = x}) (f (T.packageConfigDebugInfo s))
{-# INLINEABLE packageConfigDebugInfo #-}

packageConfigDumpBuildInfo :: Lens' PackageConfig (Flag DumpBuildInfo)
packageConfigDumpBuildInfo f s = fmap (\x -> s{T.packageConfigDumpBuildInfo = x}) (f (T.packageConfigDumpBuildInfo s))
{-# INLINEABLE packageConfigDumpBuildInfo #-}

packageConfigRunTests :: Lens' PackageConfig (Flag Bool)
packageConfigRunTests f s = fmap (\x -> s{T.packageConfigRunTests = x}) (f (T.packageConfigRunTests s))
{-# INLINEABLE packageConfigRunTests #-}

packageConfigDocumentation :: Lens' PackageConfig (Flag Bool)
packageConfigDocumentation f s = fmap (\x -> s{T.packageConfigDocumentation = x}) (f (T.packageConfigDocumentation s))
{-# INLINEABLE packageConfigDocumentation #-}

packageConfigHaddockHoogle :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockHoogle f s = fmap (\x -> s{T.packageConfigHaddockHoogle = x}) (f (T.packageConfigHaddockHoogle s))
{-# INLINEABLE packageConfigHaddockHoogle #-}

packageConfigHaddockHtml :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockHtml f s = fmap (\x -> s{T.packageConfigHaddockHtml = x}) (f (T.packageConfigHaddockHtml s))
{-# INLINEABLE packageConfigHaddockHtml #-}

packageConfigHaddockHtmlLocation :: Lens' PackageConfig (Flag String)
packageConfigHaddockHtmlLocation f s = fmap (\x -> s{T.packageConfigHaddockHtmlLocation = x}) (f (T.packageConfigHaddockHtmlLocation s))
{-# INLINEABLE packageConfigHaddockHtmlLocation #-}

packageConfigHaddockForeignLibs :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockForeignLibs f s = fmap (\x -> s{T.packageConfigHaddockForeignLibs = x}) (f (T.packageConfigHaddockForeignLibs s))
{-# INLINEABLE packageConfigHaddockForeignLibs #-}

packageConfigHaddockExecutables :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockExecutables f s = fmap (\x -> s{T.packageConfigHaddockExecutables = x}) (f (T.packageConfigHaddockExecutables s))
{-# INLINEABLE packageConfigHaddockExecutables #-}

packageConfigHaddockTestSuites :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockTestSuites f s = fmap (\x -> s{T.packageConfigHaddockTestSuites = x}) (f (T.packageConfigHaddockTestSuites s))
{-# INLINEABLE packageConfigHaddockTestSuites #-}

packageConfigHaddockBenchmarks :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockBenchmarks f s = fmap (\x -> s{T.packageConfigHaddockBenchmarks = x}) (f (T.packageConfigHaddockBenchmarks s))
{-# INLINEABLE packageConfigHaddockBenchmarks #-}

packageConfigHaddockInternal :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockInternal f s = fmap (\x -> s{T.packageConfigHaddockInternal = x}) (f (T.packageConfigHaddockInternal s))
{-# INLINEABLE packageConfigHaddockInternal #-}

packageConfigHaddockCss :: Lens' PackageConfig (Flag FilePath)
packageConfigHaddockCss f s = fmap (\x -> s{T.packageConfigHaddockCss = x}) (f (T.packageConfigHaddockCss s))
{-# INLINEABLE packageConfigHaddockCss #-}

packageConfigHaddockLinkedSource :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockLinkedSource f s = fmap (\x -> s{T.packageConfigHaddockLinkedSource = x}) (f (T.packageConfigHaddockLinkedSource s))
{-# INLINEABLE packageConfigHaddockLinkedSource #-}

packageConfigHaddockQuickJump :: Lens' PackageConfig (Flag Bool)
packageConfigHaddockQuickJump f s = fmap (\x -> s{T.packageConfigHaddockQuickJump = x}) (f (T.packageConfigHaddockQuickJump s))
{-# INLINEABLE packageConfigHaddockQuickJump #-}

packageConfigHaddockHscolourCss :: Lens' PackageConfig (Flag FilePath)
packageConfigHaddockHscolourCss f s = fmap (\x -> s{T.packageConfigHaddockHscolourCss = x}) (f (T.packageConfigHaddockHscolourCss s))
{-# INLINEABLE packageConfigHaddockHscolourCss #-}

packageConfigHaddockContents :: Lens' PackageConfig (Flag PathTemplate)
packageConfigHaddockContents f s = fmap (\x -> s{T.packageConfigHaddockContents = x}) (f (T.packageConfigHaddockContents s))
{-# INLINEABLE packageConfigHaddockContents #-}

packageConfigHaddockIndex :: Lens' PackageConfig (Flag PathTemplate)
packageConfigHaddockIndex f s = fmap (\x -> s{T.packageConfigHaddockIndex = x}) (f (T.packageConfigHaddockIndex s))
{-# INLINEABLE packageConfigHaddockIndex #-}

packageConfigHaddockBaseUrl :: Lens' PackageConfig (Flag String)
packageConfigHaddockBaseUrl f s = fmap (\x -> s{T.packageConfigHaddockBaseUrl = x}) (f (T.packageConfigHaddockBaseUrl s))
{-# INLINEABLE packageConfigHaddockBaseUrl #-}

packageConfigHaddockResourcesDir :: Lens' PackageConfig (Flag String)
packageConfigHaddockResourcesDir f s = fmap (\x -> s{T.packageConfigHaddockResourcesDir = x}) (f (T.packageConfigHaddockResourcesDir s))
{-# INLINEABLE packageConfigHaddockResourcesDir #-}

packageConfigHaddockOutputDir :: Lens' PackageConfig (Flag FilePath)
packageConfigHaddockOutputDir f s = fmap (\x -> s{T.packageConfigHaddockOutputDir = x}) (f (T.packageConfigHaddockOutputDir s))
{-# INLINEABLE packageConfigHaddockOutputDir #-}

packageConfigHaddockForHackage :: Lens' PackageConfig (Flag HaddockTarget)
packageConfigHaddockForHackage f s = fmap (\x -> s{T.packageConfigHaddockForHackage = x}) (f (T.packageConfigHaddockForHackage s))
{-# INLINEABLE packageConfigHaddockForHackage #-}

packageConfigTestHumanLog :: Lens' PackageConfig (Flag PathTemplate)
packageConfigTestHumanLog f s = fmap (\x -> s{T.packageConfigTestHumanLog = x}) (f (T.packageConfigTestHumanLog s))
{-# INLINEABLE packageConfigTestHumanLog #-}

packageConfigTestMachineLog :: Lens' PackageConfig (Flag PathTemplate)
packageConfigTestMachineLog f s = fmap (\x -> s{T.packageConfigTestMachineLog = x}) (f (T.packageConfigTestMachineLog s))
{-# INLINEABLE packageConfigTestMachineLog #-}

packageConfigTestShowDetails :: Lens' PackageConfig (Flag TestShowDetails)
packageConfigTestShowDetails f s = fmap (\x -> s{T.packageConfigTestShowDetails = x}) (f (T.packageConfigTestShowDetails s))
{-# INLINEABLE packageConfigTestShowDetails #-}

packageConfigTestKeepTix :: Lens' PackageConfig (Flag Bool)
packageConfigTestKeepTix f s = fmap (\x -> s{T.packageConfigTestKeepTix = x}) (f (T.packageConfigTestKeepTix s))
{-# INLINEABLE packageConfigTestKeepTix #-}

packageConfigTestWrapper :: Lens' PackageConfig (Flag FilePath)
packageConfigTestWrapper f s = fmap (\x -> s{T.packageConfigTestWrapper = x}) (f (T.packageConfigTestWrapper s))
{-# INLINEABLE packageConfigTestWrapper #-}

packageConfigTestFailWhenNoTestSuites :: Lens' PackageConfig (Flag Bool)
packageConfigTestFailWhenNoTestSuites f s = fmap (\x -> s{T.packageConfigTestFailWhenNoTestSuites = x}) (f (T.packageConfigTestFailWhenNoTestSuites s))
{-# INLINEABLE packageConfigTestFailWhenNoTestSuites #-}

packageConfigTestTestOptions :: Lens' PackageConfig [PathTemplate]
packageConfigTestTestOptions f s = fmap (\x -> s{T.packageConfigTestTestOptions = x}) (f (T.packageConfigTestTestOptions s))
{-# INLINEABLE packageConfigTestTestOptions #-}

packageConfigBenchmarkOptions :: Lens' PackageConfig [PathTemplate]
packageConfigBenchmarkOptions f s = fmap (\x -> s{T.packageConfigBenchmarkOptions = x}) (f (T.packageConfigBenchmarkOptions s))
{-# INLINEABLE packageConfigBenchmarkOptions #-}
