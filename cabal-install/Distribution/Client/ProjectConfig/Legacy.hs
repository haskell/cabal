{-# LANGUAGE RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable,
             ExistentialQuantification, ScopedTypeVariables #-}

-- | Handling project configuration, types and reading.
--
module Distribution.Client.ProjectConfig.Legacy (

    -- * Project config in terms of legacy types
    LegacyProjectConfig,
    parseLegacyProjectConfig,
    showLegacyProjectConfig,

    -- * Conversion to and from legacy config types
    commandLineFlagsToProjectConfig,
    convertLegacyProjectConfig,
    convertLegacyGlobalConfig,
    convertToLegacyProjectConfig,
  ) where

import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Dependency.Types
         ( ConstraintSource(..) )
import Distribution.Client.Config
         ( SavedConfig(..) )

import Distribution.Package
import Distribution.PackageDescription
         ( SourceRepo(..), RepoKind(..) )
import Distribution.PackageDescription.Parse
         ( sourceRepoFieldDescrs )
import Distribution.Simple.Compiler
         ( OptimisationLevel(..), DebugInfoLevel(..) )
import Distribution.Simple.Setup
         ( Flag(Flag), toFlag, flagToMaybe, fromFlagOrDefault
         , ConfigFlags(..), configureOptions
         , HaddockFlags(..), haddockOptions, defaultHaddockFlags
         , programConfigurationPaths', splitArgs )
import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand
         , ConfigExFlags(..), configureExOptions, defaultConfigExFlags
         , InstallFlags(..), installOptions, defaultInstallFlags )
import Distribution.Simple.Program
         ( programName, knownPrograms )
import Distribution.Simple.Program.Db
         ( ProgramDb, defaultProgramDb )
import Distribution.Simple.Utils
         ( lowercase )

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
         ( ParseResult(..), PError(..), syntaxError, PWarning(..)
         , simpleField, commaNewLineListField )
import Distribution.Client.ParseUtils
import Distribution.Simple.Command
         ( CommandUI(commandOptions), ShowOrParseArgs(..)
         , OptionField, option, reqArg' )

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isSpace)
import Distribution.Compat.Semigroup


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
       legacyConfigureShFlags  :: ConfigFlags,
       legacyConfigureExFlags  :: ConfigExFlags,
       legacyInstallFlags      :: InstallFlags
     }

instance Monoid LegacySharedConfig where
  mempty  = LegacySharedConfig mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup LegacySharedConfig where
  a <> b =
    LegacySharedConfig {
      legacyGlobalFlags        = combine legacyGlobalFlags,
      legacyConfigureShFlags   = combine legacyConfigureShFlags,
      legacyConfigureExFlags   = combine legacyConfigureExFlags,
      legacyInstallFlags       = combine legacyInstallFlags
    }
    where combine field = field a `mappend` field b


------------------------------------------------------------------
-- Converting from and to the legacy types
--

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
    legacySharedConfig = LegacySharedConfig globalFlags configShFlags
                                            configExFlags installFlags,
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
                            globalFlags (configFlags <> configShFlags)
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
      configRelocatable         = projectConfigRelocatable,
      configAllowNewer          = projectConfigAllowNewer'
    } = configFlags
    projectConfigAllowNewer     = maybe mempty toFlag projectConfigAllowNewer'

    ConfigExFlags {
      configCabalVersion        = projectConfigCabalVersion,
      configExConstraints       = projectConfigConstraints,
      configPreferences         = projectConfigPreferences,
      configSolver              = projectConfigSolver
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
      configExtraFrameworkDirs  = packageConfigExtraFrameworkDirs,
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
      legacyConfigureShFlags = configFlags,
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

    configFlags = mempty {
      configAllowNewer    = flagToMaybe projectConfigAllowNewer
    }

    configExFlags = ConfigExFlags {
      configCabalVersion  = projectConfigCabalVersion,
      configExConstraints = projectConfigConstraints,
      configPreferences   = projectConfigPreferences,
      configSolver        = projectConfigSolver
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
      configPrograms_           = mempty,
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
      configExtraFrameworkDirs  = mempty,
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
      configDebugInfo           = mempty,
      configAllowNewer          = mempty
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
      configPrograms_           = configPrograms_ mempty,
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
      configExtraFrameworkDirs  = packageConfigExtraFrameworkDirs,
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
      configDebugInfo           = packageConfigDebugInfo,
      configAllowNewer          = mempty
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
      legacyConfigureShFlags
      (\flags conf -> conf { legacyConfigureShFlags = flags })
  . filterFields ["allow-newer"]
  . commandOptionsToFields
  ) (configureOptions ParseArgs)
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
      [ "cabal-lib-version", "solver"
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

