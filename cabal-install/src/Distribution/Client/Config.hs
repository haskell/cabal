{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Config
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling saved state such as known packages, known servers and
-- downloaded packages.
module Distribution.Client.Config
  ( SavedConfig (..)
  , loadConfig
  , getConfigFilePath
  , showConfig
  , showConfigWithComments
  , parseConfig
  , defaultConfigFile
  , defaultCacheDir
  , defaultScriptBuildsDir
  , defaultStoreDir
  , defaultCompiler
  , defaultInstallPath
  , defaultLogsDir
  , defaultReportsDir
  , defaultUserInstall
  , baseSavedConfig
  , commentSavedConfig
  , initialSavedConfig
  , configFieldDescriptions
  , haddockFlagsFields
  , installDirsFields
  , withProgramsFields
  , withProgramOptionsFields
  , userConfigDiff
  , userConfigUpdate
  , createDefaultConfigFile
  , remoteRepoFields
  , postProcessRepo
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Environment (lookupEnv)
import Prelude ()

import Language.Haskell.Extension (Language (Haskell2010))

import Distribution.Deprecated.ViewAsFieldDescr
  ( viewAsFieldDescr
  )

import Distribution.Client.BuildReports.Types
  ( ReportLevel (..)
  )
import Distribution.Client.CmdInstall.ClientInstallFlags
  ( ClientInstallFlags (..)
  , clientInstallOptions
  , defaultClientInstallFlags
  )
import qualified Distribution.Client.Init.Defaults as IT
import qualified Distribution.Client.Init.Types as IT
  ( InitFlags (..)
  )
import Distribution.Client.Setup
  ( ConfigExFlags (..)
  , GlobalFlags (..)
  , InstallFlags (..)
  , ReportFlags (..)
  , UploadFlags (..)
  , configureExOptions
  , defaultConfigExFlags
  , defaultGlobalFlags
  , defaultInstallFlags
  , globalCommand
  , initOptions
  , installOptions
  , reportCommand
  , uploadCommand
  )
import Distribution.Client.Types
  ( AllowNewer (..)
  , AllowOlder (..)
  , LocalRepo (..)
  , RelaxDeps (..)
  , RemoteRepo (..)
  , RepoName (..)
  , emptyRemoteRepo
  , isRelaxDeps
  , unRepoName
  )
import Distribution.Client.Types.Credentials (Password (..), Username (..))
import Distribution.Utils.NubList
  ( NubList
  , fromNubList
  , overNubList
  , toNubList
  )

import Distribution.Client.HttpUtils
  ( isOldHackageURI
  )
import Distribution.Client.ParseUtils
  ( parseFields
  , ppFields
  , ppSection
  )
import Distribution.Client.ProjectFlags (ProjectFlags (..))
import Distribution.Client.Version
  ( cabalInstallVersion
  )
import qualified Distribution.Compat.CharParsing as P
import Distribution.Compiler
  ( CompilerFlavor (..)
  , defaultCompilerFlavor
  )
import Distribution.Deprecated.ParseUtils
  ( FieldDescr (..)
  , PError (..)
  , PWarning (..)
  , ParseResult (..)
  , liftField
  , lineNo
  , listField
  , listFieldParsec
  , locatedErrorMsg
  , parseOptCommaList
  , parseTokenQ
  , readFields
  , runP
  , showPWarning
  , simpleField
  , simpleFieldParsec
  , spaceListField
  , syntaxError
  , warning
  )
import qualified Distribution.Deprecated.ParseUtils as ParseUtils
  ( Field (..)
  )
import Distribution.Simple.Command
  ( CommandUI (commandOptions)
  , ShowOrParseArgs (..)
  , commandDefaultFlags
  )
import Distribution.Simple.Compiler
  ( DebugInfoLevel (..)
  , OptimisationLevel (..)
  )
import Distribution.Simple.InstallDirs
  ( InstallDirs (..)
  , PathTemplate
  , defaultInstallDirs
  , toPathTemplate
  )
import Distribution.Simple.Program
  ( defaultProgramDb
  )
import Distribution.Simple.Setup
  ( BenchmarkFlags (..)
  , ConfigFlags (..)
  , Flag (..)
  , HaddockFlags (..)
  , TestFlags (..)
  , configureOptions
  , defaultBenchmarkFlags
  , defaultConfigFlags
  , defaultHaddockFlags
  , defaultTestFlags
  , flagToMaybe
  , fromFlagOrDefault
  , haddockOptions
  , installDirsOptions
  , optionDistPref
  , programDbOptions
  , programDbPaths'
  , toFlag
  )
import Distribution.Simple.Utils
  ( cabalVersion
  , die'
  , lowercase
  , notice
  , toUTF8BS
  , warn
  )
import Distribution.Solver.Types.ConstraintSource
import Distribution.Verbosity
  ( normal
  )

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Distribution.Client.ReplFlags
import Distribution.Compat.Environment
  ( getEnvironment
  )
import Distribution.Parsec (ParsecParser, parsecFilePath, parsecOptCommaList, parsecToken)
import Network.URI
  ( URI (..)
  , URIAuth (..)
  , parseURI
  )
import System.Directory
  ( XdgDirectory (XdgCache, XdgConfig, XdgState)
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getAppUserDataDirectory
  , getHomeDirectory
  , getXdgDirectory
  , renameFile
  )
import System.FilePath
  ( takeDirectory
  , (<.>)
  , (</>)
  )
import System.IO.Error
  ( isDoesNotExistError
  )
import Text.PrettyPrint
  ( ($+$)
  )
import qualified Text.PrettyPrint as Disp
  ( empty
  , render
  , text
  )
import Text.PrettyPrint.HughesPJ
  ( Doc
  , text
  )

--

-- * Configuration saved in the config file

--

data SavedConfig = SavedConfig
  { savedGlobalFlags :: GlobalFlags
  , savedInitFlags :: IT.InitFlags
  , savedInstallFlags :: InstallFlags
  , savedClientInstallFlags :: ClientInstallFlags
  , savedConfigureFlags :: ConfigFlags
  , savedConfigureExFlags :: ConfigExFlags
  , savedUserInstallDirs :: InstallDirs (Flag PathTemplate)
  , savedGlobalInstallDirs :: InstallDirs (Flag PathTemplate)
  , savedUploadFlags :: UploadFlags
  , savedReportFlags :: ReportFlags
  , savedHaddockFlags :: HaddockFlags
  , savedTestFlags :: TestFlags
  , savedBenchmarkFlags :: BenchmarkFlags
  , savedProjectFlags :: ProjectFlags
  , savedReplMulti :: Flag Bool
  }
  deriving (Generic)

instance Monoid SavedConfig where
  mempty = gmempty
  mappend = (<>)

instance Semigroup SavedConfig where
  a <> b =
    SavedConfig
      { savedGlobalFlags = combinedSavedGlobalFlags
      , savedInitFlags = combinedSavedInitFlags
      , savedInstallFlags = combinedSavedInstallFlags
      , savedClientInstallFlags = combinedSavedClientInstallFlags
      , savedConfigureFlags = combinedSavedConfigureFlags
      , savedConfigureExFlags = combinedSavedConfigureExFlags
      , savedUserInstallDirs = combinedSavedUserInstallDirs
      , savedGlobalInstallDirs = combinedSavedGlobalInstallDirs
      , savedUploadFlags = combinedSavedUploadFlags
      , savedReportFlags = combinedSavedReportFlags
      , savedHaddockFlags = combinedSavedHaddockFlags
      , savedTestFlags = combinedSavedTestFlags
      , savedBenchmarkFlags = combinedSavedBenchmarkFlags
      , savedProjectFlags = combinedSavedProjectFlags
      , savedReplMulti = combinedSavedReplMulti
      }
    where
      -- This is ugly, but necessary. If we're mappending two config files, we
      -- want the values of the *non-empty* list fields from the second one to
      -- \*override* the corresponding values from the first one. Default
      -- behaviour (concatenation) is confusing and makes some use cases (see
      -- #1884) impossible.
      --
      -- However, we also want to allow specifying multiple values for a list
      -- field in a *single* config file. For example, we want the following to
      -- continue to work:
      --
      -- remote-repo: hackage.haskell.org:http://hackage.haskell.org/
      -- remote-repo: private-collection:http://hackage.local/
      --
      -- So we can't just wrap the list fields inside Flags; we have to do some
      -- special-casing just for SavedConfig.

      -- NB: the signature prevents us from using 'combine' on lists.
      combine' :: (SavedConfig -> flags) -> (flags -> Flag a) -> Flag a
      combine' field subfield =
        (subfield . field $ a) `mappend` (subfield . field $ b)

      combineMonoid
        :: Monoid mon
        => (SavedConfig -> flags)
        -> (flags -> mon)
        -> mon
      combineMonoid field subfield =
        (subfield . field $ a) `mappend` (subfield . field $ b)

      lastNonEmpty' :: (SavedConfig -> flags) -> (flags -> [a]) -> [a]
      lastNonEmpty' field subfield =
        let a' = subfield . field $ a
            b' = subfield . field $ b
         in case b' of
              [] -> a'
              _ -> b'

      lastNonMempty'
        :: (Eq a, Monoid a) => (SavedConfig -> flags) -> (flags -> a) -> a
      lastNonMempty' field subfield =
        let a' = subfield . field $ a
            b' = subfield . field $ b
         in if b' == mempty then a' else b'

      lastNonEmptyNL'
        :: (SavedConfig -> flags)
        -> (flags -> NubList a)
        -> NubList a
      lastNonEmptyNL' field subfield =
        let a' = subfield . field $ a
            b' = subfield . field $ b
         in case fromNubList b' of
              [] -> a'
              _ -> b'

      combinedSavedGlobalFlags =
        GlobalFlags
          { globalVersion = combine globalVersion
          , globalNumericVersion = combine globalNumericVersion
          , globalConfigFile = combine globalConfigFile
          , globalConstraintsFile = combine globalConstraintsFile
          , globalRemoteRepos = lastNonEmptyNL globalRemoteRepos
          , globalCacheDir = combine globalCacheDir
          , globalLocalNoIndexRepos = lastNonEmptyNL globalLocalNoIndexRepos
          , globalActiveRepos = combine globalActiveRepos
          , globalLogsDir = combine globalLogsDir
          , globalIgnoreExpiry = combine globalIgnoreExpiry
          , globalHttpTransport = combine globalHttpTransport
          , globalNix = combine globalNix
          , globalStoreDir = combine globalStoreDir
          , globalProgPathExtra = lastNonEmptyNL globalProgPathExtra
          }
        where
          combine = combine' savedGlobalFlags
          lastNonEmptyNL = lastNonEmptyNL' savedGlobalFlags

      combinedSavedInitFlags =
        IT.InitFlags
          { IT.applicationDirs = combineMonoid savedInitFlags IT.applicationDirs
          , IT.author = combine IT.author
          , IT.buildTools = combineMonoid savedInitFlags IT.buildTools
          , IT.cabalVersion = combine IT.cabalVersion
          , IT.category = combine IT.category
          , IT.dependencies = combineMonoid savedInitFlags IT.dependencies
          , IT.email = combine IT.email
          , IT.exposedModules = combineMonoid savedInitFlags IT.exposedModules
          , IT.extraSrc = combineMonoid savedInitFlags IT.extraSrc
          , IT.extraDoc = combineMonoid savedInitFlags IT.extraDoc
          , IT.homepage = combine IT.homepage
          , IT.initHcPath = combine IT.initHcPath
          , IT.initVerbosity = combine IT.initVerbosity
          , IT.initializeTestSuite = combine IT.initializeTestSuite
          , IT.interactive = combine IT.interactive
          , IT.language = combine IT.language
          , IT.license = combine IT.license
          , IT.mainIs = combine IT.mainIs
          , IT.minimal = combine IT.minimal
          , IT.noComments = combine IT.noComments
          , IT.otherExts = combineMonoid savedInitFlags IT.otherExts
          , IT.otherModules = combineMonoid savedInitFlags IT.otherModules
          , IT.overwrite = combine IT.overwrite
          , IT.packageDir = combine IT.packageDir
          , IT.packageName = combine IT.packageName
          , IT.packageType = combine IT.packageType
          , IT.quiet = combine IT.quiet
          , IT.simpleProject = combine IT.simpleProject
          , IT.sourceDirs = combineMonoid savedInitFlags IT.sourceDirs
          , IT.synopsis = combine IT.synopsis
          , IT.testDirs = combineMonoid savedInitFlags IT.testDirs
          , IT.version = combine IT.version
          }
        where
          combine = combine' savedInitFlags

      combinedSavedInstallFlags =
        InstallFlags
          { installDocumentation = combine installDocumentation
          , installHaddockIndex = combine installHaddockIndex
          , installDryRun = combine installDryRun
          , installOnlyDownload = combine installOnlyDownload
          , installDest = combine installDest
          , installMaxBackjumps = combine installMaxBackjumps
          , installReorderGoals = combine installReorderGoals
          , installCountConflicts = combine installCountConflicts
          , installFineGrainedConflicts = combine installFineGrainedConflicts
          , installMinimizeConflictSet = combine installMinimizeConflictSet
          , installIndependentGoals = combine installIndependentGoals
          , installPreferOldest = combine installPreferOldest
          , installShadowPkgs = combine installShadowPkgs
          , installStrongFlags = combine installStrongFlags
          , installAllowBootLibInstalls = combine installAllowBootLibInstalls
          , installOnlyConstrained = combine installOnlyConstrained
          , installReinstall = combine installReinstall
          , installAvoidReinstalls = combine installAvoidReinstalls
          , installOverrideReinstall = combine installOverrideReinstall
          , installUpgradeDeps = combine installUpgradeDeps
          , installOnly = combine installOnly
          , installOnlyDeps = combine installOnlyDeps
          , installIndexState = combine installIndexState
          , installRootCmd = combine installRootCmd
          , installSummaryFile = lastNonEmptyNL installSummaryFile
          , installLogFile = combine installLogFile
          , installBuildReports = combine installBuildReports
          , installReportPlanningFailure = combine installReportPlanningFailure
          , installSymlinkBinDir = combine installSymlinkBinDir
          , installPerComponent = combine installPerComponent
          , installNumJobs = combine installNumJobs
          , installKeepGoing = combine installKeepGoing
          , installRunTests = combine installRunTests
          , installOfflineMode = combine installOfflineMode
          }
        where
          combine = combine' savedInstallFlags
          lastNonEmptyNL = lastNonEmptyNL' savedInstallFlags

      combinedSavedClientInstallFlags =
        ClientInstallFlags
          { cinstInstallLibs = combine cinstInstallLibs
          , cinstEnvironmentPath = combine cinstEnvironmentPath
          , cinstOverwritePolicy = combine cinstOverwritePolicy
          , cinstInstallMethod = combine cinstInstallMethod
          , cinstInstalldir = combine cinstInstalldir
          }
        where
          combine = combine' savedClientInstallFlags

      combinedSavedConfigureFlags =
        ConfigFlags
          { configArgs = lastNonEmpty configArgs
          , configPrograms_ = configPrograms_ . savedConfigureFlags $ b
          , -- TODO: NubListify
            configProgramPaths = lastNonEmpty configProgramPaths
          , -- TODO: NubListify
            configProgramArgs = lastNonEmpty configProgramArgs
          , configProgramPathExtra = lastNonEmptyNL configProgramPathExtra
          , configInstantiateWith = lastNonEmpty configInstantiateWith
          , configHcFlavor = combine configHcFlavor
          , configHcPath = combine configHcPath
          , configHcPkg = combine configHcPkg
          , configVanillaLib = combine configVanillaLib
          , configProfLib = combine configProfLib
          , configProf = combine configProf
          , configSharedLib = combine configSharedLib
          , configStaticLib = combine configStaticLib
          , configDynExe = combine configDynExe
          , configFullyStaticExe = combine configFullyStaticExe
          , configProfExe = combine configProfExe
          , configProfDetail = combine configProfDetail
          , configProfLibDetail = combine configProfLibDetail
          , -- TODO: NubListify
            configConfigureArgs = lastNonEmpty configConfigureArgs
          , configOptimization = combine configOptimization
          , configDebugInfo = combine configDebugInfo
          , configProgPrefix = combine configProgPrefix
          , configProgSuffix = combine configProgSuffix
          , -- Parametrised by (Flag PathTemplate), so safe to use 'mappend'.
            configInstallDirs =
              (configInstallDirs . savedConfigureFlags $ a)
                `mappend` (configInstallDirs . savedConfigureFlags $ b)
          , configScratchDir = combine configScratchDir
          , -- TODO: NubListify
            configExtraLibDirs = lastNonEmpty configExtraLibDirs
          , configExtraLibDirsStatic = lastNonEmpty configExtraLibDirsStatic
          , -- TODO: NubListify
            configExtraFrameworkDirs = lastNonEmpty configExtraFrameworkDirs
          , -- TODO: NubListify
            configExtraIncludeDirs = lastNonEmpty configExtraIncludeDirs
          , configDeterministic = combine configDeterministic
          , configIPID = combine configIPID
          , configCID = combine configCID
          , configDistPref = combine configDistPref
          , configCabalFilePath = combine configCabalFilePath
          , configVerbosity = combine configVerbosity
          , configUserInstall = combine configUserInstall
          , -- TODO: NubListify
            configPackageDBs = lastNonEmpty configPackageDBs
          , configGHCiLib = combine configGHCiLib
          , configSplitSections = combine configSplitSections
          , configSplitObjs = combine configSplitObjs
          , configStripExes = combine configStripExes
          , configStripLibs = combine configStripLibs
          , -- TODO: NubListify
            configConstraints = lastNonEmpty configConstraints
          , -- TODO: NubListify
            configDependencies = lastNonEmpty configDependencies
          , configPromisedDependencies = lastNonEmpty configPromisedDependencies
          , -- TODO: NubListify
            configConfigurationsFlags = lastNonMempty configConfigurationsFlags
          , configTests = combine configTests
          , configBenchmarks = combine configBenchmarks
          , configCoverage = combine configCoverage
          , configLibCoverage = combine configLibCoverage
          , configExactConfiguration = combine configExactConfiguration
          , configFlagError = combine configFlagError
          , configRelocatable = combine configRelocatable
          , configUseResponseFiles = combine configUseResponseFiles
          , configDumpBuildInfo = combine configDumpBuildInfo
          , configAllowDependingOnPrivateLibs =
              combine configAllowDependingOnPrivateLibs
          }
        where
          combine = combine' savedConfigureFlags
          lastNonEmpty = lastNonEmpty' savedConfigureFlags
          lastNonEmptyNL = lastNonEmptyNL' savedConfigureFlags
          lastNonMempty = lastNonMempty' savedConfigureFlags

      combinedSavedConfigureExFlags =
        ConfigExFlags
          { configCabalVersion = combine configCabalVersion
          , configAppend = combine configAppend
          , configBackup = combine configBackup
          , -- TODO: NubListify
            configExConstraints = lastNonEmpty configExConstraints
          , -- TODO: NubListify
            configPreferences = lastNonEmpty configPreferences
          , configSolver = combine configSolver
          , configAllowNewer =
              combineMonoid savedConfigureExFlags configAllowNewer
          , configAllowOlder =
              combineMonoid savedConfigureExFlags configAllowOlder
          , configWriteGhcEnvironmentFilesPolicy =
              combine configWriteGhcEnvironmentFilesPolicy
          }
        where
          combine = combine' savedConfigureExFlags
          lastNonEmpty = lastNonEmpty' savedConfigureExFlags

      -- Parametrised by (Flag PathTemplate), so safe to use 'mappend'.
      combinedSavedUserInstallDirs =
        savedUserInstallDirs a
          `mappend` savedUserInstallDirs b

      -- Parametrised by (Flag PathTemplate), so safe to use 'mappend'.
      combinedSavedGlobalInstallDirs =
        savedGlobalInstallDirs a
          `mappend` savedGlobalInstallDirs b

      combinedSavedUploadFlags =
        UploadFlags
          { uploadCandidate = combine uploadCandidate
          , uploadDoc = combine uploadDoc
          , uploadUsername = combine uploadUsername
          , uploadPassword = combine uploadPassword
          , uploadPasswordCmd = combine uploadPasswordCmd
          , uploadVerbosity = combine uploadVerbosity
          }
        where
          combine = combine' savedUploadFlags

      combinedSavedReportFlags =
        ReportFlags
          { reportUsername = combine reportUsername
          , reportPassword = combine reportPassword
          , reportVerbosity = combine reportVerbosity
          }
        where
          combine = combine' savedReportFlags

      combinedSavedHaddockFlags =
        HaddockFlags
          { -- TODO: NubListify
            haddockProgramPaths = lastNonEmpty haddockProgramPaths
          , -- TODO: NubListify
            haddockProgramArgs = lastNonEmpty haddockProgramArgs
          , haddockHoogle = combine haddockHoogle
          , haddockHtml = combine haddockHtml
          , haddockHtmlLocation = combine haddockHtmlLocation
          , haddockForHackage = combine haddockForHackage
          , haddockExecutables = combine haddockExecutables
          , haddockTestSuites = combine haddockTestSuites
          , haddockBenchmarks = combine haddockBenchmarks
          , haddockForeignLibs = combine haddockForeignLibs
          , haddockInternal = combine haddockInternal
          , haddockCss = combine haddockCss
          , haddockLinkedSource = combine haddockLinkedSource
          , haddockQuickJump = combine haddockQuickJump
          , haddockHscolourCss = combine haddockHscolourCss
          , haddockContents = combine haddockContents
          , haddockDistPref = combine haddockDistPref
          , haddockKeepTempFiles = combine haddockKeepTempFiles
          , haddockVerbosity = combine haddockVerbosity
          , haddockCabalFilePath = combine haddockCabalFilePath
          , haddockIndex = combine haddockIndex
          , haddockBaseUrl = combine haddockBaseUrl
          , haddockLib = combine haddockLib
          , haddockOutputDir = combine haddockOutputDir
          , haddockArgs = lastNonEmpty haddockArgs
          }
        where
          combine = combine' savedHaddockFlags
          lastNonEmpty = lastNonEmpty' savedHaddockFlags

      combinedSavedTestFlags =
        TestFlags
          { testDistPref = combine testDistPref
          , testVerbosity = combine testVerbosity
          , testHumanLog = combine testHumanLog
          , testMachineLog = combine testMachineLog
          , testShowDetails = combine testShowDetails
          , testKeepTix = combine testKeepTix
          , testWrapper = combine testWrapper
          , testFailWhenNoTestSuites = combine testFailWhenNoTestSuites
          , testOptions = lastNonEmpty testOptions
          }
        where
          combine = combine' savedTestFlags
          lastNonEmpty = lastNonEmpty' savedTestFlags

      combinedSavedBenchmarkFlags =
        BenchmarkFlags
          { benchmarkDistPref = combine benchmarkDistPref
          , benchmarkVerbosity = combine benchmarkVerbosity
          , benchmarkOptions = lastNonEmpty benchmarkOptions
          }
        where
          combine = combine' savedBenchmarkFlags
          lastNonEmpty = lastNonEmpty' savedBenchmarkFlags

      combinedSavedReplMulti = combine' savedReplMulti id

      combinedSavedProjectFlags =
        ProjectFlags
          { flagProjectDir = combine flagProjectDir
          , flagProjectFile = combine flagProjectFile
          , flagIgnoreProject = combine flagIgnoreProject
          }
        where
          combine = combine' savedProjectFlags

--

-- * Default config

--

-- | These are the absolute basic defaults. The fields that must be
-- initialised. When we load the config from the file we layer the loaded
-- values over these ones, so any missing fields in the file take their values
-- from here.
baseSavedConfig :: IO SavedConfig
baseSavedConfig = do
  userPrefix <- defaultInstallPrefix
  cacheDir <- defaultCacheDir
  logsDir <- defaultLogsDir
  return
    mempty
      { savedConfigureFlags =
          mempty
            { configHcFlavor = toFlag defaultCompiler
            , configUserInstall = toFlag defaultUserInstall
            , configVerbosity = toFlag normal
            }
      , savedUserInstallDirs =
          mempty
            { prefix = toFlag (toPathTemplate userPrefix)
            }
      , savedGlobalFlags =
          mempty
            { globalCacheDir = toFlag cacheDir
            , globalLogsDir = toFlag logsDir
            }
      }

-- | This is the initial configuration that we write out to the config file
-- if the file does not exist (or the config we use if the file cannot be read
-- for some other reason). When the config gets loaded it gets layered on top
-- of 'baseSavedConfig' so we do not need to include it into the initial
-- values we save into the config file.
initialSavedConfig :: IO SavedConfig
initialSavedConfig = do
  cacheDir <- defaultCacheDir
  logsDir <- defaultLogsDir
  extraPath <- defaultExtraPath
  installPath <- defaultInstallPath
  return
    mempty
      { savedGlobalFlags =
          mempty
            { globalCacheDir = toFlag cacheDir
            , globalRemoteRepos = toNubList [defaultRemoteRepo]
            }
      , savedConfigureFlags =
          mempty
            { configProgramPathExtra = toNubList extraPath
            }
      , savedInstallFlags =
          mempty
            { installSummaryFile = toNubList [toPathTemplate (logsDir </> "build.log")]
            , installBuildReports = toFlag NoReports
            , installNumJobs = toFlag Nothing
            }
      , savedClientInstallFlags =
          mempty
            { cinstInstalldir = toFlag installPath
            }
      }

-- | Issue a warning if both @$XDG_CONFIG_HOME/cabal/config@ and
-- @~/.cabal@ exists.
warnOnTwoConfigs :: Verbosity -> IO ()
warnOnTwoConfigs verbosity = do
  defaultDir <- getAppUserDataDirectory "cabal"
  dotCabalExists <- doesDirectoryExist defaultDir
  xdgCfg <- getXdgDirectory XdgConfig ("cabal" </> "config")
  xdgCfgExists <- doesFileExist xdgCfg
  when (dotCabalExists && xdgCfgExists) $
    warn verbosity $
      "Both "
        <> defaultDir
        <> " and "
        <> xdgCfg
        <> " exist - ignoring the former.\n"
        <> "It is advisable to remove one of them. In that case, we will use the remaining one by default (unless '$CABAL_DIR' is explicitly set)."

-- | If @CABAL\_DIR@ is set, return @Just@ its value. Otherwise, if
-- @~/.cabal@ exists and @$XDG_CONFIG_HOME/cabal/config@ does not
-- exist, return @Just "~/.cabal"@.  Otherwise, return @Nothing@.  If
-- this function returns Nothing, then it implies that we are not
-- using a single directory for everything, but instead use XDG paths.
-- Fundamentally, this function is used to implement transparent
-- backwards compatibility with pre-XDG versions of cabal-install.
maybeGetCabalDir :: IO (Maybe FilePath)
maybeGetCabalDir = do
  mDir <- lookupEnv "CABAL_DIR"
  case mDir of
    Just dir -> return $ Just dir
    Nothing -> do
      defaultDir <- getAppUserDataDirectory "cabal"
      dotCabalExists <- doesDirectoryExist defaultDir
      xdgCfg <- getXdgDirectory XdgConfig ("cabal" </> "config")
      xdgCfgExists <- doesFileExist xdgCfg
      if dotCabalExists && not xdgCfgExists
        then return $ Just defaultDir
        else return Nothing

-- | The default behaviour of cabal-install is to use the XDG
-- directory standard.  However, if @CABAL_DIR@ is set, we instead use
-- that directory as a single store for everything cabal-related, like
-- the old @~/.cabal@ behaviour.  Also, for backwards compatibility,
-- if @~/.cabal@ exists we treat that as equivalent to @CABAL_DIR@
-- being set.  This function abstracts that decision-making.
getDefaultDir :: XdgDirectory -> FilePath -> IO FilePath
getDefaultDir xdg subdir = do
  mDir <- maybeGetCabalDir
  case mDir of
    Just dir -> return $ dir </> subdir
    Nothing -> getXdgDirectory xdg $ "cabal" </> subdir

-- | The default prefix used for installation.
defaultInstallPrefix :: IO FilePath
defaultInstallPrefix = do
  mDir <- maybeGetCabalDir
  case mDir of
    Just dir ->
      return dir
    Nothing -> do
      dir <- getHomeDirectory
      return $ dir </> ".local"

defaultConfigFile :: IO FilePath
defaultConfigFile =
  getDefaultDir XdgConfig "config"

defaultCacheDir :: IO FilePath
defaultCacheDir =
  getDefaultDir XdgCache "packages"

defaultScriptBuildsDir :: IO FilePath
defaultScriptBuildsDir =
  getDefaultDir XdgCache "script-builds"

defaultStoreDir :: IO FilePath
defaultStoreDir =
  getDefaultDir XdgState "store"

defaultLogsDir :: IO FilePath
defaultLogsDir =
  getDefaultDir XdgCache "logs"

defaultReportsDir :: IO FilePath
defaultReportsDir =
  getDefaultDir XdgCache "reports"

defaultExtraPath :: IO [FilePath]
defaultExtraPath = do
  mDir <- maybeGetCabalDir
  case mDir of
    Just dir ->
      return [dir </> "bin"]
    Nothing -> do
      dir <- getHomeDirectory
      return [dir </> ".local" </> "bin"]

defaultInstallPath :: IO FilePath
defaultInstallPath = do
  mDir <- maybeGetCabalDir
  case mDir of
    Just dir ->
      return $ dir </> "bin"
    Nothing -> do
      dir <- getHomeDirectory
      return $ dir </> ".local" </> "bin"

defaultCompiler :: CompilerFlavor
defaultCompiler = fromMaybe GHC defaultCompilerFlavor

defaultUserInstall :: Bool
defaultUserInstall = True

-- We do per-user installs by default on all platforms. We used to default to
-- global installs on Windows but that no longer works on Windows Vista or 7.

defaultRemoteRepo :: RemoteRepo
defaultRemoteRepo = RemoteRepo name uri Nothing [] 0 False
  where
    str = "hackage.haskell.org"
    name = RepoName str
    uri = URI "http:" (Just (URIAuth "" str "")) "/" "" ""

-- Note that lots of old config files will have the old url
-- http://hackage.haskell.org/packages/archive
-- but new config files can use the new url (without the /packages/archive)
-- and avoid having to do a http redirect

-- For the default repo we know extra information, fill this in.
--
-- We need this because the 'defaultRemoteRepo' above is only used for the
-- first time when a config file is made. So for users with older config files
-- we might have only have older info. This lets us fill that in even for old
-- config files.
--
addInfoForKnownRepos :: RemoteRepo -> RemoteRepo
addInfoForKnownRepos repo
  | remoteRepoName repo == remoteRepoName defaultRemoteRepo =
      useSecure . tryHttps . fixOldURI $ repo
  where
    fixOldURI r
      | isOldHackageURI (remoteRepoURI r) =
          r{remoteRepoURI = remoteRepoURI defaultRemoteRepo}
      | otherwise = r

    tryHttps r = r{remoteRepoShouldTryHttps = True}

    useSecure
      r@RemoteRepo
        { remoteRepoSecure = secure
        , remoteRepoRootKeys = []
        , remoteRepoKeyThreshold = 0
        }
        | secure /= Just False =
            r
              { -- Use hackage-security by default unless you opt-out with
                -- secure: False
                remoteRepoSecure = Just True
              , remoteRepoRootKeys = defaultHackageRemoteRepoKeys
              , remoteRepoKeyThreshold = defaultHackageRemoteRepoKeyThreshold
              }
    useSecure r = r
addInfoForKnownRepos other = other

-- | The current hackage.haskell.org repo root keys that we ship with cabal.

---
-- This lets us bootstrap trust in this repo without user intervention.
-- These keys need to be periodically updated when new root keys are added.
-- See the root key procedures for details.
--
defaultHackageRemoteRepoKeys :: [String]
defaultHackageRemoteRepoKeys =
  -- Key owners and public keys are provided as a convenience to readers.
  -- The canonical source for this mapping data is the hackage-root-keys
  -- repository and Hackage's root.json file.
  --
  -- Links:
  --  * https://github.com/haskell-infra/hackage-root-keys
  --  * https://hackage.haskell.org/root.json
  -- Please consult root.json on Hackage to map key IDs to public keys,
  -- and the hackage-root-keys repository to map public keys to their
  -- owners.
  [ -- Adam Gundry (uRPdSiL3/MNsk50z6NB55ABo0OrrNDXigtCul4vtzmw=)
    "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
  , -- Gershom Bazerman (bYoUXXQ9TtX10UriaMiQtTccuXPGnmldP68djzZ7cLo=)
    "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
  , -- John Wiegley (zazm5w480r+zPO6Z0+8fjGuxZtb9pAuoVmQ+VkuCvgU=)
    "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
  , -- Norman Ramsey (ZI8di3a9Un0s2RBrt5GwVRvfOXVuywADfXGPZfkiDb0=)
    "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
  ]

-- | The required threshold of root key signatures for hackage.haskell.org
defaultHackageRemoteRepoKeyThreshold :: Int
defaultHackageRemoteRepoKeyThreshold = 3

--

-- * Config file reading

--

-- | Loads the main configuration, and applies additional defaults to give the
-- effective configuration. To loads just what is actually in the config file,
-- use 'loadRawConfig'.
loadConfig :: Verbosity -> Flag FilePath -> IO SavedConfig
loadConfig verbosity configFileFlag = do
  warnOnTwoConfigs verbosity
  config <- loadRawConfig verbosity configFileFlag
  extendToEffectiveConfig config

extendToEffectiveConfig :: SavedConfig -> IO SavedConfig
extendToEffectiveConfig config = do
  base <- baseSavedConfig
  let effective0 = base `mappend` config
      globalFlags0 = savedGlobalFlags effective0
      effective =
        effective0
          { savedGlobalFlags =
              globalFlags0
                { globalRemoteRepos =
                    overNubList
                      (map addInfoForKnownRepos)
                      (globalRemoteRepos globalFlags0)
                }
          }
  return effective

-- | Like 'loadConfig' but does not apply any additional defaults, it just
-- loads what is actually in the config file. This is thus suitable for
-- comparing or editing a config file, but not suitable for using as the
-- effective configuration.
loadRawConfig :: Verbosity -> Flag FilePath -> IO SavedConfig
loadRawConfig verbosity configFileFlag = do
  (source, configFile) <- getConfigFilePathAndSource configFileFlag
  minp <- readConfigFile mempty configFile
  case minp of
    Nothing -> do
      notice verbosity $
        "Config file path source is " ++ sourceMsg source ++ "."
      -- 2021-10-07, issue #7705
      -- Only create default config file if name was not given explicitly
      -- via option --config-file or environment variable.
      case source of
        Default -> do
          notice verbosity msgNotFound
          createDefaultConfigFile verbosity [] configFile
        CommandlineOption -> failNoConfigFile
        EnvironmentVariable -> failNoConfigFile
      where
        msgNotFound = unwords ["Config file not found:", configFile]
        failNoConfigFile =
          die' verbosity $
            unlines
              [ msgNotFound
              , "(Config files can be created via the cabal-command 'user-config init'.)"
              ]
    Just (ParseOk ws conf) -> do
      unless (null ws) $
        warn verbosity $
          unlines (map (showPWarning configFile) ws)
      return conf
    Just (ParseFailed err) -> do
      let (line, msg) = locatedErrorMsg err
      die' verbosity $
        "Error parsing config file "
          ++ configFile
          ++ maybe "" (\n -> ':' : show n) line
          ++ ":\n"
          ++ msg
  where
    sourceMsg CommandlineOption = "commandline option"
    sourceMsg EnvironmentVariable = "environment variable CABAL_CONFIG"
    sourceMsg Default = "default config file"

-- | Provenance of the config file.
data ConfigFileSource
  = CommandlineOption
  | EnvironmentVariable
  | Default

-- | Returns the config file path, without checking that the file exists.
-- The order of precedence is: input flag, CABAL_CONFIG, default location.
getConfigFilePath :: Flag FilePath -> IO FilePath
getConfigFilePath = fmap snd . getConfigFilePathAndSource

getConfigFilePathAndSource :: Flag FilePath -> IO (ConfigFileSource, FilePath)
getConfigFilePathAndSource configFileFlag =
  getSource sources
  where
    sources =
      [ (CommandlineOption, return . flagToMaybe $ configFileFlag)
      , (EnvironmentVariable, lookup "CABAL_CONFIG" `liftM` getEnvironment)
      , (Default, Just `liftM` defaultConfigFile)
      ]

    getSource [] = error "no config file path candidate found."
    getSource ((source, action) : xs) =
      action >>= maybe (getSource xs) (return . (,) source)

readConfigFile
  :: SavedConfig -> FilePath -> IO (Maybe (ParseResult SavedConfig))
readConfigFile initial file =
  handleNotExists $
    fmap
      (Just . parseConfig (ConstraintSourceMainConfig file) initial)
      (BS.readFile file)
  where
    handleNotExists action = catchIO action $ \ioe ->
      if isDoesNotExistError ioe
        then return Nothing
        else ioError ioe

createDefaultConfigFile :: Verbosity -> [String] -> FilePath -> IO SavedConfig
createDefaultConfigFile verbosity extraLines filePath = do
  commentConf <- commentSavedConfig
  initialConf <- initialSavedConfig
  extraConf <- parseExtraLines verbosity extraLines
  notice verbosity $ "Writing default configuration to " ++ filePath
  writeConfigFile filePath commentConf (initialConf `mappend` extraConf)
  return initialConf

writeConfigFile :: FilePath -> SavedConfig -> SavedConfig -> IO ()
writeConfigFile file comments vals = do
  let tmpFile = file <.> "tmp"
  createDirectoryIfMissing True (takeDirectory file)
  writeFile tmpFile $
    explanation ++ showConfigWithComments comments vals ++ "\n"
  renameFile tmpFile file
  where
    explanation =
      unlines
        [ "-- This is the configuration file for the 'cabal' command line tool."
        , "--"
        , "-- The available configuration options are listed below."
        , "-- Some of them have default values listed."
        , "--"
        , "-- Lines (like this one) beginning with '--' are comments."
        , "-- Be careful with spaces and indentation because they are"
        , "-- used to indicate layout for nested sections."
        , "--"
        , "-- This config file was generated using the following versions"
        , "-- of Cabal and cabal-install:"
        , "-- Cabal library version: " ++ prettyShow cabalVersion
        , "-- cabal-install version: " ++ prettyShow cabalInstallVersion
        , ""
        , ""
        ]

-- | These are the default values that get used in Cabal if a no value is
-- given. We use these here to include in comments when we write out the
-- initial config file so that the user can see what default value they are
-- overriding.
commentSavedConfig :: IO SavedConfig
commentSavedConfig = do
  userInstallDirs <- defaultInstallDirs defaultCompiler True True
  globalInstallDirs <- defaultInstallDirs defaultCompiler False True
  let conf0 =
        mempty
          { savedGlobalFlags =
              defaultGlobalFlags
                { globalRemoteRepos = toNubList [defaultRemoteRepo]
                , globalNix = mempty
                }
          , savedInitFlags =
              mempty
                { IT.interactive = toFlag False
                , IT.cabalVersion = toFlag IT.defaultCabalVersion
                , IT.language = toFlag Haskell2010
                , IT.license = NoFlag
                , IT.sourceDirs = Flag [IT.defaultSourceDir]
                , IT.applicationDirs = Flag [IT.defaultApplicationDir]
                , IT.quiet = Flag False
                , IT.noComments = Flag False
                , IT.minimal = Flag False
                , IT.simpleProject = Flag False
                }
          , savedInstallFlags = defaultInstallFlags
          , savedClientInstallFlags = defaultClientInstallFlags
          , savedConfigureExFlags =
              defaultConfigExFlags
                { configAllowNewer = Just (AllowNewer mempty)
                , configAllowOlder = Just (AllowOlder mempty)
                }
          , savedConfigureFlags =
              (defaultConfigFlags defaultProgramDb)
                { configUserInstall = toFlag defaultUserInstall
                }
          , savedUserInstallDirs = fmap toFlag userInstallDirs
          , savedGlobalInstallDirs = fmap toFlag globalInstallDirs
          , savedUploadFlags = commandDefaultFlags uploadCommand
          , savedReportFlags = commandDefaultFlags reportCommand
          , savedHaddockFlags = defaultHaddockFlags
          , savedTestFlags = defaultTestFlags
          , savedBenchmarkFlags = defaultBenchmarkFlags
          }
  conf1 <- extendToEffectiveConfig conf0
  let globalFlagsConf1 = savedGlobalFlags conf1
      conf2 =
        conf1
          { savedGlobalFlags =
              globalFlagsConf1
                { globalRemoteRepos =
                    overNubList
                      (map removeRootKeys)
                      (globalRemoteRepos globalFlagsConf1)
                }
          }
  return conf2
  where
    -- Most people don't want to see default root keys, so don't print them.
    removeRootKeys :: RemoteRepo -> RemoteRepo
    removeRootKeys r = r{remoteRepoRootKeys = []}

-- | All config file fields.
configFieldDescriptions :: ConstraintSource -> [FieldDescr SavedConfig]
configFieldDescriptions src =
  toSavedConfig
    liftGlobalFlag
    (commandOptions (globalCommand []) ParseArgs)
    ["version", "numeric-version", "config-file"]
    []
    ++ toSavedConfig
      liftConfigFlag
      (configureOptions ParseArgs)
      ( ["builddir", "constraint", "dependency", "promised-dependency", "ipid"]
          ++ map fieldName installDirsFields
      )
      -- This is only here because viewAsFieldDescr gives us a parser
      -- that only recognises 'ghc' etc, the case-sensitive flag names, not
      -- what the normal case-insensitive parser gives us.
      [ simpleFieldParsec
          "compiler"
          (fromFlagOrDefault Disp.empty . fmap pretty)
          (Flag <$> parsec <|> pure NoFlag)
          configHcFlavor
          (\v flags -> flags{configHcFlavor = v})
      , -- TODO: The following is a temporary fix. The "optimization"
        -- and "debug-info" fields are OptArg, and viewAsFieldDescr
        -- fails on that. Instead of a hand-written hackaged parser
        -- and printer, we should handle this case properly in the
        -- library.
        liftField
          configOptimization
          ( \v flags ->
              flags{configOptimization = v}
          )
          $ let name = "optimization"
             in FieldDescr
                  name
                  ( \f -> case f of
                      Flag NoOptimisation -> Disp.text "False"
                      Flag NormalOptimisation -> Disp.text "True"
                      Flag MaximumOptimisation -> Disp.text "2"
                      _ -> Disp.empty
                  )
                  ( \line str _ -> case () of
                      _
                        | str == "False" -> ParseOk [] (Flag NoOptimisation)
                        | str == "True" -> ParseOk [] (Flag NormalOptimisation)
                        | str == "0" -> ParseOk [] (Flag NoOptimisation)
                        | str == "1" -> ParseOk [] (Flag NormalOptimisation)
                        | str == "2" -> ParseOk [] (Flag MaximumOptimisation)
                        | lstr == "false" -> ParseOk [caseWarning] (Flag NoOptimisation)
                        | lstr == "true" ->
                            ParseOk
                              [caseWarning]
                              (Flag NormalOptimisation)
                        | otherwise -> ParseFailed (NoParse name line)
                        where
                          lstr = lowercase str
                          caseWarning =
                            PWarning $
                              "The '"
                                ++ name
                                ++ "' field is case sensitive, use 'True' or 'False'."
                  )
      , liftField configDebugInfo (\v flags -> flags{configDebugInfo = v}) $
          let name = "debug-info"
           in FieldDescr
                name
                ( \f -> case f of
                    Flag NoDebugInfo -> Disp.text "False"
                    Flag MinimalDebugInfo -> Disp.text "1"
                    Flag NormalDebugInfo -> Disp.text "True"
                    Flag MaximalDebugInfo -> Disp.text "3"
                    _ -> Disp.empty
                )
                ( \line str _ -> case () of
                    _
                      | str == "False" -> ParseOk [] (Flag NoDebugInfo)
                      | str == "True" -> ParseOk [] (Flag NormalDebugInfo)
                      | str == "0" -> ParseOk [] (Flag NoDebugInfo)
                      | str == "1" -> ParseOk [] (Flag MinimalDebugInfo)
                      | str == "2" -> ParseOk [] (Flag NormalDebugInfo)
                      | str == "3" -> ParseOk [] (Flag MaximalDebugInfo)
                      | lstr == "false" -> ParseOk [caseWarning] (Flag NoDebugInfo)
                      | lstr == "true" -> ParseOk [caseWarning] (Flag NormalDebugInfo)
                      | otherwise -> ParseFailed (NoParse name line)
                      where
                        lstr = lowercase str
                        caseWarning =
                          PWarning $
                            "The '"
                              ++ name
                              ++ "' field is case sensitive, use 'True' or 'False'."
                )
      ]
    ++ toSavedConfig
      liftConfigExFlag
      (configureExOptions ParseArgs src)
      []
      [ let pkgs =
              (Just . AllowOlder . RelaxDepsSome)
                `fmap` parsecOptCommaList parsec
            parseAllowOlder =
              ( (Just . AllowOlder . toRelaxDeps)
                  `fmap` parsec
              )
                <|> pkgs
         in simpleFieldParsec
              "allow-older"
              (showRelaxDeps . fmap unAllowOlder)
              parseAllowOlder
              configAllowOlder
              (\v flags -> flags{configAllowOlder = v})
      , let pkgs =
              (Just . AllowNewer . RelaxDepsSome)
                `fmap` parsecOptCommaList parsec
            parseAllowNewer =
              ( (Just . AllowNewer . toRelaxDeps)
                  `fmap` parsec
              )
                <|> pkgs
         in simpleFieldParsec
              "allow-newer"
              (showRelaxDeps . fmap unAllowNewer)
              parseAllowNewer
              configAllowNewer
              (\v flags -> flags{configAllowNewer = v})
      ]
    ++ toSavedConfig
      liftInstallFlag
      (installOptions ParseArgs)
      ["dry-run", "only", "only-dependencies", "dependencies-only"]
      []
    ++ toSavedConfig
      liftClientInstallFlag
      (clientInstallOptions ParseArgs)
      []
      []
    ++ toSavedConfig
      liftUploadFlag
      (commandOptions uploadCommand ParseArgs)
      ["verbose", "check", "documentation", "publish"]
      []
    ++ toSavedConfig
      liftReportFlag
      (commandOptions reportCommand ParseArgs)
      ["verbose", "username", "password"]
      []
    -- FIXME: this is a hack, hiding the user name and password.
    -- But otherwise it masks the upload ones. Either need to
    -- share the options or make then distinct. In any case
    -- they should probably be per-server.

    ++ toSavedConfig
      liftReplFlag
      [multiReplOption]
      []
      []
    ++ [ viewAsFieldDescr $
          optionDistPref
            (configDistPref . savedConfigureFlags)
            ( \distPref config ->
                config
                  { savedConfigureFlags =
                      (savedConfigureFlags config)
                        { configDistPref = distPref
                        }
                  , savedHaddockFlags =
                      (savedHaddockFlags config)
                        { haddockDistPref = distPref
                        }
                  }
            )
            ParseArgs
       ]
  where
    toSavedConfig lift options exclusions replacements =
      [ lift (fromMaybe field replacement)
      | opt <- options
      , let field = viewAsFieldDescr opt
            name = fieldName field
            replacement = find ((== name) . fieldName) replacements
      , name `notElem` exclusions
      ]

    showRelaxDeps Nothing = mempty
    showRelaxDeps (Just rd)
      | isRelaxDeps rd = Disp.text "True"
      | otherwise = Disp.text "False"

    toRelaxDeps True = RelaxDepsAll
    toRelaxDeps False = mempty

-- TODO: next step, make the deprecated fields elicit a warning.
--
deprecatedFieldDescriptions :: [FieldDescr SavedConfig]
deprecatedFieldDescriptions =
  [ liftGlobalFlag $
      listFieldParsec
        "repos"
        pretty
        parsec
        (fromNubList . globalRemoteRepos)
        (\rs cfg -> cfg{globalRemoteRepos = toNubList rs})
  , liftGlobalFlag $
      simpleFieldParsec
        "cachedir"
        (Disp.text . fromFlagOrDefault "")
        (optionalFlag parsecFilePath)
        globalCacheDir
        (\d cfg -> cfg{globalCacheDir = d})
  , liftUploadFlag $
      simpleFieldParsec
        "hackage-username"
        (Disp.text . fromFlagOrDefault "" . fmap unUsername)
        (optionalFlag (fmap Username parsecToken))
        uploadUsername
        (\d cfg -> cfg{uploadUsername = d})
  , liftUploadFlag $
      simpleFieldParsec
        "hackage-password"
        (Disp.text . fromFlagOrDefault "" . fmap unPassword)
        (optionalFlag (fmap Password parsecToken))
        uploadPassword
        (\d cfg -> cfg{uploadPassword = d})
  , liftUploadFlag $
      spaceListField
        "hackage-password-command"
        Disp.text
        parseTokenQ
        (fromFlagOrDefault [] . uploadPasswordCmd)
        (\d cfg -> cfg{uploadPasswordCmd = Flag d})
  ]
    ++ map
      (modifyFieldName ("user-" ++) . liftUserInstallDirs)
      installDirsFields
    ++ map
      (modifyFieldName ("global-" ++) . liftGlobalInstallDirs)
      installDirsFields
  where
    optionalFlag :: ParsecParser a -> ParsecParser (Flag a)
    optionalFlag p = toFlag <$> p <|> pure mempty

    modifyFieldName :: (String -> String) -> FieldDescr a -> FieldDescr a
    modifyFieldName f d = d{fieldName = f (fieldName d)}

liftUserInstallDirs
  :: FieldDescr (InstallDirs (Flag PathTemplate))
  -> FieldDescr SavedConfig
liftUserInstallDirs =
  liftField
    savedUserInstallDirs
    (\flags conf -> conf{savedUserInstallDirs = flags})

liftGlobalInstallDirs
  :: FieldDescr (InstallDirs (Flag PathTemplate))
  -> FieldDescr SavedConfig
liftGlobalInstallDirs =
  liftField
    savedGlobalInstallDirs
    (\flags conf -> conf{savedGlobalInstallDirs = flags})

liftGlobalFlag :: FieldDescr GlobalFlags -> FieldDescr SavedConfig
liftGlobalFlag =
  liftField
    savedGlobalFlags
    (\flags conf -> conf{savedGlobalFlags = flags})

liftConfigFlag :: FieldDescr ConfigFlags -> FieldDescr SavedConfig
liftConfigFlag =
  liftField
    savedConfigureFlags
    (\flags conf -> conf{savedConfigureFlags = flags})

liftConfigExFlag :: FieldDescr ConfigExFlags -> FieldDescr SavedConfig
liftConfigExFlag =
  liftField
    savedConfigureExFlags
    (\flags conf -> conf{savedConfigureExFlags = flags})

liftInstallFlag :: FieldDescr InstallFlags -> FieldDescr SavedConfig
liftInstallFlag =
  liftField
    savedInstallFlags
    (\flags conf -> conf{savedInstallFlags = flags})

liftClientInstallFlag :: FieldDescr ClientInstallFlags -> FieldDescr SavedConfig
liftClientInstallFlag =
  liftField
    savedClientInstallFlags
    (\flags conf -> conf{savedClientInstallFlags = flags})

liftUploadFlag :: FieldDescr UploadFlags -> FieldDescr SavedConfig
liftUploadFlag =
  liftField
    savedUploadFlags
    (\flags conf -> conf{savedUploadFlags = flags})

liftReportFlag :: FieldDescr ReportFlags -> FieldDescr SavedConfig
liftReportFlag =
  liftField
    savedReportFlags
    (\flags conf -> conf{savedReportFlags = flags})

liftReplFlag :: FieldDescr (Flag Bool) -> FieldDescr SavedConfig
liftReplFlag =
  liftField
    savedReplMulti
    (\flags conf -> conf{savedReplMulti = flags})

parseConfig
  :: ConstraintSource
  -> SavedConfig
  -> BS.ByteString
  -> ParseResult SavedConfig
parseConfig src initial = \str -> do
  fields <- readFields str
  let (knownSections, others) = partition isKnownSection fields
  config <- parse others
  let init0 = savedInitFlags config
      user0 = savedUserInstallDirs config
      global0 = savedGlobalInstallDirs config
  (remoteRepoSections0, localRepoSections0, haddockFlags, initFlags, user, global, paths, args) <-
    foldM
      parseSections
      ([], [], savedHaddockFlags config, init0, user0, global0, [], [])
      knownSections

  let remoteRepoSections =
        reverse
          . nubBy ((==) `on` remoteRepoName)
          $ remoteRepoSections0

  let localRepoSections =
        reverse
          . nubBy ((==) `on` localRepoName)
          $ localRepoSections0

  return . fixConfigMultilines $
    config
      { savedGlobalFlags =
          (savedGlobalFlags config)
            { globalRemoteRepos = toNubList remoteRepoSections
            , globalLocalNoIndexRepos = toNubList localRepoSections
            , -- the global extra prog path comes from the configure flag prog path
              globalProgPathExtra = configProgramPathExtra (savedConfigureFlags config)
            }
      , savedConfigureFlags =
          (savedConfigureFlags config)
            { configProgramPaths = paths
            , configProgramArgs = args
            }
      , savedHaddockFlags = haddockFlags
      , savedInitFlags = initFlags
      , savedUserInstallDirs = user
      , savedGlobalInstallDirs = global
      }
  where
    isKnownSection (ParseUtils.Section _ "repository" _ _) = True
    isKnownSection (ParseUtils.F _ "remote-repo" _) = True
    isKnownSection (ParseUtils.Section _ "haddock" _ _) = True
    isKnownSection (ParseUtils.Section _ "init" _ _) = True
    isKnownSection (ParseUtils.Section _ "install-dirs" _ _) = True
    isKnownSection (ParseUtils.Section _ "program-locations" _ _) = True
    isKnownSection (ParseUtils.Section _ "program-default-options" _ _) = True
    isKnownSection _ = False

    -- Attempt to split fields that can represent lists of paths into
    -- actual lists on failure, leave the field untouched.
    splitMultiPath :: [String] -> [String]
    splitMultiPath [s] = case runP 0 "" (parseOptCommaList parseTokenQ) s of
      ParseOk _ res -> res
      _ -> [s]
    splitMultiPath xs = xs

    -- This is a fixup, pending a full config parser rewrite, to
    -- ensure that config fields which can be comma-separated lists
    -- actually parse as comma-separated lists.
    fixConfigMultilines conf =
      conf
        { savedConfigureFlags =
            let scf = savedConfigureFlags conf
             in scf
                  { configProgramPathExtra =
                      toNubList $
                        splitMultiPath
                          (fromNubList $ configProgramPathExtra scf)
                  , configExtraLibDirs =
                      splitMultiPath
                        (configExtraLibDirs scf)
                  , configExtraLibDirsStatic =
                      splitMultiPath
                        (configExtraLibDirsStatic scf)
                  , configExtraFrameworkDirs =
                      splitMultiPath
                        (configExtraFrameworkDirs scf)
                  , configExtraIncludeDirs =
                      splitMultiPath
                        (configExtraIncludeDirs scf)
                  , configConfigureArgs =
                      splitMultiPath
                        (configConfigureArgs scf)
                  }
        }

    parse =
      parseFields
        ( configFieldDescriptions src
            ++ deprecatedFieldDescriptions
        )
        initial

    parseSections
      (rs, ls, h, i, u, g, p, a)
      (ParseUtils.Section lineno "repository" name fs) = do
        name' <-
          maybe (ParseFailed $ NoParse "repository name" lineno) return $
            simpleParsec name
        r' <- parseFields remoteRepoFields (emptyRemoteRepo name') fs
        r'' <- postProcessRepo lineno name r'
        case r'' of
          Left local -> return (rs, local : ls, h, i, u, g, p, a)
          Right remote -> return (remote : rs, ls, h, i, u, g, p, a)
    parseSections
      (rs, ls, h, i, u, g, p, a)
      (ParseUtils.F lno "remote-repo" raw) = do
        let mr' = simpleParsec raw
        r' <- maybe (ParseFailed $ NoParse "remote-repo" lno) return mr'
        return (r' : rs, ls, h, i, u, g, p, a)
    parseSections
      accum@(rs, ls, h, i, u, g, p, a)
      (ParseUtils.Section _ "haddock" name fs)
        | name == "" = do
            h' <- parseFields haddockFlagsFields h fs
            return (rs, ls, h', i, u, g, p, a)
        | otherwise = do
            warning "The 'haddock' section should be unnamed"
            return accum
    parseSections
      accum@(rs, ls, h, i, u, g, p, a)
      (ParseUtils.Section _ "init" name fs)
        | name == "" = do
            i' <- parseFields initFlagsFields i fs
            return (rs, ls, h, i', u, g, p, a)
        | otherwise = do
            warning "The 'init' section should be unnamed"
            return accum
    parseSections
      accum@(rs, ls, h, i, u, g, p, a)
      (ParseUtils.Section _ "install-dirs" name fs)
        | name' == "user" = do
            u' <- parseFields installDirsFields u fs
            return (rs, ls, h, i, u', g, p, a)
        | name' == "global" = do
            g' <- parseFields installDirsFields g fs
            return (rs, ls, h, i, u, g', p, a)
        | otherwise = do
            warning "The 'install-paths' section should be for 'user' or 'global'"
            return accum
        where
          name' = lowercase name
    parseSections
      accum@(rs, ls, h, i, u, g, p, a)
      (ParseUtils.Section _ "program-locations" name fs)
        | name == "" = do
            p' <- parseFields withProgramsFields p fs
            return (rs, ls, h, i, u, g, p', a)
        | otherwise = do
            warning "The 'program-locations' section should be unnamed"
            return accum
    parseSections
      accum@(rs, ls, h, i, u, g, p, a)
      (ParseUtils.Section _ "program-default-options" name fs)
        | name == "" = do
            a' <- parseFields withProgramOptionsFields a fs
            return (rs, ls, h, i, u, g, p, a')
        | otherwise = do
            warning "The 'program-default-options' section should be unnamed"
            return accum
    parseSections accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

postProcessRepo :: Int -> String -> RemoteRepo -> ParseResult (Either LocalRepo RemoteRepo)
postProcessRepo lineno reponameStr repo0 = do
  when (null reponameStr) $
    syntaxError lineno $
      "a 'repository' section requires the "
        ++ "repository name as an argument"

  reponame <-
    maybe (fail $ "Invalid repository name " ++ reponameStr) return $
      simpleParsec reponameStr

  case uriScheme (remoteRepoURI repo0) of
    -- TODO: check that there are no authority, query or fragment
    -- Note: the trailing colon is important
    "file+noindex:" -> do
      let uri = remoteRepoURI repo0
      return $ Left $ LocalRepo reponame (uriPath uri) (uriFragment uri == "#shared-cache")
    _ -> do
      let repo = repo0{remoteRepoName = reponame}

      when (remoteRepoKeyThreshold repo > length (remoteRepoRootKeys repo)) $
        warning $
          "'key-threshold' for repository "
            ++ show (remoteRepoName repo)
            ++ " higher than number of keys"

      when (not (null (remoteRepoRootKeys repo)) && remoteRepoSecure repo /= Just True) $
        warning $
          "'root-keys' for repository "
            ++ show (remoteRepoName repo)
            ++ " non-empty, but 'secure' not set to True."

      return $ Right repo

showConfig :: SavedConfig -> String
showConfig = showConfigWithComments mempty

showConfigWithComments :: SavedConfig -> SavedConfig -> String
showConfigWithComments comment vals =
  Disp.render $
    case fmap
      (uncurry ppRemoteRepoSection)
      (zip (getRemoteRepos comment) (getRemoteRepos vals)) of
      [] -> Disp.text ""
      (x : xs) -> foldl' (\r r' -> r $+$ Disp.text "" $+$ r') x xs
      $+$ Disp.text ""
      $+$ ppFields
        (skipSomeFields (configFieldDescriptions ConstraintSourceUnknown))
        mcomment
        vals
      $+$ Disp.text ""
      $+$ ppSection
        "haddock"
        ""
        haddockFlagsFields
        (fmap savedHaddockFlags mcomment)
        (savedHaddockFlags vals)
      $+$ Disp.text ""
      $+$ ppSection
        "init"
        ""
        initFlagsFields
        (fmap savedInitFlags mcomment)
        (savedInitFlags vals)
      $+$ Disp.text ""
      $+$ installDirsSection "user" savedUserInstallDirs
      $+$ Disp.text ""
      $+$ installDirsSection "global" savedGlobalInstallDirs
      $+$ Disp.text ""
      $+$ configFlagsSection
        "program-locations"
        withProgramsFields
        configProgramPaths
      $+$ Disp.text ""
      $+$ configFlagsSection
        "program-default-options"
        withProgramOptionsFields
        configProgramArgs
  where
    getRemoteRepos = fromNubList . globalRemoteRepos . savedGlobalFlags
    mcomment = Just comment
    installDirsSection name field =
      ppSection
        "install-dirs"
        name
        installDirsFields
        (fmap field mcomment)
        (field vals)
    configFlagsSection name fields field =
      ppSection
        name
        ""
        fields
        (fmap (field . savedConfigureFlags) mcomment)
        ((field . savedConfigureFlags) vals)

    -- skip fields based on field name.  currently only skips "remote-repo",
    -- because that is rendered as a section.  (see 'ppRemoteRepoSection'.)
    skipSomeFields = filter ((/= "remote-repo") . fieldName)

-- | Fields for the 'install-dirs' sections.
installDirsFields :: [FieldDescr (InstallDirs (Flag PathTemplate))]
installDirsFields = map viewAsFieldDescr installDirsOptions

ppRemoteRepoSection :: RemoteRepo -> RemoteRepo -> Doc
ppRemoteRepoSection def vals =
  ppSection
    "repository"
    (unRepoName (remoteRepoName vals))
    remoteRepoFields
    (Just def)
    vals

remoteRepoFields :: [FieldDescr RemoteRepo]
remoteRepoFields =
  [ simpleField
      "url"
      (text . show)
      (parseTokenQ >>= parseURI')
      remoteRepoURI
      (\x repo -> repo{remoteRepoURI = x})
  , simpleFieldParsec
      "secure"
      showSecure
      (Just `fmap` parsec)
      remoteRepoSecure
      (\x repo -> repo{remoteRepoSecure = x})
  , listField
      "root-keys"
      text
      parseTokenQ
      remoteRepoRootKeys
      (\x repo -> repo{remoteRepoRootKeys = x})
  , simpleFieldParsec
      "key-threshold"
      showThreshold
      P.integral
      remoteRepoKeyThreshold
      (\x repo -> repo{remoteRepoKeyThreshold = x})
  ]
  where
    parseURI' uriString =
      case parseURI uriString of
        Nothing -> fail $ "remote-repo: no parse on " ++ show uriString
        Just uri -> return uri

    showSecure Nothing = mempty -- default 'secure' setting
    showSecure (Just True) = text "True" -- user explicitly enabled it
    showSecure (Just False) = text "False" -- user explicitly disabled it

    -- If the key-threshold is set to 0, we omit it as this is the default
    -- and it looks odd to have a value for key-threshold but not for 'secure'
    -- (note that an empty list of keys is already omitted by default, since
    -- that is what we do for all list fields)
    showThreshold 0 = mempty
    showThreshold t = text (show t)

-- | Fields for the 'haddock' section.
haddockFlagsFields :: [FieldDescr HaddockFlags]
haddockFlagsFields =
  [ field
  | opt <- haddockOptions ParseArgs
  , let field = viewAsFieldDescr opt
        name = fieldName field
  , name `notElem` exclusions
  ]
  where
    exclusions = ["verbose", "builddir", "for-hackage"]

-- | Fields for the 'init' section.
initFlagsFields :: [FieldDescr IT.InitFlags]
initFlagsFields =
  [ field
  | opt <- initOptions ParseArgs
  , let field = viewAsFieldDescr opt
        name = fieldName field
  , name `notElem` exclusions
  ]
  where
    exclusions =
      [ "author"
      , "email"
      , "overwrite"
      , "package-dir"
      , "packagedir"
      , "package-name"
      , "version"
      , "homepage"
      , "synopsis"
      , "category"
      , "extra-source-file"
      , "lib"
      , "exe"
      , "libandexe"
      , "main-is"
      , "expose-module"
      , "exposed-modules"
      , "extension"
      , "dependency"
      , "build-tool"
      , "with-compiler"
      , "verbose"
      ]

-- | Fields for the 'program-locations' section.
withProgramsFields :: [FieldDescr [(String, FilePath)]]
withProgramsFields =
  map viewAsFieldDescr $
    programDbPaths'
      (++ "-location")
      defaultProgramDb
      ParseArgs
      id
      (++)

-- | Fields for the 'program-default-options' section.
withProgramOptionsFields :: [FieldDescr [(String, [String])]]
withProgramOptionsFields =
  map viewAsFieldDescr $
    programDbOptions defaultProgramDb ParseArgs id (++)

parseExtraLines :: Verbosity -> [String] -> IO SavedConfig
parseExtraLines verbosity extraLines =
  case parseConfig
    (ConstraintSourceMainConfig "additional lines")
    mempty
    (toUTF8BS (unlines extraLines)) of
    ParseFailed err ->
      let (line, msg) = locatedErrorMsg err
       in die' verbosity $
            "Error parsing additional config lines\n"
              ++ maybe "" (\n -> ':' : show n) line
              ++ ":\n"
              ++ msg
    ParseOk [] r -> return r
    ParseOk ws _ ->
      die' verbosity $
        unlines (map (showPWarning "Error parsing additional config lines") ws)

-- | Get the differences (as a pseudo code diff) between the user's
-- config file and the one that cabal would generate if it didn't exist.
userConfigDiff :: Verbosity -> GlobalFlags -> [String] -> IO [String]
userConfigDiff verbosity globalFlags extraLines = do
  userConfig <- loadRawConfig normal (globalConfigFile globalFlags)
  extraConfig <- parseExtraLines verbosity extraLines
  testConfig <- initialSavedConfig
  return $
    reverse . foldl' createDiff [] . M.toList $
      M.unionWith
        combine
        (M.fromList . map justFst $ filterShow testConfig)
        (M.fromList . map justSnd $ filterShow (userConfig `mappend` extraConfig))
  where
    justFst (a, b) = (a, (Just b, Nothing))
    justSnd (a, b) = (a, (Nothing, Just b))

    combine (Nothing, Just b) (Just a, Nothing) = (Just a, Just b)
    combine (Just a, Nothing) (Nothing, Just b) = (Just a, Just b)
    combine x y =
      error $
        "Can't happen : userConfigDiff "
          ++ show x
          ++ " "
          ++ show y

    createDiff :: [String] -> (String, (Maybe String, Maybe String)) -> [String]
    createDiff acc (key, (Just a, Just b))
      | a == b = acc
      | otherwise =
          ("+ " ++ key ++ ": " ++ b)
            : ("- " ++ key ++ ": " ++ a)
            : acc
    createDiff acc (key, (Nothing, Just b)) = ("+ " ++ key ++ ": " ++ b) : acc
    createDiff acc (key, (Just a, Nothing)) = ("- " ++ key ++ ": " ++ a) : acc
    createDiff acc (_, (Nothing, Nothing)) = acc

    filterShow :: SavedConfig -> [(String, String)]
    filterShow cfg =
      map keyValueSplit
        . filter (\s -> not (null s) && ':' `elem` s)
        . map nonComment
        . lines
        $ showConfig cfg

    nonComment [] = []
    nonComment ('-' : '-' : _) = []
    nonComment (x : xs) = x : nonComment xs

    topAndTail = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    keyValueSplit s =
      let (left, right) = break (== ':') s
       in (topAndTail left, topAndTail (drop 1 right))

-- | Update the user's config file keeping the user's customizations.
userConfigUpdate :: Verbosity -> GlobalFlags -> [String] -> IO ()
userConfigUpdate verbosity globalFlags extraLines = do
  userConfig <- loadRawConfig normal (globalConfigFile globalFlags)
  extraConfig <- parseExtraLines verbosity extraLines
  newConfig <- initialSavedConfig
  commentConf <- commentSavedConfig
  cabalFile <- getConfigFilePath $ globalConfigFile globalFlags
  let backup = cabalFile ++ ".backup"
  notice verbosity $ "Renaming " ++ cabalFile ++ " to " ++ backup ++ "."
  renameFile cabalFile backup
  notice verbosity $ "Writing merged config to " ++ cabalFile ++ "."
  writeConfigFile
    cabalFile
    commentConf
    (newConfig `mappend` userConfig `mappend` extraConfig)
