{-# LANGUAGE CPP #-}

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
-----------------------------------------------------------------------------
module Distribution.Client.Config (
    SavedConfig(..),
    loadConfig,

    showConfig,
    showConfigWithComments,
    parseConfig,

    defaultCabalDir,
    defaultConfigFile,
    defaultCacheDir,
    defaultCompiler,
    defaultLogsDir,
    defaultUserInstall,

    baseSavedConfig,
    commentSavedConfig,
    initialSavedConfig,
    configFieldDescriptions,
    haddockFlagsFields,
    installDirsFields,
    withProgramsFields,
    withProgramOptionsFields,
    userConfigDiff,
    userConfigUpdate
  ) where

import Distribution.Client.Types
         ( RemoteRepo(..), Username(..), Password(..), emptyRemoteRepo )
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand, defaultGlobalFlags
         , ConfigExFlags(..), configureExOptions, defaultConfigExFlags
         , InstallFlags(..), installOptions, defaultInstallFlags
         , UploadFlags(..), uploadCommand
         , ReportFlags(..), reportCommand
         , showRepo, parseRepo, readRepo )
import Distribution.Utils.NubList
         ( NubList, fromNubList, toNubList)

import Distribution.Simple.Compiler
         ( DebugInfoLevel(..), OptimisationLevel(..) )
import Distribution.Simple.Setup
         ( ConfigFlags(..), configureOptions, defaultConfigFlags
         , HaddockFlags(..), haddockOptions, defaultHaddockFlags
         , installDirsOptions, optionDistPref
         , programConfigurationPaths', programConfigurationOptions
         , Flag(..), toFlag, flagToMaybe, fromFlagOrDefault )
import Distribution.Simple.InstallDirs
         ( InstallDirs(..), defaultInstallDirs
         , PathTemplate, toPathTemplate )
import Distribution.ParseUtils
         ( FieldDescr(..), liftField
         , ParseResult(..), PError(..), PWarning(..)
         , locatedErrorMsg, showPWarning
         , readFields, warning, lineNo
         , simpleField, listField, spaceListField
         , parseFilePathQ, parseTokenQ )
import Distribution.Client.ParseUtils
         ( parseFields, ppFields, ppSection )
import qualified Distribution.ParseUtils as ParseUtils
         ( Field(..) )
import qualified Distribution.Text as Text
         ( Text(..) )
import Distribution.Simple.Command
         ( CommandUI(commandOptions), commandDefaultFlags, ShowOrParseArgs(..)
         , viewAsFieldDescr )
import Distribution.Simple.Program
         ( defaultProgramConfiguration )
import Distribution.Simple.Utils
         ( die, notice, warn, lowercase, cabalVersion )
import Distribution.Compiler
         ( CompilerFlavor(..), defaultCompilerFlavor )
import Distribution.Verbosity
         ( Verbosity, normal )

import Data.List
         ( partition, find, foldl' )
import Data.Maybe
         ( fromMaybe )
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
         ( Monoid(..) )
#endif
import Control.Monad
         ( unless, foldM, liftM, liftM2 )
import qualified Distribution.Compat.ReadP as Parse
         ( option )
import qualified Text.PrettyPrint as Disp
         ( render, text, empty )
import Text.PrettyPrint
         ( ($+$) )
import Text.PrettyPrint.HughesPJ
         ( text, Doc )
import System.Directory
         ( createDirectoryIfMissing, getAppUserDataDirectory, renameFile )
import Network.URI
         ( URI(..), URIAuth(..), parseURI )
import System.FilePath
         ( (<.>), (</>), takeDirectory )
import System.IO.Error
         ( isDoesNotExistError )
import Distribution.Compat.Environment
         ( getEnvironment )
import Distribution.Compat.Exception
         ( catchIO )
import qualified Paths_cabal_install
         ( version )
import Data.Version
         ( showVersion )
import Data.Char
         ( isSpace )
import qualified Data.Map as M
import Data.Function
         ( on )
import Data.List
         ( nubBy )

--
-- * Configuration saved in the config file
--

data SavedConfig = SavedConfig {
    savedGlobalFlags       :: GlobalFlags,
    savedInstallFlags      :: InstallFlags,
    savedConfigureFlags    :: ConfigFlags,
    savedConfigureExFlags  :: ConfigExFlags,
    savedUserInstallDirs   :: InstallDirs (Flag PathTemplate),
    savedGlobalInstallDirs :: InstallDirs (Flag PathTemplate),
    savedUploadFlags       :: UploadFlags,
    savedReportFlags       :: ReportFlags,
    savedHaddockFlags      :: HaddockFlags
  }

instance Monoid SavedConfig where
  mempty = SavedConfig {
    savedGlobalFlags       = mempty,
    savedInstallFlags      = mempty,
    savedConfigureFlags    = mempty,
    savedConfigureExFlags  = mempty,
    savedUserInstallDirs   = mempty,
    savedGlobalInstallDirs = mempty,
    savedUploadFlags       = mempty,
    savedReportFlags       = mempty,
    savedHaddockFlags      = mempty
  }
  mappend a b = SavedConfig {
    savedGlobalFlags       = combinedSavedGlobalFlags,
    savedInstallFlags      = combinedSavedInstallFlags,
    savedConfigureFlags    = combinedSavedConfigureFlags,
    savedConfigureExFlags  = combinedSavedConfigureExFlags,
    savedUserInstallDirs   = combinedSavedUserInstallDirs,
    savedGlobalInstallDirs = combinedSavedGlobalInstallDirs,
    savedUploadFlags       = combinedSavedUploadFlags,
    savedReportFlags       = combinedSavedReportFlags,
    savedHaddockFlags      = combinedSavedHaddockFlags
  }
    where
      -- This is ugly, but necessary. If we're mappending two config files, we
      -- want the values of the *non-empty* list fields from the second one to
      -- *override* the corresponding values from the first one. Default
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
      combine'        field subfield =
        (subfield . field $ a) `mappend` (subfield . field $ b)

      lastNonEmpty' :: (SavedConfig -> flags) -> (flags -> [a]) -> [a]
      lastNonEmpty'   field subfield =
        let a' = subfield . field $ a
            b' = subfield . field $ b
        in case b' of [] -> a'
                      _  -> b'

      lastNonEmptyNL' :: (SavedConfig -> flags) -> (flags -> NubList a)
                      -> NubList a
      lastNonEmptyNL' field subfield =
        let a' = subfield . field $ a
            b' = subfield . field $ b
        in case fromNubList b' of [] -> a'
                                  _  -> b'

      combinedSavedGlobalFlags = GlobalFlags {
        globalVersion           = combine globalVersion,
        globalNumericVersion    = combine globalNumericVersion,
        globalConfigFile        = combine globalConfigFile,
        globalSandboxConfigFile = combine globalSandboxConfigFile,
        globalRemoteRepos       = lastNonEmptyNL globalRemoteRepos,
        globalCacheDir          = combine globalCacheDir,
        globalLocalRepos        = lastNonEmptyNL globalLocalRepos,
        globalLogsDir           = combine globalLogsDir,
        globalWorldFile         = combine globalWorldFile,
        globalRequireSandbox    = combine globalRequireSandbox,
        globalIgnoreSandbox     = combine globalIgnoreSandbox,
        globalHttpTransport     = combine globalHttpTransport
        }
        where
          combine        = combine'        savedGlobalFlags
          lastNonEmptyNL = lastNonEmptyNL' savedGlobalFlags

      combinedSavedInstallFlags = InstallFlags {
        installDocumentation         = combine installDocumentation,
        installHaddockIndex          = combine installHaddockIndex,
        installDryRun                = combine installDryRun,
        installMaxBackjumps          = combine installMaxBackjumps,
        installReorderGoals          = combine installReorderGoals,
        installIndependentGoals      = combine installIndependentGoals,
        installShadowPkgs            = combine installShadowPkgs,
        installStrongFlags           = combine installStrongFlags,
        installReinstall             = combine installReinstall,
        installAvoidReinstalls       = combine installAvoidReinstalls,
        installOverrideReinstall     = combine installOverrideReinstall,
        installUpgradeDeps           = combine installUpgradeDeps,
        installOnly                  = combine installOnly,
        installOnlyDeps              = combine installOnlyDeps,
        installRootCmd               = combine installRootCmd,
        installSummaryFile           = lastNonEmptyNL installSummaryFile,
        installLogFile               = combine installLogFile,
        installBuildReports          = combine installBuildReports,
        installReportPlanningFailure = combine installReportPlanningFailure,
        installSymlinkBinDir         = combine installSymlinkBinDir,
        installOneShot               = combine installOneShot,
        installNumJobs               = combine installNumJobs,
        installRunTests              = combine installRunTests,
        installOfflineMode           = combine installOfflineMode
        }
        where
          combine        = combine'        savedInstallFlags
          lastNonEmptyNL = lastNonEmptyNL' savedInstallFlags

      combinedSavedConfigureFlags = ConfigFlags {
        configPrograms            = configPrograms . savedConfigureFlags $ b,
        -- TODO: NubListify
        configProgramPaths        = lastNonEmpty configProgramPaths,
        -- TODO: NubListify
        configProgramArgs         = lastNonEmpty configProgramArgs,
        configProgramPathExtra    = lastNonEmptyNL configProgramPathExtra,
        configHcFlavor            = combine configHcFlavor,
        configHcPath              = combine configHcPath,
        configHcPkg               = combine configHcPkg,
        configVanillaLib          = combine configVanillaLib,
        configProfLib             = combine configProfLib,
        configProf                = combine configProf,
        configSharedLib           = combine configSharedLib,
        configDynExe              = combine configDynExe,
        configProfExe             = combine configProfExe,
        configProfDetail          = combine configProfDetail,
        configProfLibDetail       = combine configProfLibDetail,
        -- TODO: NubListify
        configConfigureArgs       = lastNonEmpty configConfigureArgs,
        configOptimization        = combine configOptimization,
        configDebugInfo           = combine configDebugInfo,
        configProgPrefix          = combine configProgPrefix,
        configProgSuffix          = combine configProgSuffix,
        -- Parametrised by (Flag PathTemplate), so safe to use 'mappend'.
        configInstallDirs         =
          (configInstallDirs . savedConfigureFlags $ a)
          `mappend` (configInstallDirs . savedConfigureFlags $ b),
        configScratchDir          = combine configScratchDir,
        -- TODO: NubListify
        configExtraLibDirs        = lastNonEmpty configExtraLibDirs,
        -- TODO: NubListify
        configExtraIncludeDirs    = lastNonEmpty configExtraIncludeDirs,
        configDistPref            = combine configDistPref,
        configVerbosity           = combine configVerbosity,
        configUserInstall         = combine configUserInstall,
        -- TODO: NubListify
        configPackageDBs          = lastNonEmpty configPackageDBs,
        configView                = combine configView,
        configGHCiLib             = combine configGHCiLib,
        configSplitObjs           = combine configSplitObjs,
        configStripExes           = combine configStripExes,
        configStripLibs           = combine configStripLibs,
        -- TODO: NubListify
        configConstraints         = lastNonEmpty configConstraints,
        -- TODO: NubListify
        configDependencies        = lastNonEmpty configDependencies,
        configInstantiateWith     = lastNonEmpty configInstantiateWith,
        -- TODO: NubListify
        configConfigurationsFlags = lastNonEmpty configConfigurationsFlags,
        configTests               = combine configTests,
        configBenchmarks          = combine configBenchmarks,
        configCoverage            = combine configCoverage,
        configLibCoverage         = combine configLibCoverage,
        configExactConfiguration  = combine configExactConfiguration,
        configFlagError           = combine configFlagError,
        configRelocatable         = combine configRelocatable
        }
        where
          combine        = combine'        savedConfigureFlags
          lastNonEmpty   = lastNonEmpty'   savedConfigureFlags
          lastNonEmptyNL = lastNonEmptyNL' savedConfigureFlags

      combinedSavedConfigureExFlags = ConfigExFlags {
        configCabalVersion  = combine configCabalVersion,
        -- TODO: NubListify
        configExConstraints = lastNonEmpty configExConstraints,
        -- TODO: NubListify
        configPreferences   = lastNonEmpty configPreferences,
        configSolver        = combine configSolver,
        configAllowNewer    = combine configAllowNewer
        }
        where
          combine      = combine' savedConfigureExFlags
          lastNonEmpty = lastNonEmpty' savedConfigureExFlags

      -- Parametrised by (Flag PathTemplate), so safe to use 'mappend'.
      combinedSavedUserInstallDirs = savedUserInstallDirs a
                                     `mappend` savedUserInstallDirs b

      -- Parametrised by (Flag PathTemplate), so safe to use 'mappend'.
      combinedSavedGlobalInstallDirs = savedGlobalInstallDirs a
                                       `mappend` savedGlobalInstallDirs b

      combinedSavedUploadFlags = UploadFlags {
        uploadCheck       = combine uploadCheck,
        uploadUsername    = combine uploadUsername,
        uploadPassword    = combine uploadPassword,
        uploadPasswordCmd = combine uploadPasswordCmd,
        uploadVerbosity   = combine uploadVerbosity
        }
        where
          combine = combine' savedUploadFlags

      combinedSavedReportFlags = ReportFlags {
        reportUsername  = combine reportUsername,
        reportPassword  = combine reportPassword,
        reportVerbosity = combine reportVerbosity
        }
        where
          combine = combine' savedReportFlags

      combinedSavedHaddockFlags = HaddockFlags {
        -- TODO: NubListify
        haddockProgramPaths  = lastNonEmpty haddockProgramPaths,
        -- TODO: NubListify
        haddockProgramArgs   = lastNonEmpty haddockProgramArgs,
        haddockHoogle        = combine haddockHoogle,
        haddockHtml          = combine haddockHtml,
        haddockHtmlLocation  = combine haddockHtmlLocation,
        haddockExecutables   = combine haddockExecutables,
        haddockTestSuites    = combine haddockTestSuites,
        haddockBenchmarks    = combine haddockBenchmarks,
        haddockInternal      = combine haddockInternal,
        haddockCss           = combine haddockCss,
        haddockHscolour      = combine haddockHscolour,
        haddockHscolourCss   = combine haddockHscolourCss,
        haddockContents      = combine haddockContents,
        haddockDistPref      = combine haddockDistPref,
        haddockKeepTempFiles = combine haddockKeepTempFiles,
        haddockVerbosity     = combine haddockVerbosity
        }
        where
          combine      = combine'        savedHaddockFlags
          lastNonEmpty = lastNonEmpty'   savedHaddockFlags


--
-- * Default config
--

-- | These are the absolute basic defaults. The fields that must be
-- initialised. When we load the config from the file we layer the loaded
-- values over these ones, so any missing fields in the file take their values
-- from here.
--
baseSavedConfig :: IO SavedConfig
baseSavedConfig = do
  userPrefix <- defaultCabalDir
  logsDir    <- defaultLogsDir
  worldFile  <- defaultWorldFile
  return mempty {
    savedConfigureFlags  = mempty {
      configHcFlavor     = toFlag defaultCompiler,
      configUserInstall  = toFlag defaultUserInstall,
      configVerbosity    = toFlag normal
    },
    savedUserInstallDirs = mempty {
      prefix             = toFlag (toPathTemplate userPrefix)
    },
    savedGlobalFlags = mempty {
      globalLogsDir      = toFlag logsDir,
      globalWorldFile    = toFlag worldFile
    }
  }

-- | This is the initial configuration that we write out to to the config file
-- if the file does not exist (or the config we use if the file cannot be read
-- for some other reason). When the config gets loaded it gets layered on top
-- of 'baseSavedConfig' so we do not need to include it into the initial
-- values we save into the config file.
--
initialSavedConfig :: IO SavedConfig
initialSavedConfig = do
  cacheDir   <- defaultCacheDir
  logsDir    <- defaultLogsDir
  worldFile  <- defaultWorldFile
  extraPath  <- defaultExtraPath
  return mempty {
    savedGlobalFlags     = mempty {
      globalCacheDir     = toFlag cacheDir,
      globalRemoteRepos  = toNubList [defaultRemoteRepo],
      globalWorldFile    = toFlag worldFile
    },
    savedConfigureFlags  = mempty {
      configProgramPathExtra = toNubList extraPath
    },
    savedInstallFlags    = mempty {
      installSummaryFile = toNubList [toPathTemplate (logsDir </> "build.log")],
      installBuildReports= toFlag AnonymousReports,
      installNumJobs     = toFlag Nothing
    }
  }

--TODO: misleading, there's no way to override this default
--      either make it possible or rename to simply getCabalDir.
defaultCabalDir :: IO FilePath
defaultCabalDir = getAppUserDataDirectory "cabal"

defaultConfigFile :: IO FilePath
defaultConfigFile = do
  dir <- defaultCabalDir
  return $ dir </> "config"

defaultCacheDir :: IO FilePath
defaultCacheDir = do
  dir <- defaultCabalDir
  return $ dir </> "packages"

defaultLogsDir :: IO FilePath
defaultLogsDir = do
  dir <- defaultCabalDir
  return $ dir </> "logs"

-- | Default position of the world file
defaultWorldFile :: IO FilePath
defaultWorldFile = do
  dir <- defaultCabalDir
  return $ dir </> "world"

defaultExtraPath :: IO [FilePath]
defaultExtraPath = do
  dir <- defaultCabalDir
  return [dir </> "bin"]

defaultCompiler :: CompilerFlavor
defaultCompiler = fromMaybe GHC defaultCompilerFlavor

defaultUserInstall :: Bool
defaultUserInstall = True
-- We do per-user installs by default on all platforms. We used to default to
-- global installs on Windows but that no longer works on Windows Vista or 7.

defaultRemoteRepo :: RemoteRepo
defaultRemoteRepo = RemoteRepo name uri () False
  where
    name = "hackage.haskell.org"
    uri  = URI "http:" (Just (URIAuth "" name "")) "/" "" ""
    -- Note that lots of old ~/.cabal/config files will have the old url
    -- http://hackage.haskell.org/packages/archive
    -- but new config files can use the new url (without the /packages/archive)
    -- and avoid having to do a http redirect

    -- Use this as a source for crypto credentials when finding old remote-repo
    -- entries that match repo name and url (not only be used for generating
    -- fresh config files).

-- For the default repo we know extra information, fill this in.
--
-- We need this because the 'defaultRemoteRepo' above is only used for the
-- first time when a config file is made. So for users with older config files
-- we might have only have older info. This lets us fill that in even for old
-- config files.
--
addInfoForKnownRepos :: RemoteRepo -> RemoteRepo
addInfoForKnownRepos repo@RemoteRepo{ remoteRepoName = "hackage.haskell.org" } =
    repo {
      --remoteRepoRootKeys --TODO: when this list is empty, fill in known crypto credentials
      remoteRepoShouldTryHttps = True
    }
addInfoForKnownRepos other = other

--
-- * Config file reading
--

loadConfig :: Verbosity -> Flag FilePath -> IO SavedConfig
loadConfig verbosity configFileFlag = addBaseConf $ do
  let sources = [
        ("commandline option",   return . flagToMaybe $ configFileFlag),
        ("env var CABAL_CONFIG", lookup "CABAL_CONFIG" `liftM` getEnvironment),
        ("default config file",  Just `liftM` defaultConfigFile) ]

      getSource [] = error "no config file path candidate found."
      getSource ((msg,action): xs) =
                        action >>= maybe (getSource xs) (return . (,) msg)

  (source, configFile) <- getSource sources
  minp <- readConfigFile mempty configFile
  case minp of
    Nothing -> do
      notice verbosity $ "Config file path source is " ++ source ++ "."
      notice verbosity $ "Config file " ++ configFile ++ " not found."
      notice verbosity $ "Writing default configuration to " ++ configFile
      commentConf <- commentSavedConfig
      initialConf <- initialSavedConfig
      writeConfigFile configFile commentConf initialConf
      return initialConf
    Just (ParseOk ws conf) -> do
      unless (null ws) $ warn verbosity $
        unlines (map (showPWarning configFile) ws)
      return conf
    Just (ParseFailed err) -> do
      let (line, msg) = locatedErrorMsg err
      die $
          "Error parsing config file " ++ configFile
        ++ maybe "" (\n -> ':' : show n) line ++ ":\n" ++ msg

  where
    addBaseConf body = do
      base  <- baseSavedConfig
      extra <- body
      return (base `mappend` extra)

readConfigFile :: SavedConfig -> FilePath -> IO (Maybe (ParseResult SavedConfig))
readConfigFile initial file = handleNotExists $
  fmap (Just . parseConfig initial) (readFile file)

  where
    handleNotExists action = catchIO action $ \ioe ->
      if isDoesNotExistError ioe
        then return Nothing
        else ioError ioe

writeConfigFile :: FilePath -> SavedConfig -> SavedConfig -> IO ()
writeConfigFile file comments vals = do
  let tmpFile = file <.> "tmp"
  createDirectoryIfMissing True (takeDirectory file)
  writeFile tmpFile $ explanation ++ showConfigWithComments comments vals ++ "\n"
  renameFile tmpFile file
  where
    explanation = unlines
      ["-- This is the configuration file for the 'cabal' command line tool."
      ,""
      ,"-- The available configuration options are listed below."
      ,"-- Some of them have default values listed."
      ,""
      ,"-- Lines (like this one) beginning with '--' are comments."
      ,"-- Be careful with spaces and indentation because they are"
      ,"-- used to indicate layout for nested sections."
      ,""
      ,"-- Cabal library version: " ++ showVersion cabalVersion
      ,"-- cabal-install version: " ++ showVersion Paths_cabal_install.version
      ,"",""
      ]

-- | These are the default values that get used in Cabal if a no value is
-- given. We use these here to include in comments when we write out the
-- initial config file so that the user can see what default value they are
-- overriding.
--
commentSavedConfig :: IO SavedConfig
commentSavedConfig = do
  userInstallDirs   <- defaultInstallDirs defaultCompiler True True
  globalInstallDirs <- defaultInstallDirs defaultCompiler False True
  return SavedConfig {
    savedGlobalFlags       = defaultGlobalFlags,
    savedInstallFlags      = defaultInstallFlags,
    savedConfigureExFlags  = defaultConfigExFlags,
    savedConfigureFlags    = (defaultConfigFlags defaultProgramConfiguration) {
      configUserInstall    = toFlag defaultUserInstall
    },
    savedUserInstallDirs   = fmap toFlag userInstallDirs,
    savedGlobalInstallDirs = fmap toFlag globalInstallDirs,
    savedUploadFlags       = commandDefaultFlags uploadCommand,
    savedReportFlags       = commandDefaultFlags reportCommand,
    savedHaddockFlags      = defaultHaddockFlags
  }

-- | All config file fields.
--
configFieldDescriptions :: [FieldDescr SavedConfig]
configFieldDescriptions =

     toSavedConfig liftGlobalFlag
       (commandOptions (globalCommand []) ParseArgs)
       ["version", "numeric-version", "config-file", "sandbox-config-file"] []

  ++ toSavedConfig liftConfigFlag
       (configureOptions ParseArgs)
       (["builddir", "constraint", "dependency"]
        ++ map fieldName installDirsFields)

        --FIXME: this is only here because viewAsFieldDescr gives us a parser
        -- that only recognises 'ghc' etc, the case-sensitive flag names, not
        -- what the normal case-insensitive parser gives us.
       [simpleField "compiler"
          (fromFlagOrDefault Disp.empty . fmap Text.disp) (optional Text.parse)
          configHcFlavor (\v flags -> flags { configHcFlavor = v })
        -- TODO: The following is a temporary fix. The "optimization"
        -- and "debug-info" fields are OptArg, and viewAsFieldDescr
        -- fails on that. Instead of a hand-written hackaged parser
        -- and printer, we should handle this case properly in the
        -- library.
       ,liftField configOptimization (\v flags -> flags { configOptimization = v }) $
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
       ,liftField configDebugInfo (\v flags -> flags { configDebugInfo = v }) $
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
       ]

  ++ toSavedConfig liftConfigExFlag
       (configureExOptions ParseArgs)
       [] []

  ++ toSavedConfig liftInstallFlag
       (installOptions ParseArgs)
       ["dry-run", "only", "only-dependencies", "dependencies-only"] []

  ++ toSavedConfig liftUploadFlag
       (commandOptions uploadCommand ParseArgs)
       ["verbose", "check"] []

  ++ toSavedConfig liftReportFlag
       (commandOptions reportCommand ParseArgs)
       ["verbose", "username", "password"] []
       --FIXME: this is a hack, hiding the user name and password.
       -- But otherwise it masks the upload ones. Either need to
       -- share the options or make then distinct. In any case
       -- they should probably be per-server.

  ++ [ viewAsFieldDescr
       $ optionDistPref
       (configDistPref . savedConfigureFlags)
       (\distPref config ->
          config
          { savedConfigureFlags = (savedConfigureFlags config) { configDistPref = distPref }
          , savedHaddockFlags = (savedHaddockFlags config) { haddockDistPref = distPref }
          }
       )
       ParseArgs
     ]

  where
    toSavedConfig lift options exclusions replacements =
      [ lift (fromMaybe field replacement)
      | opt <- options
      , let field       = viewAsFieldDescr opt
            name        = fieldName field
            replacement = find ((== name) . fieldName) replacements
      , name `notElem` exclusions ]
    optional = Parse.option mempty . fmap toFlag

-- TODO: next step, make the deprecated fields elicit a warning.
--
deprecatedFieldDescriptions :: [FieldDescr SavedConfig]
deprecatedFieldDescriptions =
  [ liftGlobalFlag $
    listField "repos"
      (Disp.text . showRepo) parseRepo
      (fromNubList . globalRemoteRepos)
      (\rs cfg -> cfg { globalRemoteRepos = toNubList rs })
  , liftGlobalFlag $
    simpleField "cachedir"
      (Disp.text . fromFlagOrDefault "") (optional parseFilePathQ)
      globalCacheDir    (\d cfg -> cfg { globalCacheDir = d })
  , liftUploadFlag $
    simpleField "hackage-username"
      (Disp.text . fromFlagOrDefault "" . fmap unUsername)
      (optional (fmap Username parseTokenQ))
      uploadUsername    (\d cfg -> cfg { uploadUsername = d })
  , liftUploadFlag $
    simpleField "hackage-password"
      (Disp.text . fromFlagOrDefault "" . fmap unPassword)
      (optional (fmap Password parseTokenQ))
      uploadPassword    (\d cfg -> cfg { uploadPassword = d })
  , liftUploadFlag $
    spaceListField "hackage-password-command"
      Disp.text parseTokenQ
      (fromFlagOrDefault [] . uploadPasswordCmd)
                        (\d cfg -> cfg { uploadPasswordCmd = Flag d })
  ]
 ++ map (modifyFieldName ("user-"++)   . liftUserInstallDirs)   installDirsFields
 ++ map (modifyFieldName ("global-"++) . liftGlobalInstallDirs) installDirsFields
  where
    optional = Parse.option mempty . fmap toFlag
    modifyFieldName :: (String -> String) -> FieldDescr a -> FieldDescr a
    modifyFieldName f d = d { fieldName = f (fieldName d) }

liftUserInstallDirs :: FieldDescr (InstallDirs (Flag PathTemplate))
                    -> FieldDescr SavedConfig
liftUserInstallDirs = liftField
  savedUserInstallDirs (\flags conf -> conf { savedUserInstallDirs = flags })

liftGlobalInstallDirs :: FieldDescr (InstallDirs (Flag PathTemplate))
                      -> FieldDescr SavedConfig
liftGlobalInstallDirs = liftField
  savedGlobalInstallDirs (\flags conf -> conf { savedGlobalInstallDirs = flags })

liftGlobalFlag :: FieldDescr GlobalFlags -> FieldDescr SavedConfig
liftGlobalFlag = liftField
  savedGlobalFlags (\flags conf -> conf { savedGlobalFlags = flags })

liftConfigFlag :: FieldDescr ConfigFlags -> FieldDescr SavedConfig
liftConfigFlag = liftField
  savedConfigureFlags (\flags conf -> conf { savedConfigureFlags = flags })

liftConfigExFlag :: FieldDescr ConfigExFlags -> FieldDescr SavedConfig
liftConfigExFlag = liftField
  savedConfigureExFlags (\flags conf -> conf { savedConfigureExFlags = flags })

liftInstallFlag :: FieldDescr InstallFlags -> FieldDescr SavedConfig
liftInstallFlag = liftField
  savedInstallFlags (\flags conf -> conf { savedInstallFlags = flags })

liftUploadFlag :: FieldDescr UploadFlags -> FieldDescr SavedConfig
liftUploadFlag = liftField
  savedUploadFlags (\flags conf -> conf { savedUploadFlags = flags })

liftReportFlag :: FieldDescr ReportFlags -> FieldDescr SavedConfig
liftReportFlag = liftField
  savedReportFlags (\flags conf -> conf { savedReportFlags = flags })

parseConfig :: SavedConfig -> String -> ParseResult SavedConfig
parseConfig initial = \str -> do
  fields <- readFields str
  let (knownSections, others) = partition isKnownSection fields
  config <- parse others
  let user0   = savedUserInstallDirs config
      global0 = savedGlobalInstallDirs config
  (remoteRepoSections0, haddockFlags, user, global, paths, args) <-
    foldM parseSections
          ([], savedHaddockFlags config, user0, global0, [], [])
          knownSections

  let remoteRepoSections =
          map addInfoForKnownRepos
        . reverse
        . nubBy ((==) `on` remoteRepoName)
        $ remoteRepoSections0

  return config {
    savedGlobalFlags       = (savedGlobalFlags config) {
       globalRemoteRepos   = toNubList remoteRepoSections
       },
    savedConfigureFlags    = (savedConfigureFlags config) {
       configProgramPaths  = paths,
       configProgramArgs   = args
       },
    savedHaddockFlags      = haddockFlags,
    savedUserInstallDirs   = user,
    savedGlobalInstallDirs = global
  }

  where
    isKnownSection (ParseUtils.Section _ "remote-repo" _ _)             = True
    isKnownSection (ParseUtils.F _ "remote-repo" _)                     = True
    isKnownSection (ParseUtils.Section _ "haddock" _ _)                 = True
    isKnownSection (ParseUtils.Section _ "install-dirs" _ _)            = True
    isKnownSection (ParseUtils.Section _ "program-locations" _ _)       = True
    isKnownSection (ParseUtils.Section _ "program-default-options" _ _) = True
    isKnownSection _                                                    = False

    parse = parseFields (configFieldDescriptions
                      ++ deprecatedFieldDescriptions) initial

    parseSections (rs, h, u, g, p, a)
                 (ParseUtils.Section _ "remote-repo" name fs) = do
      r' <- parseFields remoteRepoFields (emptyRemoteRepo name) fs
      return (r':rs, h, u, g, p, a)

    parseSections (rs, h, u, g, p, a)
                 (ParseUtils.F lno "remote-repo" raw) = do
      let mr' = readRepo raw
      r' <- maybe (ParseFailed $ NoParse "remote-repo" lno) return mr'
      return (r':rs, h, u, g, p, a)

    parseSections accum@(rs, h, u, g, p, a)
                 (ParseUtils.Section _ "haddock" name fs)
      | name == ""        = do h' <- parseFields haddockFlagsFields h fs
                               return (rs, h', u, g, p, a)
      | otherwise         = do
          warning "The 'haddock' section should be unnamed"
          return accum
    parseSections accum@(rs, h, u, g, p, a)
                  (ParseUtils.Section _ "install-dirs" name fs)
      | name' == "user"   = do u' <- parseFields installDirsFields u fs
                               return (rs, h, u', g, p, a)
      | name' == "global" = do g' <- parseFields installDirsFields g fs
                               return (rs, h, u, g', p, a)
      | otherwise         = do
          warning "The 'install-paths' section should be for 'user' or 'global'"
          return accum
      where name' = lowercase name
    parseSections accum@(rs, h, u, g, p, a)
                 (ParseUtils.Section _ "program-locations" name fs)
      | name == ""        = do p' <- parseFields withProgramsFields p fs
                               return (rs, h, u, g, p', a)
      | otherwise         = do
          warning "The 'program-locations' section should be unnamed"
          return accum
    parseSections accum@(rs, h, u, g, p, a)
                  (ParseUtils.Section _ "program-default-options" name fs)
      | name == ""        = do a' <- parseFields withProgramOptionsFields a fs
                               return (rs, h, u, g, p, a')
      | otherwise         = do
          warning "The 'program-default-options' section should be unnamed"
          return accum
    parseSections accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

showConfig :: SavedConfig -> String
showConfig = showConfigWithComments mempty

showConfigWithComments :: SavedConfig -> SavedConfig -> String
showConfigWithComments comment vals = Disp.render $
      case fmap ppRemoteRepoSection . fromNubList . globalRemoteRepos . savedGlobalFlags $ vals of
        [] -> Disp.text ""
        (x:xs) -> foldl' (\ r r' -> r $+$ Disp.text "" $+$ r') x xs
  $+$ Disp.text ""
  $+$ ppFields (skipSomeFields configFieldDescriptions) mcomment vals
  $+$ Disp.text ""
  $+$ ppSection "haddock" "" haddockFlagsFields
                (fmap savedHaddockFlags mcomment) (savedHaddockFlags vals)
  $+$ Disp.text ""
  $+$ installDirsSection "user"   savedUserInstallDirs
  $+$ Disp.text ""
  $+$ installDirsSection "global" savedGlobalInstallDirs
  $+$ Disp.text ""
  $+$ configFlagsSection "program-locations" withProgramsFields
                         configProgramPaths
  $+$ Disp.text ""
  $+$ configFlagsSection "program-default-options" withProgramOptionsFields
                         configProgramArgs
  where
    mcomment = Just comment
    installDirsSection name field =
      ppSection "install-dirs" name installDirsFields
                (fmap field mcomment) (field vals)
    configFlagsSection name fields field =
      ppSection name "" fields
               (fmap (field . savedConfigureFlags) mcomment)
               ((field . savedConfigureFlags) vals)

    -- skip fields based on field name.  currently only skips "remote-repo",
    -- because that is rendered as a section.  (see 'ppRemoteRepoSection'.)
    skipSomeFields = filter ((/= "remote-repo") . fieldName)

-- | Fields for the 'install-dirs' sections.
installDirsFields :: [FieldDescr (InstallDirs (Flag PathTemplate))]
installDirsFields = map viewAsFieldDescr installDirsOptions

ppRemoteRepoSection :: RemoteRepo -> Doc
ppRemoteRepoSection vals = ppSection "remote-repo" (remoteRepoName vals)
        remoteRepoFields Nothing vals

remoteRepoFields :: [FieldDescr RemoteRepo]
remoteRepoFields =
    [ FieldDescr { fieldName = "url",
                   fieldGet = text . show . remoteRepoURI,
                   fieldSet = \ _ uriString remoteRepo -> maybe
                          (fail $ "remote-repo: no parse on " ++ show uriString)
                          (\ uri -> return $ remoteRepo { remoteRepoURI = uri })
                          (parseURI uriString)
                 }
    ]

-- | Fields for the 'haddock' section.
haddockFlagsFields :: [FieldDescr HaddockFlags]
haddockFlagsFields = [ field
                     | opt <- haddockOptions ParseArgs
                     , let field = viewAsFieldDescr opt
                           name  = fieldName field
                     , name `notElem` exclusions ]
  where
    exclusions = ["verbose", "builddir"]

-- | Fields for the 'program-locations' section.
withProgramsFields :: [FieldDescr [(String, FilePath)]]
withProgramsFields =
  map viewAsFieldDescr $
  programConfigurationPaths' (++ "-location") defaultProgramConfiguration
                             ParseArgs id (++)

-- | Fields for the 'program-default-options' section.
withProgramOptionsFields :: [FieldDescr [(String, [String])]]
withProgramOptionsFields =
  map viewAsFieldDescr $
  programConfigurationOptions defaultProgramConfiguration ParseArgs id (++)

-- | Get the differences (as a pseudo code diff) between the user's
-- '~/.cabal/config' and the one that cabal would generate if it didn't exist.
userConfigDiff :: GlobalFlags -> IO [String]
userConfigDiff globalFlags = do
  userConfig <- loadConfig normal (globalConfigFile globalFlags)
  testConfig <- liftM2 mappend baseSavedConfig initialSavedConfig
  return $ reverse . foldl' createDiff [] . M.toList
                $ M.unionWith combine
                    (M.fromList . map justFst $ filterShow testConfig)
                    (M.fromList . map justSnd $ filterShow userConfig)
  where
    justFst (a, b) = (a, (Just b, Nothing))
    justSnd (a, b) = (a, (Nothing, Just b))

    combine (Nothing, Just b) (Just a, Nothing) = (Just a, Just b)
    combine (Just a, Nothing) (Nothing, Just b) = (Just a, Just b)
    combine x y = error $ "Can't happen : userConfigDiff " ++ show x ++ " " ++ show y

    createDiff :: [String] -> (String, (Maybe String, Maybe String)) -> [String]
    createDiff acc (key, (Just a, Just b))
        | a == b = acc
        | otherwise = ("+ " ++ key ++ ": " ++ b) : ("- " ++ key ++ ": " ++ a) : acc
    createDiff acc (key, (Nothing, Just b)) = ("+ " ++ key ++ ": " ++ b) : acc
    createDiff acc (key, (Just a, Nothing)) = ("- " ++ key ++ ": " ++ a) : acc
    createDiff acc (_, (Nothing, Nothing)) = acc

    filterShow :: SavedConfig -> [(String, String)]
    filterShow cfg = map keyValueSplit
        . filter (\s -> not (null s) && any (== ':') s)
        . map nonComment
        . lines
        $ showConfig cfg

    nonComment [] = []
    nonComment ('-':'-':_) = []
    nonComment (x:xs) = x : nonComment xs

    topAndTail = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    keyValueSplit s =
        let (left, right) = break (== ':') s
        in (topAndTail left, topAndTail (drop 1 right))


-- | Update the user's ~/.cabal/config' keeping the user's customizations.
userConfigUpdate :: Verbosity -> GlobalFlags -> IO ()
userConfigUpdate verbosity globalFlags = do
  userConfig <- loadConfig normal (globalConfigFile globalFlags)
  newConfig <- liftM2 mappend baseSavedConfig initialSavedConfig
  commentConf <- commentSavedConfig
  cabalFile <- defaultConfigFile
  let backup = cabalFile ++ ".backup"
  notice verbosity $ "Renaming " ++ cabalFile ++ " to " ++ backup ++ "."
  renameFile cabalFile backup
  notice verbosity $ "Writing merged config to " ++ cabalFile ++ "."
  writeConfigFile cabalFile commentConf (newConfig `mappend` userConfig)
