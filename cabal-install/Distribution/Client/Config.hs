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
    installDirsFields
  ) where


import Distribution.Client.Types
         ( RemoteRepo(..), Username(..), Password(..) )
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand
         , ConfigExFlags(..), configureExOptions, defaultConfigExFlags
         , InstallFlags(..), installOptions, defaultInstallFlags
         , UploadFlags(..), uploadCommand
         , ReportFlags(..), reportCommand
         , showRepo, parseRepo )

import Distribution.Simple.Compiler
         ( OptimisationLevel(..) )
import Distribution.Simple.Setup
         ( ConfigFlags(..), configureOptions, defaultConfigFlags
         , installDirsOptions
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
         , simpleField, listField, parseFilePathQ, parseTokenQ )
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
         ( notice, warn, lowercase )
import Distribution.Compiler
         ( CompilerFlavor(..), defaultCompilerFlavor )
import Distribution.Verbosity
         ( Verbosity, normal )

import Data.List
         ( partition, find )
import Data.Maybe
         ( fromMaybe )
import Data.Monoid
         ( Monoid(..) )
import Control.Monad
         ( unless, foldM, liftM )
import qualified Distribution.Compat.ReadP as Parse
         ( option )
import qualified Text.PrettyPrint as Disp
         ( render, text, empty )
import Text.PrettyPrint
         ( ($+$) )
import System.Directory
         ( createDirectoryIfMissing, getAppUserDataDirectory, renameFile )
import Network.URI
         ( URI(..), URIAuth(..) )
import System.FilePath
         ( (<.>), (</>), takeDirectory )
import System.IO.Error
         ( isDoesNotExistError )
import Distribution.Compat.Environment
         ( getEnvironment )
import Distribution.Compat.Exception
         ( catchIO )

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
    savedReportFlags       :: ReportFlags
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
    savedReportFlags       = mempty
  }
  mappend a b = SavedConfig {
    savedGlobalFlags       = combine savedGlobalFlags,
    savedInstallFlags      = combine savedInstallFlags,
    savedConfigureFlags    = combine savedConfigureFlags,
    savedConfigureExFlags  = combine savedConfigureExFlags,
    savedUserInstallDirs   = combine savedUserInstallDirs,
    savedGlobalInstallDirs = combine savedGlobalInstallDirs,
    savedUploadFlags       = combine savedUploadFlags,
    savedReportFlags       = combine savedReportFlags
  }
    where combine field = field a `mappend` field b

updateInstallDirs :: Flag Bool -> SavedConfig -> SavedConfig
updateInstallDirs userInstallFlag
  savedConfig@SavedConfig {
    savedConfigureFlags    = configureFlags,
    savedUserInstallDirs   = userInstallDirs,
    savedGlobalInstallDirs = globalInstallDirs
  } =
  savedConfig {
    savedConfigureFlags = configureFlags {
      configInstallDirs = installDirs
    }
  }
  where
    installDirs | userInstall = userInstallDirs
                | otherwise   = globalInstallDirs
    userInstall = fromFlagOrDefault defaultUserInstall $
                    configUserInstall configureFlags `mappend` userInstallFlag

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
      globalRemoteRepos  = [defaultRemoteRepo],
      globalWorldFile    = toFlag worldFile
    },
    savedConfigureFlags  = mempty {
      configProgramPathExtra = extraPath
    },
    savedInstallFlags    = mempty {
      installSummaryFile = [toPathTemplate (logsDir </> "build.log")],
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
defaultRemoteRepo = RemoteRepo name uri
  where
    name = "hackage.haskell.org"
    uri  = URI "http:" (Just (URIAuth "" name "")) "/packages/archive" "" ""

--
-- * Config file reading
--

loadConfig :: Verbosity -> Flag FilePath -> Flag Bool -> IO SavedConfig
loadConfig verbosity configFileFlag userInstallFlag = addBaseConf $ do
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
      warn verbosity $
          "Error parsing config file " ++ configFile
        ++ maybe "" (\n -> ':' : show n) line ++ ":\n" ++ msg
      warn verbosity "Using default configuration."
      initialSavedConfig

  where
    addBaseConf body = do
      base  <- baseSavedConfig
      extra <- body
      return (updateInstallDirs userInstallFlag (base `mappend` extra))

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
    savedGlobalFlags       = commandDefaultFlags globalCommand,
    savedInstallFlags      = defaultInstallFlags,
    savedConfigureExFlags  = defaultConfigExFlags,
    savedConfigureFlags    = (defaultConfigFlags defaultProgramConfiguration) {
      configUserInstall    = toFlag defaultUserInstall
    },
    savedUserInstallDirs   = fmap toFlag userInstallDirs,
    savedGlobalInstallDirs = fmap toFlag globalInstallDirs,
    savedUploadFlags       = commandDefaultFlags uploadCommand,
    savedReportFlags       = commandDefaultFlags reportCommand
  }

-- | All config file fields.
--
configFieldDescriptions :: [FieldDescr SavedConfig]
configFieldDescriptions =

     toSavedConfig liftGlobalFlag
       (commandOptions globalCommand ParseArgs)
       ["version", "numeric-version", "config-file", "sandbox-config-file"] []

  ++ toSavedConfig liftConfigFlag
       (configureOptions ParseArgs)
       (["builddir", "configure-option", "constraint", "dependency"]
        ++ map fieldName installDirsFields)

        --FIXME: this is only here because viewAsFieldDescr gives us a parser
        -- that only recognises 'ghc' etc, the case-sensitive flag names, not
        -- what the normal case-insensitive parser gives us.
       [simpleField "compiler"
          (fromFlagOrDefault Disp.empty . fmap Text.disp) (optional Text.parse)
          configHcFlavor (\v flags -> flags { configHcFlavor = v })
        -- TODO: The following is a temporary fix. The "optimization" field is
        -- OptArg, and viewAsFieldDescr fails on that. Instead of a hand-written
        -- hackaged parser and printer, we should handle this case properly in
        -- the library.
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
       --FIXME: this is a hack, hiding the username and password.
       -- But otherwise it masks the upload ones. Either need to
       -- share the options or make then distinct. In any case
       -- they should probably be per-server.

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
      globalRemoteRepos (\rs cfg -> cfg { globalRemoteRepos = rs })
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
  (user, global, paths, args) <-
    foldM parseSections (user0, global0, [], []) knownSections
  return config {
    savedConfigureFlags    = (savedConfigureFlags config) {
       configProgramPaths  = paths,
       configProgramArgs   = args
       },
    savedUserInstallDirs   = user,
    savedGlobalInstallDirs = global
  }

  where
    isKnownSection (ParseUtils.Section _ "install-dirs" _ _)            = True
    isKnownSection (ParseUtils.Section _ "program-locations" _ _)       = True
    isKnownSection (ParseUtils.Section _ "program-default-options" _ _) = True
    isKnownSection _                                                    = False

    parse = parseFields (configFieldDescriptions
                      ++ deprecatedFieldDescriptions) initial

    parseSections accum@(u,g,p,a) (ParseUtils.Section _ "install-dirs" name fs)
      | name' == "user"   = do u' <- parseFields installDirsFields u fs
                               return (u', g, p, a)
      | name' == "global" = do g' <- parseFields installDirsFields g fs
                               return (u, g', p, a)
      | otherwise         = do
          warning "The install-paths section should be for 'user' or 'global'"
          return accum
      where name' = lowercase name
    parseSections accum@(u,g,p,a)
                 (ParseUtils.Section _ "program-locations" name fs)
      | name == ""        = do p' <- parseFields withProgramsFields p fs
                               return (u, g, p', a)
      | otherwise         = do
          warning "The 'program-locations' section should be unnamed"
          return accum
    parseSections accum@(u, g, p, a)
                  (ParseUtils.Section _ "program-default-options" name fs)
      | name == ""        = do a' <- parseFields withProgramOptionsFields a fs
                               return (u, g, p, a')
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
      ppFields configFieldDescriptions mcomment vals
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


installDirsFields :: [FieldDescr (InstallDirs (Flag PathTemplate))]
installDirsFields = map viewAsFieldDescr installDirsOptions

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
