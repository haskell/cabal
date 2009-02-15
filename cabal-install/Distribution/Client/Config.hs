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
-- Utilities for handling saved state such as known packages, known servers and downloaded packages.
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
    defaultLogsDir,
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
         , showRepo, parseRepo )

import Distribution.Simple.Setup
         ( ConfigFlags(..), configureOptions, defaultConfigFlags
         , Flag, toFlag, flagToMaybe, fromFlagOrDefault, flagToList )
import Distribution.Simple.InstallDirs
         ( InstallDirs(..), defaultInstallDirs
         , PathTemplate, toPathTemplate, fromPathTemplate )
import Distribution.ParseUtils
         ( FieldDescr(..), liftField
         , ParseResult(..), locatedErrorMsg, showPWarning
         , readFields, warning, lineNo
         , simpleField, listField, parseFilePathQ, showFilePath, parseTokenQ )
import qualified Distribution.ParseUtils as ParseUtils
         ( Field(..) )
import qualified Distribution.Text as Text
         ( Text(..) )
import Distribution.ReadE
         ( readP_to_E )
import Distribution.Simple.Command
         ( CommandUI(commandOptions), commandDefaultFlags, ShowOrParseArgs(..)
         , viewAsFieldDescr, OptionField, option, reqArg )
import Distribution.Simple.Program
         ( defaultProgramConfiguration )
import Distribution.Simple.Utils
         ( notice, warn, lowercase )
import Distribution.Compiler
         ( CompilerFlavor(..), defaultCompilerFlavor )
import Distribution.System
         ( OS(Windows), buildOS )
import Distribution.Verbosity
         ( Verbosity, normal )

import Data.List
         ( partition, find )
import Data.Maybe
         ( fromMaybe )
import Data.Monoid
         ( Monoid(..) )
import Control.Monad
         ( when, foldM )
import qualified Data.Map as Map
import qualified Distribution.Compat.ReadP as Parse
         ( option )
import qualified Text.PrettyPrint.HughesPJ as Disp
         ( Doc, render, text, colon, vcat, empty, isEmpty, nest )
import Text.PrettyPrint.HughesPJ
         ( (<>), (<+>), ($$), ($+$) )
import System.Directory
         ( createDirectoryIfMissing, getAppUserDataDirectory )
import Network.URI
         ( URI(..), URIAuth(..) )
import System.FilePath
         ( (</>), takeDirectory )
import System.IO.Error
         ( isDoesNotExistError )

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
    savedUploadFlags       :: UploadFlags
  }

instance Monoid SavedConfig where
  mempty = SavedConfig {
    savedGlobalFlags       = mempty,
    savedInstallFlags      = mempty,
    savedConfigureFlags    = mempty,
    savedConfigureExFlags  = mempty,
    savedUserInstallDirs   = mempty,
    savedGlobalInstallDirs = mempty,
    savedUploadFlags       = mempty
  }
  mappend a b = SavedConfig {
    savedGlobalFlags       = combine savedGlobalFlags,
    savedInstallFlags      = combine savedInstallFlags,
    savedConfigureFlags    = combine savedConfigureFlags,
    savedConfigureExFlags  = combine savedConfigureExFlags,
    savedUserInstallDirs   = combine savedUserInstallDirs,
    savedGlobalInstallDirs = combine savedGlobalInstallDirs,
    savedUploadFlags       = combine savedUploadFlags
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
  return mempty {
    savedConfigureFlags  = mempty {
      configHcFlavor     = toFlag defaultCompiler,
      configUserInstall  = toFlag defaultUserInstall,
      configVerbosity    = toFlag normal
    },
    savedUserInstallDirs = mempty {
      prefix             = toFlag (toPathTemplate userPrefix)
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
  return mempty {
    savedGlobalFlags     = mempty {
      globalCacheDir     = toFlag cacheDir,
      globalRemoteRepos  = [defaultRemoteRepo]
    },
    savedInstallFlags    = mempty {
      installSummaryFile = [toPathTemplate (logsDir </> "build.log")],
      installBuildReports= toFlag AnonymousReports
    }
  }

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

defaultCompiler :: CompilerFlavor
defaultCompiler = fromMaybe GHC defaultCompilerFlavor

defaultUserInstall :: Bool
defaultUserInstall = case buildOS of
  -- We do global installs by default on Windows
  Windows -> False
  -- and per-user installs by default everywhere else
  _       -> True

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
  configFile <- maybe defaultConfigFile return (flagToMaybe configFileFlag)

  minp <- readConfigFile mempty configFile
  case minp of
    Nothing -> do
      notice verbosity $ "Config file " ++ configFile ++ " not found."
      notice verbosity $ "Writing default configuration to " ++ configFile
      commentConf <- commentSavedConfig
      initialConf <- initialSavedConfig
      writeConfigFile configFile commentConf initialConf
      return initialConf
    Just (ParseOk ws conf) -> do
      when (not $ null ws) $ warn verbosity $
        unlines (map (showPWarning configFile) ws)
      return conf
    Just (ParseFailed err) -> do
      let (line, msg) = locatedErrorMsg err
      warn verbosity $
          "Error parsing config file " ++ configFile
        ++ maybe "" (\n -> ":" ++ show n) line ++ ":\n" ++ msg
      warn verbosity $ "Using default configuration."
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
    handleNotExists action = catch action $ \ioe ->
      if isDoesNotExistError ioe
        then return Nothing
        else ioError ioe

writeConfigFile :: FilePath -> SavedConfig -> SavedConfig -> IO ()
writeConfigFile file comments vals = do
  createDirectoryIfMissing True (takeDirectory file)
  writeFile file $ explanation ++ showConfigWithComments comments vals ++ "\n"
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
    savedUploadFlags       = commandDefaultFlags uploadCommand
  }

-- | All config file fields.
--
configFieldDescriptions :: [FieldDescr SavedConfig]
configFieldDescriptions =

     toSavedConfig liftGlobalFlag
       (commandOptions globalCommand ParseArgs)
       ["version", "numeric-version", "config-file"] []

  ++ toSavedConfig liftConfigFlag
       (configureOptions ParseArgs)
       (["builddir", "configure-option"] ++ map fieldName installDirsFields)

        --FIXME: this is only here because viewAsFieldDescr gives us a parser
        -- that only recognises 'ghc' etc, the case-sensitive flag names, not
        -- what the normal case-insensitive parser gives us.
       [simpleField "compiler"
          (fromFlagOrDefault Disp.empty . fmap Text.disp) (optional Text.parse)
          configHcFlavor (\v flags -> flags { configHcFlavor = v })
       ]

  ++ toSavedConfig liftConfigExFlag
       (configureExOptions ParseArgs)
       [] []

  ++ toSavedConfig liftInstallFlag
       (installOptions ParseArgs)
       ["dry-run", "reinstall", "only"] []

  ++ toSavedConfig liftUploadFlag
       (commandOptions uploadCommand ParseArgs)
       ["verbose", "check"] []

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

parseConfig :: SavedConfig -> String -> ParseResult SavedConfig
parseConfig initial = \str -> do
  fields <- readFields str
  let (knownSections, others) = partition isKnownSection fields
  config <- parse others
  let user0   = savedUserInstallDirs config
      global0 = savedGlobalInstallDirs config
  (user, global) <- foldM parseSections (user0, global0) knownSections
  return config {
    savedUserInstallDirs   = user,
    savedGlobalInstallDirs = global
  }

  where
    isKnownSection (ParseUtils.Section _ "install-dirs" _ _) = True
    isKnownSection _                                          = False

    parse = parseFields (configFieldDescriptions
                      ++ deprecatedFieldDescriptions) initial

    parseSections accum@(u,g) (ParseUtils.Section _ "install-dirs" name fs)
      | name' == "user"   = do u' <- parseFields installDirsFields u fs
                               return (u', g)
      | name' == "global" = do g' <- parseFields installDirsFields g fs
                               return (u, g')
      | otherwise         = do
          warning "The install-paths section should be for 'user' or 'global'"
          return accum
      where name' = lowercase name
    parseSections accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

showConfig :: SavedConfig -> String
showConfig = showConfigWithComments mempty

showConfigWithComments :: SavedConfig -> SavedConfig -> String
showConfigWithComments comment vals = Disp.render $
      ppFields configFieldDescriptions comment vals
  $+$ Disp.text ""
  $+$ installDirsSection "user"   savedUserInstallDirs
  $+$ Disp.text ""
  $+$ installDirsSection "global" savedGlobalInstallDirs
  where
    installDirsSection name field =
      ppSection "install-dirs" name installDirsFields
                (field comment) (field vals)

------------------------
-- * Parsing utils
--

--FIXME: replace this with something better in Cabal-1.5
parseFields :: [FieldDescr a] -> a -> [ParseUtils.Field] -> ParseResult a
parseFields fields initial = foldM setField initial
  where
    fieldMap = Map.fromList
      [ (name, f) | f@(FieldDescr name _ _) <- fields ]
    setField accum (ParseUtils.F line name value) = case Map.lookup name fieldMap of
      Just (FieldDescr _ _ set) -> set line value accum
      Nothing -> do
        warning $ "Unrecognized field " ++ name ++ " on line " ++ show line
        return accum
    setField accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

-- | This is a customised version of the function from Cabal that also prints
-- default values for empty fields as comments.
--
ppFields :: [FieldDescr a] -> a -> a -> Disp.Doc
ppFields fields def cur = Disp.vcat [ ppField name (getter def) (getter cur)
                                    | FieldDescr name getter _ <- fields]

ppField :: String -> Disp.Doc -> Disp.Doc -> Disp.Doc
ppField name def cur
  | Disp.isEmpty cur = Disp.text "--" <+> Disp.text name <> Disp.colon <+> def
  | otherwise        =                    Disp.text name <> Disp.colon <+> cur

ppSection :: String -> String -> [FieldDescr a] -> a -> a -> Disp.Doc
ppSection name arg fields def cur =
     Disp.text name <+> Disp.text arg
  $$ Disp.nest 2 (ppFields fields def cur)

installDirsFields :: [FieldDescr (InstallDirs (Flag PathTemplate))]
installDirsFields = map viewAsFieldDescr installDirsOptions

--TODO: this is now exported in Cabal-1.5
installDirsOptions :: [OptionField (InstallDirs (Flag PathTemplate))]
installDirsOptions =
  [ option "" ["prefix"]
      "bake this prefix in preparation of installation"
      prefix (\v flags -> flags { prefix = v })
      installDirArg

  , option "" ["bindir"]
      "installation directory for executables"
      bindir (\v flags -> flags { bindir = v })
      installDirArg

  , option "" ["libdir"]
      "installation directory for libraries"
      libdir (\v flags -> flags { libdir = v })
      installDirArg

  , option "" ["libsubdir"]
      "subdirectory of libdir in which libs are installed"
      libsubdir (\v flags -> flags { libsubdir = v })
      installDirArg

  , option "" ["libexecdir"]
      "installation directory for program executables"
      libexecdir (\v flags -> flags { libexecdir = v })
      installDirArg

  , option "" ["datadir"]
      "installation directory for read-only data"
      datadir (\v flags -> flags { datadir = v })
      installDirArg

  , option "" ["datasubdir"]
      "subdirectory of datadir in which data files are installed"
      datasubdir (\v flags -> flags { datasubdir = v })
      installDirArg

  , option "" ["docdir"]
      "installation directory for documentation"
      docdir (\v flags -> flags { docdir = v })
      installDirArg

  , option "" ["htmldir"]
      "installation directory for HTML documentation"
      htmldir (\v flags -> flags { htmldir = v })
      installDirArg

  , option "" ["haddockdir"]
      "installation directory for haddock interfaces"
      haddockdir (\v flags -> flags { haddockdir = v })
      installDirArg
  ]
  where
    installDirArg _sf _lf d get set =
      reqArgFlag "DIR" _sf _lf d
        (fmap fromPathTemplate . get) (set . fmap toPathTemplate)

    reqArgFlag ad = reqArg ad (fmap toFlag (readP_to_E err parseFilePathQ))
                              (map (show . showFilePath) . flagToList)
      where err _ = "paths with spaces must use Haskell String syntax"
