-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PackageEnvironment
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for working with the package environment file. Patterned after
-- Distribution.Client.Config.
-----------------------------------------------------------------------------

module Distribution.Client.PackageEnvironment (
    PackageEnvironment(..)
  , IncludeComments(..)
  , createPackageEnvironment
  , tryLoadPackageEnvironment
  , readPackageEnvironmentFile
  , showPackageEnvironment
  , showPackageEnvironmentWithComments
  , setPackageDB

  , basePackageEnvironment
  , initialPackageEnvironment
  , commentPackageEnvironment
  , sandboxPackageEnvironmentFile
  , userPackageEnvironmentFile
  ) where

import Distribution.Client.Config      ( SavedConfig(..), commentSavedConfig,
                                         loadConfig, configFieldDescriptions,
                                         installDirsFields, defaultCompiler )
import Distribution.Client.ParseUtils  ( parseFields, ppFields, ppSection )
import Distribution.Client.Setup       ( GlobalFlags(..), ConfigExFlags(..)
                                       , InstallFlags(..)
                                       , defaultSandboxLocation )
import Distribution.Simple.Compiler    ( Compiler, PackageDB(..)
                                         , showCompilerId )
import Distribution.Simple.InstallDirs ( InstallDirs(..), PathTemplate,
                                         fromPathTemplate, toPathTemplate )
import Distribution.Simple.Setup       ( Flag(..), ConfigFlags(..),
                                         fromFlagOrDefault, toFlag )
import Distribution.Simple.Utils       ( die, notice, warn, lowercase )
import Distribution.ParseUtils         ( FieldDescr(..), ParseResult(..),
                                         commaListField,
                                         liftField, lineNo, locatedErrorMsg,
                                         parseFilePathQ, readFields,
                                         showPWarning, simpleField, warning )
import Distribution.Verbosity          ( Verbosity, normal )
import Control.Monad                   ( foldM, when )
import Data.List                       ( partition )
import Data.Monoid                     ( Monoid(..) )
import Distribution.Compat.Exception   ( catchIO )
import System.Directory                ( renameFile )
import System.FilePath                 ( (<.>), (</>) )
import System.IO.Error                 ( isDoesNotExistError )
import Text.PrettyPrint                ( ($+$) )

import qualified Text.PrettyPrint          as Disp
import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.ParseUtils   as ParseUtils ( Field(..) )
import qualified Distribution.Text         as Text


--
-- * Configuration saved in the package environment file
--

-- TODO: would be nice to remove duplication between D.C.PackageEnvironment and
-- D.C.Config.
data PackageEnvironment = PackageEnvironment {
  pkgEnvInherit       :: Flag FilePath,
  pkgEnvSavedConfig   :: SavedConfig
}

instance Monoid PackageEnvironment where
  mempty = PackageEnvironment {
    pkgEnvInherit       = mempty,
    pkgEnvSavedConfig   = mempty
    }

  mappend a b = PackageEnvironment {
    pkgEnvInherit       = combine pkgEnvInherit,
    pkgEnvSavedConfig   = combine pkgEnvSavedConfig
    }
    where
      combine f = f a `mappend` f b

-- | The automatically-created package environment file that should not be
-- touched by the user.
sandboxPackageEnvironmentFile :: FilePath
sandboxPackageEnvironmentFile = "cabal.sandbox.config"

-- | Optional package environment file that can be used to customize the default
-- settings. Created by the user.
userPackageEnvironmentFile :: FilePath
userPackageEnvironmentFile = "cabal.config"

-- | Defaults common to 'initialPackageEnvironment' and
-- 'commentPackageEnvironment'.
commonPackageEnvironmentConfig :: FilePath -> SavedConfig
commonPackageEnvironmentConfig sandboxDir =
  mempty {
    savedConfigureFlags = mempty {
       configUserInstall = toFlag True,
       configInstallDirs = sandboxInstallDirs
       },
    savedUserInstallDirs   = sandboxInstallDirs,
    savedGlobalInstallDirs = sandboxInstallDirs,
    savedGlobalFlags = mempty {
      globalLogsDir = toFlag $ sandboxDir </> "logs",
      -- Is this right? cabal-dev uses the global world file.
      globalWorldFile = toFlag $ sandboxDir </> "world"
      }
    }
  where
    sandboxInstallDirs = mempty { prefix = toFlag (toPathTemplate sandboxDir) }

-- | These are the absolute basic defaults, the fields that must be
-- initialised. When we load the package environment from the file we layer the
-- loaded values over these ones.
basePackageEnvironment :: FilePath -> PackageEnvironment
basePackageEnvironment sandboxDir = do
  let baseConf = commonPackageEnvironmentConfig sandboxDir in
    mempty {
      pkgEnvSavedConfig = baseConf {
         savedConfigureFlags = (savedConfigureFlags baseConf) {
            configHcFlavor    = toFlag defaultCompiler,
            configVerbosity   = toFlag normal
            }
         }
      }

-- | Initial configuration that we write out to the package environment file if
-- it does not exist. When the package environment gets loaded this
-- configuration gets layered on top of 'basePackageEnvironment'.
initialPackageEnvironment :: FilePath -> Compiler -> SavedConfig
                             -> IO PackageEnvironment
initialPackageEnvironment sandboxDir compiler userConfig = do
  let commonConfig  = commonPackageEnvironmentConfig sandboxDir
      initialConfig = userConfig `mappend` commonConfig
  return $ mempty {
    pkgEnvSavedConfig = initialConfig {
       savedGlobalFlags = (savedGlobalFlags initialConfig) {
          globalLocalRepos = [sandboxDir </> "packages"]
          },
       savedConfigureFlags = setPackageDB sandboxDir compiler
                             (savedConfigureFlags initialConfig),
       savedInstallFlags = (savedInstallFlags initialConfig) {
         installSummaryFile = [toPathTemplate (sandboxDir </>
                                               "logs" </> "build.log")]
         }
       }
    }

-- | Use the package DB location specific for this compiler.
setPackageDB :: FilePath -> Compiler -> ConfigFlags -> ConfigFlags
setPackageDB sandboxDir compiler configFlags =
  configFlags {
    configPackageDBs = [Just (SpecificPackageDB $ sandboxDir
                              </> (showCompilerId compiler ++
                                   "-packages.conf.d"))]
    }

-- | Default values that get used if no value is given. Used here to include in
-- comments when we write out the initial package environment.
commentPackageEnvironment :: FilePath -> IO PackageEnvironment
commentPackageEnvironment sandboxDir = do
  commentConf  <- commentSavedConfig
  let baseConf =  commonPackageEnvironmentConfig sandboxDir
  return $ mempty {
    pkgEnvSavedConfig = commentConf `mappend` baseConf
    }

-- | Return the base package environment: settings from the config file this
-- package environment optionally inherits from layered on top of
-- `basePackageEnvironment`.
basePkgEnv :: Verbosity -> FilePath -> (Flag FilePath) -> IO PackageEnvironment
basePkgEnv verbosity sandboxDir inheritConfig = do
  let base     = basePackageEnvironment sandboxDir
      baseConf = pkgEnvSavedConfig base
  -- Does this package environment inherit from some config file?
  case inheritConfig of
    NoFlag          -> return base
    (Flag confPath) -> do
      conf <- loadConfig verbosity (Flag confPath) NoFlag
      return $ base { pkgEnvSavedConfig = baseConf `mappend` conf }

-- | Load the user package environment if it exists (the optional "cabal.config"
-- file).
userPkgEnv :: Verbosity -> FilePath -> IO PackageEnvironment
userPkgEnv verbosity pkgEnvDir = do
  let path = pkgEnvDir </> userPackageEnvironmentFile
  minp <- readPackageEnvironmentFile mempty path
  case minp of
    Nothing -> return mempty
    Just (ParseOk warns parseResult) -> do
      when (not $ null warns) $ warn verbosity $
        unlines (map (showPWarning path) warns)
      return parseResult
    Just (ParseFailed err) -> do
      let (line, msg) = locatedErrorMsg err
      warn verbosity $ "Error parsing user package environment file " ++ path
        ++ maybe "" (\n -> ":" ++ show n) line ++ ":\n" ++ msg
      return mempty

-- | Try to load the package environment file ("cabal.sandbox.config"), exiting
-- with error if it doesn't exist. Also returns the path to the sandbox
-- directory. Note that the path parameter should be a name of an existing
-- directory.
tryLoadPackageEnvironment :: Verbosity -> FilePath
                             -> IO (FilePath, PackageEnvironment)
tryLoadPackageEnvironment verbosity pkgEnvDir = do
  let path = pkgEnvDir </> sandboxPackageEnvironmentFile
  minp <- readPackageEnvironmentFile mempty path
  pkgEnv <- case minp of
    Nothing -> die $
      "The package environment file '" ++ path ++ "' doesn't exist"
    Just (ParseOk warns parseResult) -> do
      when (not $ null warns) $ warn verbosity $
        unlines (map (showPWarning path) warns)
      return parseResult
    Just (ParseFailed err) -> do
      let (line, msg) = locatedErrorMsg err
      die $ "Error parsing package environment file " ++ path
        ++ maybe "" (\n -> ":" ++ show n) line ++ ":\n" ++ msg
  user <- userPkgEnv verbosity pkgEnvDir
  -- Get the saved sandbox directory.
  -- TODO: Use substPathTemplate instead of fromPathTemplate.
  let sandboxDir = fromFlagOrDefault defaultSandboxLocation
                   . fmap fromPathTemplate . prefix . savedGlobalInstallDirs
                   . pkgEnvSavedConfig $ pkgEnv
  base <- basePkgEnv verbosity sandboxDir (pkgEnvInherit pkgEnv)
  return (sandboxDir, base `mappend` user `mappend` pkgEnv)

-- | Should the generated package environment file include comments?
data IncludeComments = IncludeComments | NoComments

-- | Create a new package environment file, replacing the existing one if it
-- exists. Note that the path parameters should point to existing directories.
createPackageEnvironment :: Verbosity -> FilePath -> FilePath
                            -> IncludeComments
                            -> Compiler -> SavedConfig
                            -> IO PackageEnvironment
createPackageEnvironment verbosity sandboxDir pkgEnvDir
  incComments compiler userConfig = do
  let path = pkgEnvDir </> sandboxPackageEnvironmentFile
  notice verbosity $ "Writing default package environment to " ++ path

  commentPkgEnv <- commentPackageEnvironment sandboxDir
  initialPkgEnv <- initialPackageEnvironment sandboxDir compiler userConfig
  writePackageEnvironmentFile path incComments commentPkgEnv initialPkgEnv

  user <- userPkgEnv verbosity pkgEnvDir
  base <- basePkgEnv verbosity sandboxDir (pkgEnvInherit initialPkgEnv)
  return $ base `mappend` user `mappend` initialPkgEnv

-- | Descriptions of all fields in the package environment file.
pkgEnvFieldDescrs :: [FieldDescr PackageEnvironment]
pkgEnvFieldDescrs = [
  simpleField "inherit"
    (fromFlagOrDefault Disp.empty . fmap Disp.text) (optional parseFilePathQ)
    pkgEnvInherit (\v pkgEnv -> pkgEnv { pkgEnvInherit = v })

    -- FIXME: Should we make these fields part of ~/.cabal/config ?
  , commaListField "constraints"
    Text.disp Text.parse
    (configExConstraints . savedConfigureExFlags . pkgEnvSavedConfig)
    (\v pkgEnv -> updateConfigureExFlags pkgEnv
                  (\flags -> flags { configExConstraints = v }))

  , commaListField "preferences"
    Text.disp Text.parse
    (configPreferences . savedConfigureExFlags . pkgEnvSavedConfig)
    (\v pkgEnv -> updateConfigureExFlags pkgEnv
                  (\flags -> flags { configPreferences = v }))
  ]
  ++ map toPkgEnv configFieldDescriptions'
  where
    optional = Parse.option mempty . fmap toFlag

    configFieldDescriptions' :: [FieldDescr SavedConfig]
    configFieldDescriptions' = filter
      (\(FieldDescr name _ _) -> name /= "preference" && name /= "constraint")
      configFieldDescriptions

    toPkgEnv :: FieldDescr SavedConfig -> FieldDescr PackageEnvironment
    toPkgEnv fieldDescr =
      liftField pkgEnvSavedConfig
      (\savedConfig pkgEnv -> pkgEnv { pkgEnvSavedConfig = savedConfig})
      fieldDescr

    updateConfigureExFlags :: PackageEnvironment
                              -> (ConfigExFlags -> ConfigExFlags)
                              -> PackageEnvironment
    updateConfigureExFlags pkgEnv f = pkgEnv {
      pkgEnvSavedConfig = (pkgEnvSavedConfig pkgEnv) {
         savedConfigureExFlags = f . savedConfigureExFlags . pkgEnvSavedConfig
                                 $ pkgEnv
         }
      }

-- | Read the package environment file.
readPackageEnvironmentFile :: PackageEnvironment -> FilePath
                              -> IO (Maybe (ParseResult PackageEnvironment))
readPackageEnvironmentFile initial file =
  handleNotExists $
  fmap (Just . parsePackageEnvironment initial) (readFile file)
  where
    handleNotExists action = catchIO action $ \ioe ->
      if isDoesNotExistError ioe
        then return Nothing
        else ioError ioe

-- | Parse the package environment file.
parsePackageEnvironment :: PackageEnvironment -> String
                           -> ParseResult PackageEnvironment
parsePackageEnvironment initial str = do
  fields <- readFields str
  let (knownSections, others) = partition isKnownSection fields
  pkgEnv <- parse others
  let config       = pkgEnvSavedConfig pkgEnv
      installDirs0 = savedUserInstallDirs config
  -- 'install-dirs' is the only section that we care about.
  installDirs <- foldM parseSection installDirs0 knownSections
  return pkgEnv {
    pkgEnvSavedConfig = config {
       savedUserInstallDirs   = installDirs,
       savedGlobalInstallDirs = installDirs
       }
    }

  where
    isKnownSection :: ParseUtils.Field -> Bool
    isKnownSection (ParseUtils.Section _ "install-dirs" _ _) = True
    isKnownSection _                                         = False

    parse :: [ParseUtils.Field] -> ParseResult PackageEnvironment
    parse = parseFields pkgEnvFieldDescrs initial

    parseSection :: InstallDirs (Flag PathTemplate)
                    -> ParseUtils.Field
                    -> ParseResult (InstallDirs (Flag PathTemplate))
    parseSection accum (ParseUtils.Section _ "install-dirs" name fs)
      | name' == "" = do accum' <- parseFields installDirsFields accum fs
                         return accum'
      | otherwise   = do warning "The install-dirs section should be unnamed"
                         return accum
      where name' = lowercase name
    parseSection accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

-- | Write out the package environment file.
writePackageEnvironmentFile :: FilePath -> IncludeComments
                               -> PackageEnvironment -> PackageEnvironment
                               -> IO ()
writePackageEnvironmentFile path incComments comments pkgEnv = do
  let tmpPath = (path <.> "tmp")
  writeFile tmpPath $ explanation ++ pkgEnvStr ++ "\n"
  renameFile tmpPath path
  where
    pkgEnvStr = case incComments of
      IncludeComments -> showPackageEnvironmentWithComments
                         (Just comments) pkgEnv
      NoComments      -> showPackageEnvironment pkgEnv
    explanation = unlines
      ["-- This is a Cabal package environment file."
      ,"-- THIS FILE IS AUTO-GENERATED. DO NOT EDIT DIRECTLY."
      ,"-- Please create a 'cabal.config' file in the same directory"
      ,"-- if you want to change the default settings for this sandbox."
      ,""
      ,"-- The available configuration options are listed below."
      ,"-- Some of them have default values listed."
      ,""
      ,"-- Lines (like this one) beginning with '--' are comments."
      ,"-- Be careful with spaces and indentation because they are"
      ,"-- used to indicate layout for nested sections."
      ,"",""
      ]

-- | Pretty-print the package environment.
showPackageEnvironment :: PackageEnvironment -> String
showPackageEnvironment pkgEnv = showPackageEnvironmentWithComments Nothing pkgEnv

-- | Pretty-print the package environment with default values for empty fields
-- commented out (just like the default ~/.cabal/config).
showPackageEnvironmentWithComments :: (Maybe PackageEnvironment)
                                      -> PackageEnvironment
                                      -> String
showPackageEnvironmentWithComments mdefPkgEnv pkgEnv = Disp.render $
      ppFields pkgEnvFieldDescrs mdefPkgEnv pkgEnv
  $+$ Disp.text ""
  $+$ ppSection "install-dirs" "" installDirsFields
                (fmap installDirsSection mdefPkgEnv) (installDirsSection pkgEnv)
  where
    installDirsSection = savedUserInstallDirs . pkgEnvSavedConfig
