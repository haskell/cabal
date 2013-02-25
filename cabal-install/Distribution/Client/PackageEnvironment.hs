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
import Distribution.Simple.InstallDirs ( InstallDirs(..), PathTemplate
                                       , fromPathTemplate, toPathTemplate )
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
       configInstallDirs = installDirs
       },
    savedUserInstallDirs   = installDirs,
    savedGlobalInstallDirs = installDirs,
    savedGlobalFlags = mempty {
      globalLogsDir = toFlag $ sandboxDir </> "logs",
      -- Is this right? cabal-dev uses the global world file.
      globalWorldFile = toFlag $ sandboxDir </> "world"
      }
    }
  where
    installDirs = sandboxInstallDirs sandboxDir

-- | 'commonPackageEnvironmentConfig' wrapped inside a 'PackageEnvironment'.
commonPackageEnvironment :: FilePath -> PackageEnvironment
commonPackageEnvironment sandboxDir = mempty {
  pkgEnvSavedConfig = commonPackageEnvironmentConfig sandboxDir
  }

-- | Given a path to a sandbox, return the corresponding InstallDirs record.
sandboxInstallDirs :: FilePath -> InstallDirs (Flag PathTemplate)
sandboxInstallDirs sandboxDir = mempty {
  prefix = toFlag (toPathTemplate sandboxDir)
  }

-- | These are the absolute basic defaults, the fields that must be
-- initialised. When we load the package environment from the file we layer the
-- loaded values over these ones.
basePackageEnvironment :: PackageEnvironment
basePackageEnvironment =
    mempty {
      pkgEnvSavedConfig = mempty {
         savedConfigureFlags = mempty {
            configHcFlavor    = toFlag defaultCompiler,
            configVerbosity   = toFlag normal
            }
         }
      }

-- | Initial configuration that we write out to the package environment file if
-- it does not exist. When the package environment gets loaded this
-- configuration gets layered on top of 'basePackageEnvironment'.
initialPackageEnvironment :: FilePath -> Compiler -> IO PackageEnvironment
initialPackageEnvironment sandboxDir compiler = do
  let initialConfig = commonPackageEnvironmentConfig sandboxDir
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

-- | Almost the same as 'savedConf `mappend` pkgEnv', but some settings are
-- overridden instead of mappend'ed.
overrideSandboxSettings :: SavedConfig -> PackageEnvironment ->
                           PackageEnvironment
overrideSandboxSettings savedConf pkgEnv =
  pkgEnv {
    pkgEnvSavedConfig = mappendedConf {
       savedGlobalFlags = (savedGlobalFlags mappendedConf) {
          globalLocalRepos = globalLocalRepos pkgEnvGlobalFlags
          }
       , savedConfigureFlags = (savedConfigureFlags mappendedConf) {
          configPackageDBs = configPackageDBs pkgEnvConfigureFlags
          }
       , savedInstallFlags = (savedInstallFlags mappendedConf) {
          installSummaryFile = installSummaryFile pkgEnvInstallFlags
          }
       }
    }
  where
    pkgEnvConf           = pkgEnvSavedConfig pkgEnv
    mappendedConf        = savedConf `mappend` pkgEnvConf
    pkgEnvGlobalFlags    = savedGlobalFlags pkgEnvConf
    pkgEnvConfigureFlags = savedConfigureFlags pkgEnvConf
    pkgEnvInstallFlags   = savedInstallFlags pkgEnvConf

-- | Default values that get used if no value is given. Used here to include in
-- comments when we write out the initial package environment.
commentPackageEnvironment :: FilePath -> IO PackageEnvironment
commentPackageEnvironment sandboxDir = do
  commentConf  <- commentSavedConfig
  let baseConf =  commonPackageEnvironmentConfig sandboxDir
  return $ mempty {
    pkgEnvSavedConfig = commentConf `mappend` baseConf
    }

-- | If this package environment inherits from some other package environment,
-- return that package environment; otherwise return mempty.
inheritedPackageEnvironment :: Verbosity -> PackageEnvironment
                               -> IO PackageEnvironment
inheritedPackageEnvironment verbosity pkgEnv = do
  case (pkgEnvInherit pkgEnv) of
    NoFlag                -> return mempty
    confPathFlag@(Flag _) -> do
      conf <- loadConfig verbosity confPathFlag NoFlag
      return $ mempty { pkgEnvSavedConfig = conf }

-- | Load the user package environment if it exists (the optional "cabal.config"
-- file).
userPackageEnvironment :: Verbosity -> FilePath -> IO PackageEnvironment
userPackageEnvironment verbosity pkgEnvDir = do
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
tryLoadPackageEnvironment :: Verbosity -> FilePath -> (Flag FilePath)
                             -> IO (FilePath, PackageEnvironment)
tryLoadPackageEnvironment verbosity pkgEnvDir configFileFlag = do
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

  -- Get the saved sandbox directory.
  -- TODO: Use substPathTemplate with compilerTemplateEnv ++ platformTemplateEnv.
  let sandboxDir = fromFlagOrDefault defaultSandboxLocation
                   . fmap fromPathTemplate . prefix . savedUserInstallDirs
                   . pkgEnvSavedConfig $ pkgEnv

  let base   = basePackageEnvironment
  let common = commonPackageEnvironment sandboxDir
  user      <- userPackageEnvironment verbosity pkgEnvDir
  inherited <- inheritedPackageEnvironment verbosity user

  cabalConfig <- loadConfig verbosity configFileFlag NoFlag
  return (sandboxDir,
          base `mappend` (cabalConfig `overrideSandboxSettings`
          (common `mappend` inherited `mappend` user `mappend` pkgEnv)))

-- | Should the generated package environment file include comments?
data IncludeComments = IncludeComments | NoComments

-- | Create a new package environment file, replacing the existing one if it
-- exists. Note that the path parameters should point to existing directories.
createPackageEnvironment :: Verbosity -> FilePath -> FilePath
                            -> IncludeComments
                            -> Compiler
                            -> IO ()
createPackageEnvironment verbosity sandboxDir pkgEnvDir incComments compiler = do
  let path = pkgEnvDir </> sandboxPackageEnvironmentFile
  notice verbosity $ "Writing default package environment to " ++ path

  commentPkgEnv <- commentPackageEnvironment sandboxDir
  initialPkgEnv <- initialPackageEnvironment sandboxDir compiler
  writePackageEnvironmentFile path incComments commentPkgEnv initialPkgEnv

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
