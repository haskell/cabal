-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Config
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling saved state such as known packages, known servers and downloaded packages.
-----------------------------------------------------------------------------
module Hackage.Config
    ( SavedConfig(..)
    , savedConfigToConfigFlags
    , configRepos
    , defaultConfigFile
    , loadConfig
    , showConfig
    ) where

import Prelude hiding (catch)
import Data.Char (isAlphaNum, toLower)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.Monoid (Monoid(..))
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath ((</>), takeDirectory)
import Text.PrettyPrint.HughesPJ (text)

import Distribution.Compat.ReadP (ReadP, char, munch1, readS_to_P)
import Distribution.Compiler (CompilerFlavor(..), defaultCompilerFlavor)
import Distribution.PackageDescription.Parse (ParseResult(..))
import Distribution.ParseUtils (FieldDescr(..), simpleField, listField, liftField, field)
import Distribution.Simple.InstallDirs (InstallDirs(..), PathTemplate, toPathTemplate)
import Distribution.Simple.Setup (Flag(..), toFlag, fromFlag, fromFlagOrDefault)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Verbosity (Verbosity, normal)

import Hackage.Types (RemoteRepo(..), Repo(..), Username, Password)
import Hackage.ParseUtils
import Hackage.Utils (readFileIfExists)
import Distribution.Simple.Utils (notice, warn)


--
-- * Configuration saved in the config file
--

data SavedConfig = SavedConfig {
    configCompiler          :: Flag CompilerFlavor,
    configCompilerPath      :: Flag FilePath,
    configHcPkgPath         :: Flag FilePath,
    configUserInstallDirs   :: InstallDirs (Flag PathTemplate),
    configGlobalInstallDirs :: InstallDirs (Flag PathTemplate),
    configCacheDir          :: Flag FilePath,
    configRemoteRepos       :: [RemoteRepo],     -- ^Available Hackage servers.
    configVerbose           :: Flag Verbosity,
    configUserInstall       :: Flag Bool,        -- ^--user-install flag
    configUploadUsername    :: Flag Username,
    configUploadPassword    :: Flag Password
  }
  deriving (Show)

configRepos :: SavedConfig -> [Repo]
configRepos config =
  [ let cacheDir = fromFlag (configCacheDir config)
               </> remoteRepoName remote
     in Repo remote cacheDir
  | remote <- configRemoteRepos config ]

savedConfigToConfigFlags :: Flag Bool -> SavedConfig -> Cabal.ConfigFlags
savedConfigToConfigFlags userInstallFlag config = mempty {
    Cabal.configHcFlavor    = configCompiler config,
    Cabal.configHcPath      = configCompilerPath config,
    Cabal.configHcPkg       = configHcPkgPath config,
    Cabal.configUserInstall = toFlag userInstall,
    Cabal.configInstallDirs = if userInstall
                                then configUserInstallDirs config
                                else configGlobalInstallDirs config,
    Cabal.configVerbose     = configVerbose config
  }
  where userInstall :: Bool
        userInstall = fromFlag $ configUserInstall config
                       `mappend` userInstallFlag

--
-- * Default config
--

defaultCabalDir :: IO FilePath
defaultCabalDir = getAppUserDataDirectory "cabal"

defaultConfigFile :: IO FilePath
defaultConfigFile = do dir <- defaultCabalDir
                       return $ dir </> "config"

defaultCacheDir :: IO FilePath
defaultCacheDir = do dir <- defaultCabalDir
                     return $ dir </> "packages"

defaultCompiler :: CompilerFlavor
defaultCompiler = fromMaybe GHC defaultCompilerFlavor

defaultUserInstallDirs :: IO (InstallDirs (Flag PathTemplate))
defaultUserInstallDirs =
    do userPrefix <- defaultCabalDir
       return $ defaultGlobalInstallDirs {
         prefix = toFlag (toPathTemplate userPrefix)
       }

defaultGlobalInstallDirs :: InstallDirs (Flag PathTemplate)
defaultGlobalInstallDirs = mempty

defaultSavedConfig :: IO SavedConfig
defaultSavedConfig =
    do userInstallDirs <- defaultUserInstallDirs
       cacheDir        <- defaultCacheDir
       return SavedConfig
         { configCompiler          = toFlag defaultCompiler
         , configCompilerPath      = mempty
         , configHcPkgPath         = mempty
         , configUserInstallDirs   = userInstallDirs
         , configGlobalInstallDirs = defaultGlobalInstallDirs
         , configCacheDir          = toFlag cacheDir
         , configRemoteRepos       = [defaultRemoteRepo]
         , configVerbose           = toFlag normal
         , configUserInstall       = toFlag True
         , configUploadUsername    = mempty
         , configUploadPassword    = mempty
         }

defaultRemoteRepo :: RemoteRepo
defaultRemoteRepo =
  RemoteRepo "hackage.haskell.org"
             "http://hackage.haskell.org/packages/archive"

--
-- * Config file reading
--

loadConfig :: Verbosity -> FilePath -> IO SavedConfig
loadConfig verbosity configFile = 
    do defaultConf <- defaultSavedConfig
       minp <- readFileIfExists configFile
       case minp of
         Nothing -> do notice verbosity $ "Config file " ++ configFile ++ " not found."
                       notice verbosity $ "Writing default configuration to " ++ configFile
                       writeDefaultConfigFile configFile defaultConf
                       return defaultConf
         Just inp -> case parseBasicStanza configFieldDescrs defaultConf inp of
                       ParseOk ws conf -> 
                           do when (not $ null ws) $
                                warn verbosity $ "Config file: " ++ unlines ws
                              return conf
                       ParseFailed err -> 
                           do warn verbosity $ "Error parsing config file " 
                                            ++ configFile ++ ": " ++ showPError err
                              warn verbosity $ "Using default configuration."
                              return defaultConf

writeDefaultConfigFile :: FilePath -> SavedConfig -> IO ()
writeDefaultConfigFile file cfg = 
    do createDirectoryIfMissing True (takeDirectory file)
       writeFile file $ showFields configWriteFieldDescrs cfg ++ "\n"

showConfig :: SavedConfig -> String
showConfig = showFields configFieldDescrs

-- | All config file fields.
configFieldDescrs :: [FieldDescr SavedConfig]
configFieldDescrs = 
    configWriteFieldDescrs
    ++ map userInstallDirField installDirDescrs
    ++ map globalInstallDirField installDirDescrs

-- | The subset of the config file fields that we write out
-- if the config file is missing.
configWriteFieldDescrs :: [FieldDescr SavedConfig]
configWriteFieldDescrs =
    [ simpleField "compiler"
                (text . show . fromFlagOrDefault GHC) (fmap toFlag parseCompilerFlavor)
                configCompiler (\c cfg -> cfg { configCompiler = c })
    , listField "repos"
                (text . showRepo)                  parseRepo
                configRemoteRepos (\rs cfg -> cfg { configRemoteRepos = rs })
    , simpleField "cachedir"
                (text . show . fromFlagOrDefault "") (fmap emptyToNothing $ readS_to_P reads)
                configCacheDir    (\d cfg -> cfg { configCacheDir = d })
    , boolField "user-install" (fromFlag . configUserInstall) (\u cfg -> cfg { configUserInstall = toFlag u })
    , simpleField "hackage-username"
                (text . show . fromFlagOrDefault "")
                (fmap emptyToNothing $ readS_to_P reads)
                configUploadUsername    (\d cfg -> cfg { configUploadUsername = d })
    , simpleField "hackage-password"
                (text . show . fromFlagOrDefault "")
                (fmap emptyToNothing $ readS_to_P reads)
                configUploadPassword    (\d cfg -> cfg { configUploadPassword = d })
    ]
    where emptyToNothing "" = mempty
          emptyToNothing f  = toFlag f

installDirDescrs :: [FieldDescr (InstallDirs (Flag PathTemplate))]
installDirDescrs =
    [ installDirField "prefix"     prefix     (\d ds -> ds { prefix     = d })
    , installDirField "bindir"     bindir     (\d ds -> ds { bindir     = d })
    , installDirField "libdir"     libdir     (\d ds -> ds { libdir     = d })
    , installDirField "libexecdir" libexecdir (\d ds -> ds { libexecdir = d })
    , installDirField "datadir"    datadir    (\d ds -> ds { datadir    = d })
    , installDirField "docdir"     docdir     (\d ds -> ds { docdir     = d })
    , installDirField "htmldir"    htmldir    (\d ds -> ds { htmldir    = d })
    ]


userInstallDirField :: FieldDescr (InstallDirs (Flag PathTemplate)) -> FieldDescr SavedConfig
userInstallDirField f = modifyFieldName ("user-"++) $
    liftField configUserInstallDirs 
              (\d cfg -> cfg { configUserInstallDirs = d }) 
              f

globalInstallDirField :: FieldDescr (InstallDirs (Flag PathTemplate)) -> FieldDescr SavedConfig
globalInstallDirField f = modifyFieldName ("global-"++) $
    liftField configGlobalInstallDirs 
              (\d cfg -> cfg { configGlobalInstallDirs = d }) 
              f

installDirField :: String 
                -> (InstallDirs (Flag PathTemplate) -> Flag PathTemplate) 
                -> (Flag PathTemplate -> InstallDirs (Flag PathTemplate) -> InstallDirs (Flag PathTemplate))
                -> FieldDescr (InstallDirs (Flag PathTemplate))
installDirField name get set = 
    liftField get set $ field name (text . show . fromFlag)
                                   (fmap toFlag $ readS_to_P reads)

modifyFieldName :: (String -> String) -> FieldDescr a -> FieldDescr a
modifyFieldName f d = d { fieldName = f (fieldName d) }

parseCompilerFlavor :: ReadP r CompilerFlavor
parseCompilerFlavor = 
    do s <- munch1 isAlphaNum
       return $ case map toLower s of
                  "ghc"    -> GHC
                  "nhc"    -> NHC
                  "hugs"   -> Hugs
                  "hbc"    -> HBC
                  "helium" -> Helium
                  "jhc"    -> JHC
                  _        -> OtherCompiler s

showRepo :: RemoteRepo -> String
showRepo repo = remoteRepoName repo ++ ":" ++ remoteRepoURL repo

parseRepo :: ReadP r RemoteRepo
parseRepo = do name <- munch1 (\c -> isAlphaNum c || c `elem` "_-.")
               char ':'
               url <- munch1 (\c -> isAlphaNum c || c `elem` "+-=._/*()@'$:;&!?")
               return $ RemoteRepo { remoteRepoName = name, remoteRepoURL = url }

