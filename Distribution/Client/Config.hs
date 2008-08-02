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
module Distribution.Client.Config
    ( SavedConfig(..)
    , savedConfigToConfigFlags
    , configRepos
    , configPackageDB
    , defaultConfigFile
    , defaultCabalDir
    , defaultCacheDir
    , loadConfig
    , showConfig
    ) where

import Prelude hiding (catch)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.Monoid (Monoid(..))
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath ((</>), takeDirectory)
import Network.URI
         ( URI(..), URIAuth(..), parseAbsoluteURI, uriToString )
import Text.PrettyPrint.HughesPJ (text)

import Distribution.Compat.ReadP as ReadP
         ( ReadP, char, munch1, pfail )
import Distribution.Compiler (CompilerFlavor(..), defaultCompilerFlavor)
import Distribution.ParseUtils
         ( FieldDescr(..), simpleField, listField, liftField, field
         , parseFilePathQ, parseTokenQ, showPWarning, ParseResult(..) )
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.InstallDirs
         ( InstallDirs(..), PathTemplate, toPathTemplate, fromPathTemplate )
import Distribution.Simple.Command (ShowOrParseArgs(..), viewAsFieldDescr)
import Distribution.Simple.Setup
         ( Flag(..), toFlag, fromFlag, fromFlagOrDefault
         , ConfigFlags, configureOptions )
import qualified Distribution.Simple.Setup as ConfigFlags
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Verbosity (Verbosity, normal)
import Distribution.System
         ( OS(Windows), buildOS )

import Distribution.Client.Types
         ( RemoteRepo(..), Repo(..), Username(..), Password(..) )
import Distribution.Client.ParseUtils
import Distribution.Client.Utils (readFileIfExists)
import Distribution.Simple.Utils (notice, warn)

configPackageDB :: Cabal.ConfigFlags -> PackageDB
configPackageDB config =
  fromFlagOrDefault defaultDB (Cabal.configPackageDB config)
  where
    defaultDB = case Cabal.configUserInstall config of
      NoFlag     -> UserPackageDB
      Flag True  -> UserPackageDB
      Flag False -> GlobalPackageDB

--
-- * Configuration saved in the config file
--

data SavedConfig = SavedConfig {
    configCacheDir          :: Flag FilePath,
    configRemoteRepos       :: [RemoteRepo],     -- ^Available Hackage servers.
    configUploadUsername    :: Flag Username,
    configUploadPassword    :: Flag Password,
    configUserInstallDirs   :: InstallDirs (Flag PathTemplate),
    configGlobalInstallDirs :: InstallDirs (Flag PathTemplate),
    configFlags             :: ConfigFlags,
    configSymlinkBinDir     :: Flag FilePath
  }

configUserInstall     :: SavedConfig -> Flag Bool
configUserInstall     =  ConfigFlags.configUserInstall . configFlags

configRepos :: SavedConfig -> [Repo]
configRepos config =
  [ let cacheDir = fromFlag (configCacheDir config)
               </> remoteRepoName remote
     in Repo (Left remote) cacheDir
  | remote <- configRemoteRepos config ]

savedConfigToConfigFlags :: Flag Bool -> SavedConfig -> Cabal.ConfigFlags
savedConfigToConfigFlags userInstallFlag config = (configFlags config) {
    Cabal.configUserInstall = toFlag userInstall,
    Cabal.configInstallDirs = if userInstall
                                then configUserInstallDirs config
                                else configGlobalInstallDirs config
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

defaultUserInstall :: Bool
defaultUserInstall = case buildOS of
  -- We do global installs by default on Windows
  Windows -> False
  -- and per-user installs by default everywhere else
  _       -> True

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
       return SavedConfig {
           configFlags = mempty {
               ConfigFlags.configHcFlavor    = toFlag defaultCompiler
             , ConfigFlags.configVerbosity   = toFlag normal
             , ConfigFlags.configUserInstall = toFlag defaultUserInstall
             , ConfigFlags.configInstallDirs = error
               "ConfigFlags.installDirs: avoid this field."
               --use UserInstallDirs or GlobalInstallDirs instead
             }
         , configUserInstallDirs   = userInstallDirs
         , configGlobalInstallDirs = defaultGlobalInstallDirs
         , configCacheDir          = toFlag cacheDir
         , configRemoteRepos       = [defaultRemoteRepo]
         , configUploadUsername    = mempty
         , configUploadPassword    = mempty
         , configSymlinkBinDir     = mempty
         }

defaultRemoteRepo :: RemoteRepo
defaultRemoteRepo = RemoteRepo name uri
  where
    name = "hackage.haskell.org"
    uri  = URI "http:" (Just (URIAuth "" name "")) "/packages/archive" "" ""
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
         Just inp -> case parseBasicStanza configFieldDescrs defaultConf' inp of
                       ParseOk ws conf -> 
                           do when (not $ null ws) $ warn verbosity $
                                unlines (map (showPWarning configFile) ws)
                              return conf
                       ParseFailed err -> 
                           do warn verbosity $ "Error parsing config file " 
                                            ++ configFile ++ ": " ++ showPError err
                              warn verbosity $ "Using default configuration."
                              return defaultConf
           where defaultConf' = defaultConf { configRemoteRepos = [] }

writeDefaultConfigFile :: FilePath -> SavedConfig -> IO ()
writeDefaultConfigFile file cfg = 
    do createDirectoryIfMissing True (takeDirectory file)
       writeFile file $ showFields configWriteFieldDescrs cfg ++ "\n"

showConfig :: SavedConfig -> String
showConfig = showFields configFieldDescrs

-- | All config file fields.
configFieldDescrs :: [FieldDescr SavedConfig]
configFieldDescrs =
    map ( configFlagsField . viewAsFieldDescr) (configureOptions ShowArgs)
    ++ configCabalInstallFieldDescrs
    ++ map userInstallDirField installDirDescrs
    ++ map globalInstallDirField installDirDescrs

configCabalInstallFieldDescrs :: [FieldDescr SavedConfig]
configCabalInstallFieldDescrs =
    [ listField "repos"
                (text . showRepo)                  parseRepo
                configRemoteRepos (\rs cfg -> cfg { configRemoteRepos = rs })
    , simpleField "cachedir"
                (text . show . fromFlagOrDefault "")
                (fmap emptyToNothing parseFilePathQ)
                configCacheDir    (\d cfg -> cfg { configCacheDir = d })
    , simpleField "hackage-username"
                (text . show . fromFlagOrDefault "" . fmap unUsername)
                (fmap (fmap Username . emptyToNothing) parseTokenQ)
                configUploadUsername    (\d cfg -> cfg { configUploadUsername = d })
    , simpleField "hackage-password"
                (text . show . fromFlagOrDefault "" . fmap unPassword)
                (fmap (fmap Password . emptyToNothing) parseTokenQ)
                configUploadPassword    (\d cfg -> cfg { configUploadPassword = d })
    , simpleField "symlink-bindir"
                (text . show . fromFlagOrDefault "")
                (fmap emptyToNothing parseFilePathQ)
                configSymlinkBinDir     (\d cfg -> cfg { configSymlinkBinDir = d })
    ]
    where emptyToNothing "" = mempty
          emptyToNothing f  = toFlag f
                              
-- | The subset of the config file fields that we write out
-- if the config file is missing.
configWriteFieldDescrs :: [FieldDescr SavedConfig]
configWriteFieldDescrs = configCabalInstallFieldDescrs
                         ++ [f | f <- configFieldDescrs, fieldName f `elem` ["compiler", "user-install"]]

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

configFlagsField :: FieldDescr ConfigFlags -> FieldDescr SavedConfig
configFlagsField = liftField configFlags (\ff cfg -> cfg{configFlags=ff})


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
    liftField get set $
      field name (text . fromPathTemplate . fromFlag)
                 (fmap (toFlag . toPathTemplate) parseFilePathQ)

modifyFieldName :: (String -> String) -> FieldDescr a -> FieldDescr a
modifyFieldName f d = d { fieldName = f (fieldName d) }

showRepo :: RemoteRepo -> String
showRepo repo = remoteRepoName repo ++ ":"
             ++ uriToString id (remoteRepoURI repo) []

parseRepo :: ReadP r RemoteRepo
parseRepo = do name <- munch1 (\c -> isAlphaNum c || c `elem` "_-.")
               char ':'
               uriStr <- munch1 (\c -> isAlphaNum c || c `elem` "+-=._/*()@'$:;&!?")
               uri <- maybe ReadP.pfail return (parseAbsoluteURI uriStr)
               return $ RemoteRepo {
                 remoteRepoName = name,
                 remoteRepoURI  = uri
               }

