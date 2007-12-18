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
    ( repoCacheDir
    , packageFile
    , packageDir
    , listInstalledPackages
    , pkgURL
    , defaultConfigFile
    , loadConfig
    , showConfig
    , findCompiler
    ) where

import Prelude hiding (catch)
import Data.Char (isAlphaNum, toLower)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.Monoid (Monoid(mempty))
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath ((</>), takeDirectory, (<.>))
import Text.PrettyPrint.HughesPJ (text)

import Distribution.Compat.ReadP (ReadP, char, munch1, readS_to_P)
import Distribution.Compiler (CompilerFlavor(..), defaultCompilerFlavor)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PackageDescription (ParseResult(..))
import Distribution.ParseUtils (FieldDescr(..), simpleField, listField, liftField, field)
import Distribution.Simple.Compiler (Compiler, PackageDB(..))
import Distribution.Simple.Configure (getInstalledPackages)
import qualified Distribution.Simple.Configure as Configure (configCompiler)
import Distribution.Simple.InstallDirs (InstallDirs(..), PathTemplate, toPathTemplate)
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Setup (toFlag, fromFlagOrDefault)
import Distribution.Version (showVersion)
import Distribution.Verbosity (normal)

import Hackage.Types (ConfigFlags (..), PkgInfo (..), Repo(..))
import Hackage.Utils
import Distribution.Simple.Utils (notice, warn)


-- | Full path to the local cache directory for a repository.
repoCacheDir :: ConfigFlags -> Repo -> FilePath
repoCacheDir cfg repo = configCacheDir cfg </> repoName repo

-- |Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
packageFile :: ConfigFlags -> PkgInfo -> FilePath
packageFile cfg pkg = packageDir cfg pkg
                      </> showPackageId (pkgInfoId pkg)
                      <.> "tar.gz"

-- |Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifer@ is stored.
packageDir :: ConfigFlags -> PkgInfo -> FilePath
packageDir cfg pkg = repoCacheDir cfg (pkgRepo pkg)
                     </> pkgName p
                     </> showVersion (pkgVersion p)
  where p = pkgInfoId pkg

listInstalledPackages :: ConfigFlags -> Compiler -> ProgramConfiguration -> IO [PackageIdentifier]
listInstalledPackages cfg comp conf =
    do Just ipkgs <- getInstalledPackages
                         (configVerbose cfg) comp
                         (if configUserInstall cfg then UserPackageDB
                                               else GlobalPackageDB)
                         conf
       return ipkgs

-- | Generate the URL of the tarball for a given package.
pkgURL :: PkgInfo -> String
pkgURL pkg = joinWith "/" [repoURL (pkgRepo pkg), pkgName p, showVersion (pkgVersion p), 
                           showPackageId p ++ ".tar.gz"]              
    where joinWith tok = concat . intersperse tok
          p = pkgInfoId pkg

--
-- * Compiler and programs
--

findCompiler :: ConfigFlags -> IO (Compiler, ProgramConfiguration)
findCompiler cfg = Configure.configCompiler 
                     (Just (configCompiler cfg)) 
                     (configCompilerPath cfg)
                     (configHcPkgPath cfg)
                     defaultProgramConfiguration 
                     (configVerbose cfg)

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

defaultUserInstallDirs :: IO (InstallDirs (Maybe PathTemplate))
defaultUserInstallDirs =
    do userPrefix <- defaultCabalDir
       return $ defaultGlobalInstallDirs {
         prefix = Just (toPathTemplate userPrefix)
       }

defaultGlobalInstallDirs :: InstallDirs (Maybe PathTemplate)
defaultGlobalInstallDirs = fmap (\() -> Nothing) mempty

defaultConfigFlags :: IO ConfigFlags
defaultConfigFlags = 
    do userInstallDirs <- defaultUserInstallDirs
       cacheDir    <- defaultCacheDir
       return $ ConfigFlags 
               { configCompiler    = defaultCompiler
               , configCompilerPath = Nothing
               , configHcPkgPath   = Nothing
               , configUserInstallDirs = userInstallDirs
               , configGlobalInstallDirs = defaultGlobalInstallDirs
               , configCacheDir    = cacheDir
               , configRepos       = [Repo "hackage.haskell.org" "http://hackage.haskell.org/packages/archive"]
               , configVerbose     = normal
               , configUserInstall = True
               , configUploadUsername = mempty
               , configUploadPassword = mempty
               }

--
-- * Config file reading
--

loadConfig :: FilePath -> IO ConfigFlags
loadConfig configFile = 
    do defaultConf <- defaultConfigFlags
       let verbosity = configVerbose defaultConf
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

writeDefaultConfigFile :: FilePath -> ConfigFlags -> IO ()
writeDefaultConfigFile file cfg = 
    do createDirectoryIfMissing True (takeDirectory file)
       writeFile file $ showFields configWriteFieldDescrs cfg ++ "\n"

showConfig :: ConfigFlags -> String
showConfig = showFields configFieldDescrs

-- | All config file fields.
configFieldDescrs :: [FieldDescr ConfigFlags]
configFieldDescrs = 
    configWriteFieldDescrs
    ++ map userInstallDirField installDirDescrs
    ++ map globalInstallDirField installDirDescrs

-- | The subset of the config file fields that we write out
-- if the config file is missing.
configWriteFieldDescrs :: [FieldDescr ConfigFlags]
configWriteFieldDescrs =
    [  simpleField "compiler"
                (text . show)   parseCompilerFlavor
                configCompiler (\c cfg -> cfg { configCompiler = c })
    , listField "repos"
                (text . showRepo)                  parseRepo
                configRepos    (\rs cfg -> cfg { configRepos = rs })
    , simpleField "cachedir"
                (text . show)                  (readS_to_P reads)
                configCacheDir    (\d cfg -> cfg { configCacheDir = d })
    , boolField "user-install" configUserInstall (\u cfg -> cfg { configUserInstall = u })
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

installDirDescrs :: [FieldDescr (InstallDirs (Maybe PathTemplate))]
installDirDescrs =
    [ installDirField "prefix"     prefix     (\d ds -> ds { prefix     = d })
    , installDirField "bindir"     bindir     (\d ds -> ds { bindir     = d })
    , installDirField "libdir"     libdir     (\d ds -> ds { libdir     = d })
    , installDirField "libexecdir" libexecdir (\d ds -> ds { libexecdir = d })
    , installDirField "datadir"    datadir    (\d ds -> ds { datadir    = d })
    , installDirField "docdir"     docdir     (\d ds -> ds { docdir     = d })
    , installDirField "htmldir"    htmldir    (\d ds -> ds { htmldir    = d })
    ]


userInstallDirField :: FieldDescr (InstallDirs (Maybe PathTemplate)) -> FieldDescr ConfigFlags
userInstallDirField f = modifyFieldName ("user-"++) $
    liftField configUserInstallDirs 
              (\d cfg -> cfg { configUserInstallDirs = d }) 
              f

globalInstallDirField :: FieldDescr (InstallDirs (Maybe PathTemplate)) -> FieldDescr ConfigFlags
globalInstallDirField f = modifyFieldName ("global-"++) $
    liftField configGlobalInstallDirs 
              (\d cfg -> cfg { configGlobalInstallDirs = d }) 
              f

installDirField :: String 
                -> (InstallDirs (Maybe PathTemplate) -> Maybe PathTemplate) 
                -> (Maybe PathTemplate -> InstallDirs (Maybe PathTemplate) -> InstallDirs (Maybe PathTemplate))
                -> FieldDescr (InstallDirs (Maybe PathTemplate))
installDirField name get set = 
    liftField get set $ field name (text . show) (readS_to_P reads)

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

showRepo :: Repo -> String
showRepo repo = repoName repo ++ ":" ++ repoURL repo

parseRepo :: ReadP r Repo
parseRepo = do name <- munch1 (\c -> isAlphaNum c || c `elem` "_-.")
               char ':'
               url <- munch1 (\c -> isAlphaNum c || c `elem` "+-=._/*()@'$:;&!?")
               return $ Repo { repoName = name, repoURL = url }

