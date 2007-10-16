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
    , message
    , pkgURL
    , defaultConfigFile
    , loadConfig
    , showConfig
    , findCompiler
    ) where

import Prelude hiding (catch)
import Control.Monad (when)
import Data.Char (isAlphaNum, toLower)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath ((</>), takeDirectory, (<.>))
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint.HughesPJ (text)

import Distribution.Compat.ReadP (ReadP, char, munch1, readS_to_P)
import Distribution.Compiler (CompilerFlavor(..), defaultCompilerFlavor)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PackageDescription (ParseResult(..))
import Distribution.ParseUtils (FieldDescr(..), simpleField, listField, liftField, field)
import Distribution.Simple.Compiler (Compiler, PackageDB(..))
import Distribution.Simple.Configure (getInstalledPackages)
import qualified Distribution.Simple.Configure as Configure (configCompiler)
import Distribution.Simple.InstallDirs (InstallDirTemplates(..), PathTemplate, toPathTemplate, defaultInstallDirs)
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Version (showVersion)
import Distribution.Verbosity (Verbosity, normal)

import Hackage.Types (ConfigFlags (..), PkgInfo (..), Repo(..), pkgInfoId)
import Hackage.Utils


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

message :: ConfigFlags -> Verbosity -> String -> IO ()
message cfg v s = when (configVerbose cfg >= v) (putStrLn s)

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

defaultUserInstallDirs :: CompilerFlavor -> IO InstallDirTemplates
defaultUserInstallDirs compiler =
    do installDirs <- defaultInstallDirs compiler True
       userPrefix <- defaultCabalDir
       return $ installDirs { prefixDirTemplate = toPathTemplate userPrefix }

defaultGlobalInstallDirs :: CompilerFlavor -> IO InstallDirTemplates
defaultGlobalInstallDirs compiler = defaultInstallDirs compiler True

defaultConfigFlags :: IO ConfigFlags
defaultConfigFlags = 
    do userInstallDirs   <- defaultUserInstallDirs defaultCompiler
       globalInstallDirs <- defaultGlobalInstallDirs defaultCompiler
       cacheDir    <- defaultCacheDir
       return $ ConfigFlags 
               { configCompiler    = defaultCompiler
               , configCompilerPath = Nothing
               , configHcPkgPath   = Nothing
               , configUserInstallDirs = userInstallDirs
               , configGlobalInstallDirs = globalInstallDirs
               , configCacheDir    = cacheDir
               , configRepos       = [Repo "hackage.haskell.org" "http://hackage.haskell.org/packages/archive"]
               , configVerbose     = normal
               , configUserInstall = True
               }

--
-- * Config file reading
--

loadConfig :: FilePath -> IO ConfigFlags
loadConfig configFile = 
    do defaultConf <- defaultConfigFlags
       minp <- readFileIfExists configFile
       case minp of
         Nothing -> do hPutStrLn stderr $ "Config file " ++ configFile ++ " not found."
                       hPutStrLn stderr $ "Writing default configuration to " ++ configFile ++ "."
                       writeDefaultConfigFile configFile defaultConf
                       return defaultConf
         Just inp -> case parseBasicStanza configFieldDescrs defaultConf inp of
                       ParseOk ws conf -> 
                           do mapM_ (hPutStrLn stderr . ("Config file warning: " ++)) ws
                              return conf
                       ParseFailed err -> 
                           do hPutStrLn stderr $ "Error parsing config file " 
                                            ++ configFile ++ ": " ++ showPError err
                              hPutStrLn stderr $ "Using default configuration."
                              return defaultConf

writeDefaultConfigFile :: FilePath -> ConfigFlags -> IO ()
writeDefaultConfigFile file cfg = 
    do createDirectoryIfMissing True (takeDirectory file)
       writeFile file $ showFields configWriteFieldDescrs cfg

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
    ] 

installDirDescrs :: [FieldDescr InstallDirTemplates]
installDirDescrs =
    [ installDirField "prefix"     prefixDirTemplate  (\d ds -> ds { prefixDirTemplate  = d })
    , installDirField "bindir"     binDirTemplate     (\d ds -> ds { binDirTemplate     = d })
    , installDirField "libdir"     libDirTemplate     (\d ds -> ds { libDirTemplate     = d })
    , installDirField "libexecdir" libexecDirTemplate (\d ds -> ds { libexecDirTemplate = d })
    , installDirField "datadir"    dataDirTemplate    (\d ds -> ds { dataDirTemplate    = d })
    , installDirField "docdir"     docDirTemplate     (\d ds -> ds { docDirTemplate     = d })
    , installDirField "htmldir"    htmlDirTemplate    (\d ds -> ds { htmlDirTemplate    = d })
    ]


userInstallDirField :: FieldDescr InstallDirTemplates -> FieldDescr ConfigFlags
userInstallDirField f = modifyFieldName ("user-"++) $
    liftField configUserInstallDirs 
              (\d cfg -> cfg { configUserInstallDirs = d }) 
              f

globalInstallDirField :: FieldDescr InstallDirTemplates -> FieldDescr ConfigFlags
globalInstallDirField f = modifyFieldName ("global-"++) $
    liftField configGlobalInstallDirs 
              (\d cfg -> cfg { configGlobalInstallDirs = d }) 
              f

installDirField :: String 
                -> (InstallDirTemplates -> PathTemplate) 
                -> (PathTemplate -> InstallDirTemplates -> InstallDirTemplates)
                -> FieldDescr InstallDirTemplates
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

