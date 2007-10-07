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
    , getKnownPackages
    , message
    , pkgURL
    , defaultConfigFile
    , loadConfig
    , findCompiler
    ) where

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException))
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isAlphaNum, toLower)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath ((</>), takeDirectory, takeExtension, (<.>))
import System.IO.Error (isDoesNotExistError)
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint.HughesPJ (text)

import Distribution.Compat.ReadP (ReadP, char, munch1, readS_to_P)
import Distribution.Compiler (CompilerFlavor(..), defaultCompilerFlavor)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PackageDescription (GenericPackageDescription(..)
                                       , PackageDescription(..)
                                       , parsePackageDescription, ParseResult(..))
import Distribution.ParseUtils (FieldDescr, simpleField, listField, liftField, field)
import Distribution.Simple.Compiler (Compiler, PackageDB(..))
import Distribution.Simple.Configure (getInstalledPackages)
import qualified Distribution.Simple.Configure as Configure (configCompiler)
import Distribution.Simple.InstallDirs (InstallDirTemplates(..), PathTemplate, toPathTemplate, defaultInstallDirs)
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Version (Dependency, showVersion)
import Distribution.Verbosity (Verbosity, normal)

import Hackage.Tar (readTarArchive, tarFileName)
import Hackage.Types (ConfigFlags (..), PkgInfo (..), Repo(..))
import Hackage.Utils


-- | Full path to the local cache directory for a repository.
repoCacheDir :: ConfigFlags -> Repo -> FilePath
repoCacheDir cfg repo = configCacheDir cfg </> repoName repo

-- |Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
packageFile :: ConfigFlags -> PackageIdentifier -> Repo -> FilePath
packageFile cfg pkg repo = packageDir cfg pkg repo
                           </> showPackageId pkg 
                           <.> "tar.gz"

-- |Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifer@ is stored.
packageDir :: ConfigFlags -> PackageIdentifier -> Repo -> FilePath
packageDir cfg pkg repo = repoCacheDir cfg repo
                      </> pkgName pkg
                      </> showVersion (pkgVersion pkg)

listInstalledPackages :: ConfigFlags -> Compiler -> ProgramConfiguration -> IO [PackageIdentifier]
listInstalledPackages cfg comp conf =
    do Just ipkgs <- getInstalledPackages
                         (configVerbose cfg) comp
                         (if configUserInstall cfg then UserPackageDB
                                               else GlobalPackageDB)
                         conf
       return ipkgs

getKnownPackages :: ConfigFlags -> IO [PkgInfo]
getKnownPackages cfg
    = fmap concat $ mapM (readRepoIndex cfg) $ configRepos cfg

readRepoIndex :: ConfigFlags -> Repo -> IO [PkgInfo]
readRepoIndex cfg repo =
    do let indexFile = repoCacheDir cfg repo </> "00-index.tar"
       fmap (parseRepoIndex repo) (BS.readFile indexFile)
          `catch` (\e
                 -> do hPutStrLn stderr ("Warning: Problem opening package list '"
                                          ++ indexFile ++ "'.")
                       case e of
                         IOException ioe | isDoesNotExistError ioe ->
                           hPutStrLn stderr "File doesn't exist. Run 'cabal-install update' to create the package list."
                         _ -> hPutStrLn stderr ("Error: " ++ (show e))
                       return [])

parseRepoIndex :: Repo -> ByteString -> [PkgInfo]
parseRepoIndex repo s =
    do (hdr, content) <- readTarArchive s
       if takeExtension (tarFileName hdr) == ".cabal"
         then case parsePackageDescription (BS.unpack content) of
                    ParseOk _ descr -> return $ PkgInfo { 
                                                         pkgRepo = repo,
                                                         pkgDesc = descr
                                                        }
                    _               -> error $ "Couldn't read cabal file " ++ show (tarFileName hdr)
         else fail "Not a .cabal file"

message :: ConfigFlags -> Verbosity -> String -> IO ()
message cfg v s = when (configVerbose cfg >= v) (putStrLn s)

-- | Generate the URL of the tarball for a given package.
pkgURL :: PackageIdentifier -> Repo -> String
pkgURL pkg repo = joinWith "/" [repoURL repo, pkgName pkg, showVersion (pkgVersion pkg), showPackageId pkg] 
                           ++ ".tar.gz"
                      where joinWith tok = concat . intersperse tok

--
-- * Compiler and programs
--

-- FIXME: should use --with flags
findCompiler :: ConfigFlags -> IO (Compiler, ProgramConfiguration)
findCompiler cfg = Configure.configCompiler 
                     (Just (configCompiler cfg)) 
                     Nothing Nothing defaultProgramConfiguration (configVerbose cfg)

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

defaultConfigFlags :: IO ConfigFlags
defaultConfigFlags = 
    do defaultPrefix <- defaultCabalDir
       installDirs <- defaultInstallDirs defaultCompiler True
       cacheDir    <- defaultCacheDir
       return $ ConfigFlags 
               { configCompiler    = defaultCompiler
               , configInstallDirs = installDirs { prefixDirTemplate = toPathTemplate defaultPrefix }
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
                       ParseOk ws dummyConf -> 
                           do mapM_ (hPutStrLn stderr . ("Config file warning: " ++)) ws
                              -- There is a data dependency within the config file.
                              -- The default installation paths depend on the compiler.
                              -- Hence we need to do two passes through the config file.
                              installDirs <- defaultInstallDirs (configCompiler dummyConf) True
                              let conf = defaultConf { configInstallDirs = installDirs }
                              case parseBasicStanza configFieldDescrs conf inp of
                                ParseOk _ conf' -> return conf'
                       ParseFailed err -> 
                           do hPutStrLn stderr $ "Error parsing config file " 
                                            ++ configFile ++ ": " ++ showPError err
                              hPutStrLn stderr $ "Using default configuration."
                              return defaultConf

writeDefaultConfigFile :: FilePath -> ConfigFlags -> IO ()
writeDefaultConfigFile file cfg = 
    do createDirectoryIfMissing True (takeDirectory file)
       writeFile file $ showFields configWriteFieldDescrs cfg

-- | All config file fields.
configFieldDescrs :: [FieldDescr ConfigFlags]
configFieldDescrs =
    [ installDirField "bindir" binDirTemplate (\d ds -> ds { binDirTemplate = d })
    , installDirField "libdir" libDirTemplate (\d ds -> ds { libDirTemplate = d })
    , installDirField "libexecdir" libexecDirTemplate (\d ds -> ds { libexecDirTemplate = d })
    , installDirField "datadir" dataDirTemplate (\d ds -> ds { dataDirTemplate = d })
    , installDirField "docdir" docDirTemplate (\d ds -> ds { docDirTemplate = d })
    , installDirField "htmldir" htmlDirTemplate (\d ds -> ds { htmlDirTemplate = d })
    ] ++ configWriteFieldDescrs


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
    , installDirField "prefix" prefixDirTemplate (\d ds -> ds { prefixDirTemplate = d })
    ] 

installDirField :: String 
                -> (InstallDirTemplates -> PathTemplate) 
                -> (PathTemplate -> InstallDirTemplates -> InstallDirTemplates)
                -> FieldDescr ConfigFlags
installDirField name get set = 
    liftField (get . configInstallDirs) 
               (\d cfg -> cfg { configInstallDirs = set d (configInstallDirs cfg) }) $
               field name (text . show) (readS_to_P reads)

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
               url <- munch1 (const True)
               return $ Repo { repoName = name, repoURL = url }

