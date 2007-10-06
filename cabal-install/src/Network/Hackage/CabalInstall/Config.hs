-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Config
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling saved state such as known packages, known servers and downloaded packages.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Config
    ( packagesDirectory
    , repoCacheDir
    , packageFile
    , packageDir
    , getKnownPackages

    , pkgURL
    , defaultConfigFile
    , loadConfig
    , programConfiguration
    , findCompiler
    ) where

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException),evaluate)
import Control.Monad.Error (mplus, filterM) -- Using Control.Monad.Error to get the Error instance for IO.
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isAlphaNum, toLower)
import Data.List (intersperse)
import Data.Maybe (mapMaybe, fromMaybe)
import System.Directory (Permissions (..), getPermissions, createDirectoryIfMissing
	                    ,getTemporaryDirectory)
import System.IO.Error (isDoesNotExistError)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe
import Text.PrettyPrint.HughesPJ (text)

import Distribution.Compat.ReadP (ReadP, char, munch1, readS_to_P)
import Distribution.Compiler (CompilerFlavor(..), defaultCompilerFlavor)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PackageDescription (GenericPackageDescription(..)
                                       , PackageDescription(..)
                                       , parsePackageDescription, ParseResult(..))
import Distribution.ParseUtils (FieldDescr, simpleField, listField)
import Distribution.Simple.Compiler (Compiler)
import qualified Distribution.Simple.Configure as Configure (configCompiler)
import Distribution.Simple.InstallDirs (InstallDirTemplates(..), defaultInstallDirs)
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Version (Dependency, showVersion)
import Distribution.Verbosity
import System.FilePath ((</>), takeExtension, (<.>))
import System.Directory

import Network.Hackage.CabalInstall.Tar (readTarArchive, tarFileName)
import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen(..), PkgInfo (..), Repo(..))
import Network.Hackage.CabalInstall.Utils


-- FIXME: remove imports below, only for defaultOutputGen

import Control.Monad (guard, mplus, when)

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen (..)
                                          , ResolvedPackage (..))

import qualified Distribution.Simple.Configure as Configure (configCompiler)
import Distribution.Simple.Program
import Distribution.ParseUtils (showDependency)
import Distribution.Package (showPackageId)
import Distribution.Version (VersionRange(..))
import Distribution.Verbosity
import System.FilePath ((</>))

import Text.Printf (printf)
import System.IO (openFile, IOMode (..))
import System.Directory (doesFileExist, getHomeDirectory, getAppUserDataDirectory)
import Data.Maybe (fromMaybe)





-- |Name of the packages directory.
packagesDirectoryName :: FilePath
packagesDirectoryName = "packages"

-- | Full path to the packages directory.
packagesDirectory :: ConfigFlags -> FilePath
packagesDirectory cfg = configCacheDir cfg </> packagesDirectoryName

-- | Full path to the local cache directory for a repository.
repoCacheDir :: ConfigFlags -> Repo -> FilePath
repoCacheDir cfg repo = packagesDirectory cfg </> repoName repo

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

{-|
  Structure with default responses to various events.
-}
defaultOutputGen :: Verbosity -> IO OutputGen
defaultOutputGen verbosity
    = do (outch,errch) <- do guard (verbosity <= normal)
                             nullOut <- openFile ("/"</>"dev"</>"null") AppendMode
                             nullErr <- openFile ("/"</>"dev"</>"null") AppendMode
                             return (Just nullOut, Just nullErr)
                         `mplus` return (Nothing,Nothing)
         return OutputGen
                { prepareInstall = \_pkgs -> return ()
                , pkgIsPresent   = printf "'%s' is present.\n" . showPackageId
                , downloadingPkg = printf "Downloading '%s'...\n" . showPackageId
                , executingCmd   = \cmd args
                                 -> when (verbosity > silent) $ printf "Executing: '%s %s'\n" cmd (unwords args)
                , cmdFailed      = \cmd args errno
                                 -> error (printf "Command failed: '%s %s'. Errno: %d\n" cmd (unwords args) errno)
                , buildingPkg    = printf "Building '%s'\n" . showPackageId
                , stepConfigPkg  = const (printf "  Configuring...\n")
                , stepBuildPkg   = const (printf "  Building...\n")
                , stepInstallPkg = const (printf "  Installing...\n")
                , stepFinishedPkg= const (printf "  Done.\n")
                , noSetupScript  = const (error "Couldn't find a setup script in the tarball.")
                , noCabalFile    = const (error "Couldn't find a .cabal file in the tarball")
                , gettingPkgList = \serv ->
                                   when (verbosity > silent) (printf "Downloading package list from server '%s'\n" serv)
                , showPackageInfo = showPkgInfo
                , showOtherPackageInfo = showOtherPkg
                , cmdStdout      = outch
                , cmdStderr      = errch 
                , message        = \v s -> when (verbosity >= v) (putStrLn s)
                }
    where showOtherPkg mbPkg dep
              = do printf "  Package:     '%s'\n" (show $ showDependency dep)
                   case mbPkg of
                     Nothing  -> printf "    Not available!\n\n"
                     Just pkg -> do printf "    Using:     %s\n" (showPackageId pkg)
                                    printf "    Installed: Yes\n\n"
          showPkgInfo mbPath installed ops dep (pkg,repo,deps)
              = do printf "  Package:     '%s'\n" (show $ showDependency dep)
                   printf "    Using:     %s\n" (showPackageId pkg)
                   printf "    Installed: %s\n" (if installed then "Yes" else "No")
                   printf "    Depends:   %s\n" (showDeps deps)
                   printf "    Options:   %s\n" (unwords ops)
                   printf "    Location:  %s\n" (pkgURL pkg repo)
                   printf "    Local:     %s\n\n" (fromMaybe "*Not downloaded" mbPath)
          showDeps = show . map showDep
          showDep dep = show (showDependency (fulfilling dep))



-- | Generate the URL of the tarball for a given package.
pkgURL :: PackageIdentifier -> Repo -> String
pkgURL pkg repo = joinWith "/" [repoURL repo, pkgName pkg, showVersion (pkgVersion pkg), showPackageId pkg] 
                           ++ ".tar.gz"
                      where joinWith tok = concat . intersperse tok

--
-- * Compiler and programs
--

-- FIXME: should look at config
programConfiguration :: ConfigFlags -> IO ProgramConfiguration
programConfiguration cfg = return defaultProgramConfiguration 

findCompiler :: ConfigFlags -> IO (Compiler, ProgramConfiguration)
findCompiler cfg = 
    do conf <- programConfiguration cfg
       Configure.configCompiler 
                    (Just (configCompiler cfg)) 
                    Nothing Nothing conf (configVerbose cfg)


--
-- * Default config
--

defaultConfigDir :: IO FilePath
defaultConfigDir = getAppUserDataDirectory "cabal"

defaultConfigFile :: IO FilePath
defaultConfigFile = do dir <- defaultConfigDir
                       return $ dir </> "config"

defaultCacheDir :: IO FilePath
defaultCacheDir = defaultConfigDir

defaultCompiler :: CompilerFlavor
defaultCompiler = fromMaybe GHC defaultCompilerFlavor

defaultConfigFlags :: IO ConfigFlags
defaultConfigFlags = 
    do installDirs <- defaultInstallDirs defaultCompiler True
       cacheDir    <- defaultCacheDir
       outputGen <- defaultOutputGen normal -- FIXME: get rid of OutputGen
       return $ ConfigFlags 
               { configCompiler    = defaultCompiler
               , configInstallDirs = installDirs
               , configCacheDir    = cacheDir
               , configRepos       = [Repo "hackage.haskell.org" "http://hackage.haskell.org/packages/archive"]
               , configOutputGen   = outputGen
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
                       -- FIXME: write config file with defaults
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
                           fail $ "Error parsing config file " ++ configFile ++ ": " ++ showPError err

configFieldDescrs :: [FieldDescr ConfigFlags]
configFieldDescrs =
    [  simpleField "compiler"
                (text . show)   parseCompilerFlavor
                configCompiler (\c cfg -> cfg { configCompiler = c })
    , listField "repos"
                (text . showRepo)                  parseRepo
                configRepos    (\rs cfg -> cfg { configRepos = rs })
    , simpleField "prefix"
                (text . show)  (readS_to_P reads) 
                (prefixDirTemplate . configInstallDirs) (\d -> setInstallDir (\ds -> ds { prefixDirTemplate = d }))
    ]

setInstallDir :: (InstallDirTemplates -> InstallDirTemplates) -> ConfigFlags -> ConfigFlags
setInstallDir f cfg = cfg { configInstallDirs = f (configInstallDirs cfg) }


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

