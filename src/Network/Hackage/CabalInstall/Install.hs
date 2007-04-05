-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Install
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Install
    ( install    -- :: ConfigFlags -> [UnresolvedDependency] -> IO ()
    , installPkg -- :: ConfigFlags -> (PackageIdentifier,[String],String) -> IO ()
    ) where


import Debug.Trace
import Control.Exception (bracket_)

import Network.Hackage.CabalInstall.Dependency (getPackages, resolveDependencies)
import Network.Hackage.CabalInstall.Fetch (isFetched, packageFile, fetchPackage)
import Network.Hackage.CabalInstall.Types (ConfigFlags(..), UnresolvedDependency(..)
                                      ,OutputGen(..))
import Network.Hackage.CabalInstall.TarUtils

import Distribution.SetupWrapper (setupWrapper)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Package (showPackageId, PackageIdentifier)
import Distribution.Compat.FilePath (joinFileName, splitFileName)

import Data.Maybe (fromMaybe, maybeToList)
import Text.Printf (printf, PrintfType)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing
                        ,removeDirectoryRecursive, copyFile)
import System.IO (hPutStrLn, stderr)
import System.Process (runProcess, waitForProcess, terminateProcess)
import System.Exit (ExitCode(..))
import System.Posix.Signals

-- |Installs the packages needed to satisfy a list of dependencies.
install :: ConfigFlags -> [String] -> [UnresolvedDependency] -> IO ()
install cfg globalArgs deps
    = do ipkgs <- getInstalledPackages (configCompiler cfg) (configUserIns cfg) (configVerbose cfg)
         resolvedDeps <- resolveDependencies cfg ipkgs deps
         let apkgs = getPackages resolvedDeps
         if null apkgs
           then putStrLn "All requested packages already installed. Nothing to do."
           else mapM_ (installPkg cfg globalArgs) apkgs

-- Fetch a package and output nice messages.
downloadPkg :: ConfigFlags -> PackageIdentifier -> String -> IO FilePath
downloadPkg cfg pkg location
    = do fetched <- isFetched cfg pkg
         if fetched
            then do pkgIsPresent (configOutputGen cfg) pkg
                    return (packageFile cfg pkg)
            else do downloadingPkg (configOutputGen cfg) pkg
                    fetchPackage cfg pkg location

-- Attach the correct prefix flag to configure commands,
-- correct --user flag to install commands and no options to other commands.
mkPkgOps :: ConfigFlags -> String -> [String] -> [String]
mkPkgOps cfg cmd ops = verbosity ++
  case cmd of
    "configure" -> user ++ prefix ++ ops
    "install"   -> user
    _ -> []
 where verbosity = ["--verbose=" ++ show (configVerbose cfg)]
       user = if configUserIns cfg then ["--user"] else []
       prefix = maybeToList (fmap ("--prefix=" ++) (configPrefix cfg))

{-|
  Download, build and install a given package with some given flags.

  The process is divided up in a few steps:

    * The package is downloaded to {config-dir}\/packages\/{pkg-id} (if not already there).

    * The fetched tarball is then moved to a temporary directory (\/tmp on linux) and unpacked.

    * setupWrapper (equivalent to cabal-setup) is called with the options
      \'configure\' and the user specified options, \'--user\'
      if the 'configUser' flag is @True@ and \'--prefix=[PREFIX]\' if 'configPrefix' is not @Nothing@.

    * setupWrapper \'build\' is called with no options.

    * setupWrapper \'install\' is called with the \'--user\' flag if 'configUserIns' is @True@.

    * The installation finishes by deleting the unpacked tarball.
-} 
installPkg :: ConfigFlags
           -> [String] -- ^Options which will be parse to every package.
           -> (PackageIdentifier,[String],String) -- ^(Package, list of configure options, package location)
           -> IO ()
installPkg cfg globalArgs (pkg,ops,location)
    = do pkgPath <- downloadPkg cfg pkg location
         tmp <- getTemporaryDirectory
         let tmpDirPath = tmp `joinFileName` printf "TMP%sTMP" (showPackageId pkg)
             tmpPkgPath = tmpDirPath `joinFileName` printf "TAR%s.tgz" (showPackageId pkg)
             setup cmd
                 = let cmdOps = mkPkgOps cfg cmd (globalArgs++ops)
                       path = tmpDirPath `joinFileName` showPackageId pkg
                   in do message output 3 $ unwords ["setupWrapper", show (cmd:cmdOps), show path]
                         setupWrapper (cmd:cmdOps) (Just path)
         bracket_ (createDirectoryIfMissing True tmpDirPath)
                  (removeDirectoryRecursive tmpDirPath)
                  (do copyFile pkgPath tmpPkgPath
                      message output 3 (printf "Extracting %s..." tmpPkgPath)
                      extractTarFile tarProg tmpPkgPath
                      installUnpackedPkg cfg pkg tmpPkgPath setup
                      return ())
    where runHc = configRunHc cfg
          tarProg = configTarPath cfg
          output = configOutputGen cfg

installUnpackedPkg :: ConfigFlags -> PackageIdentifier -> FilePath
                   -> (String -> IO ()) -> IO ()
installUnpackedPkg cfg pkgId tarFile setup
    = do tarFiles <- tarballGetFiles tarProg tarFile
         let cabalFile = locateFileExt tarFiles "cabal"
         case cabalFile of
           Just f -> let (path,_) = splitFileName f
                     in do buildingPkg output pkgId
                           stepConfigPkg output pkgId
                           setup "configure"
                           stepBuildPkg output pkgId
                           setup "build"
                           stepInstallPkg output pkgId
                           setup "install"
                           stepFinishedPkg output pkgId
                           return ()
           Nothing -> noCabalFile output pkgId
    where output = configOutputGen cfg
          tarProg = configTarPath cfg
