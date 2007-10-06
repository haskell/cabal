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
    , installPackages
    , installPkg -- :: ConfigFlags -> (PackageIdentifier,[String],String) -> IO ()
    ) where

import Data.List  (elemIndex)
import Data.Maybe (fromJust)
import Debug.Trace
import Control.Exception (bracket_)

import Network.Hackage.CabalInstall.Config (programConfiguration, findCompiler)
import Network.Hackage.CabalInstall.Dependency (getPackages, resolveDependencies
                                               , listInstalledPackages)
import Network.Hackage.CabalInstall.Fetch (isFetched, packageFile, fetchPackage)
import Network.Hackage.CabalInstall.Tar (extractTarGzFile)
import Network.Hackage.CabalInstall.Types (ConfigFlags(..), UnresolvedDependency(..)
                                      ,OutputGen(..), Repo(..))

import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Simple.InstallDirs (InstallDirs(..), absoluteInstallDirs)
import Distribution.Simple.SetupWrapper (setupWrapper)
import Distribution.Simple.Setup (CopyDest(..))
import Distribution.Package (showPackageId, PackageIdentifier)
import Distribution.Verbosity

import System.FilePath ((</>), splitFileName)

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
    = do ipkgs <- listInstalledPackages cfg
         resolvedDeps <- resolveDependencies cfg ipkgs deps
         let apkgs = getPackages resolvedDeps
         if null apkgs
           then putStrLn "All requested packages already installed. Nothing to do."
           else installPackages cfg globalArgs apkgs

-- Fetch a package and output nice messages.
downloadPkg :: ConfigFlags -> PackageIdentifier -> Repo -> IO FilePath
downloadPkg cfg pkg repo
    = do fetched <- isFetched cfg pkg repo
         if fetched
            then do pkgIsPresent (configOutputGen cfg) pkg
                    return (packageFile cfg pkg repo)
            else fetchPackage cfg pkg repo

-- Attach the correct prefix flag to configure commands,
-- correct --user flag to install commands and no options to other commands.
mkPkgOps :: ConfigFlags -> Compiler -> PackageIdentifier -> String -> [String] -> [String]
mkPkgOps cfg comp pkgId cmd ops = verbosity ++
  case cmd of
    "configure" -> user ++ installDirFlags installDirs ++ ops
    "install"   -> user
    _ -> []
 where verbosity = ["--verbose=" ++ showForCabal (configVerbose cfg)]
       user = if configUserInstall cfg then ["--user"] else []
       installDirs = absoluteInstallDirs pkgId (compilerId comp) NoCopyDest (configInstallDirs cfg)
       showForCabal v = show$ fromJust$ elemIndex v [silent,normal,verbose,deafening]

installDirFlags :: InstallDirs FilePath -> [String]
installDirFlags dirs =
    [flag "prefix" prefix,
     flag "bindir" bindir,
     flag "libdir" libdir,
--     flag "dynlibdir" dynlibdir, -- not accepted as argument by cabal?
     flag "libexecdir" libexecdir,
--     flag "progdir" progdir, -- not accepted as argument by cabal?
--     flag "includedir" includedir, -- not accepted as argument by cabal?
     flag "datadir" datadir,
     flag "docdir" docdir,
     flag "htmldir" htmldir]
  where flag s f = "--" ++ s ++ "=" ++ f dirs

installPackages :: ConfigFlags
                -> [String] -- ^Options which will be parse to every package.
                -> [(PackageIdentifier,[String],Repo)] -- ^(Package, list of configure options, package location)
                -> IO ()
installPackages cfg globalArgs pkgs =
    do (comp, _) <- findCompiler cfg
       mapM_ (installPkg cfg comp globalArgs) pkgs

{-|
  Download, build and install a given package with some given flags.

  The process is divided up in a few steps:

    * The package is downloaded to {config-dir}\/packages\/{pkg-id} (if not already there).

    * The fetched tarball is then moved to a temporary directory (\/tmp on linux) and unpacked.

    * setupWrapper (equivalent to cabal-setup) is called with the options
      \'configure\' and the user specified options, \'--user\'
      if the 'configUser' flag is @True@ and install directory flags depending on @configInstallDirs@.

    * setupWrapper \'build\' is called with no options.

    * setupWrapper \'install\' is called with the \'--user\' flag if 'configUserInstall' is @True@.

    * The installation finishes by deleting the unpacked tarball.
-} 
installPkg :: ConfigFlags
           -> Compiler
           -> [String] -- ^Options which will be parse to every package.
           -> (PackageIdentifier,[String],Repo) -- ^(Package, list of configure options, package location)
           -> IO ()
installPkg cfg comp globalArgs (pkg,ops,repo)
    = do pkgPath <- downloadPkg cfg pkg repo
         tmp <- getTemporaryDirectory
         let tmpDirPath = tmp </> printf "TMP%sTMP" (showPackageId pkg)
             setup cmd
                 = do let cmdOps = mkPkgOps cfg comp pkg cmd (globalArgs++ops)
                          path = tmpDirPath </> showPackageId pkg
                      message output deafening $ 
                                 unwords ["setupWrapper", show (cmd:cmdOps), show path]
                      setupWrapper (cmd:cmdOps) (Just path)
         bracket_ (createDirectoryIfMissing True tmpDirPath)
                  (removeDirectoryRecursive tmpDirPath)
                  (do message output deafening (printf "Extracting %s..." pkgPath)
                      extractTarGzFile (Just tmpDirPath) pkgPath
                      installUnpackedPkg cfg pkg setup
                      return ())
    where output = configOutputGen cfg

installUnpackedPkg :: ConfigFlags -> PackageIdentifier
                   -> (String -> IO ()) -> IO ()
installUnpackedPkg cfg pkgId setup
    = do buildingPkg output pkgId
         stepConfigPkg output pkgId
         setup "configure"
         stepBuildPkg output pkgId
         setup "build"
         stepInstallPkg output pkgId
         setup "install"
         stepFinishedPkg output pkgId
         return ()
    where output = configOutputGen cfg
