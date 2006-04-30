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


import Control.Exception (bracket_)

import Network.Hackage.CabalInstall.Dependency (getPackages, resolveDependencies)
import Network.Hackage.CabalInstall.Fetch (isFetched, packageFile, fetchPackage)
import Network.Hackage.CabalInstall.Types (ConfigFlags(..), UnresolvedDependency(..)
                                      ,OutputGen(..))
import Network.Hackage.CabalInstall.TarUtils

import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Package (showPackageId, PackageIdentifier)
import Distribution.Compat.FilePath (joinFileName, splitFileName)

import Text.Printf (printf)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing
                        ,removeDirectoryRecursive, copyFile)
import System.Process (runProcess, waitForProcess, terminateProcess)
import System.Exit (ExitCode(..))
import System.Posix.Signals

-- |Installs the packages needed to satisfy a list of dependencies.
install :: ConfigFlags -> [String] -> [UnresolvedDependency] -> IO ()
install cfg globalArgs deps
    = do ipkgs <- getInstalledPackages (configCompiler cfg) (configUser cfg) (configVerbose cfg)
         apkgs <- fmap getPackages (resolveDependencies cfg ipkgs deps)
         mapM_ (installPkg cfg globalArgs) apkgs

-- Fetch a package and output nice messages.
downloadPkg :: ConfigFlags -> PackageIdentifier -> String -> IO FilePath
downloadPkg cfg pkg location
    = do fetched <- isFetched cfg pkg
         if fetched
            then do pkgIsPresent (configOutputGen cfg) pkg
                    return (packageFile cfg pkg)
            else do downloadingPkg (configOutputGen cfg) pkg
                    fetchPackage cfg pkg location

whenFlag :: Bool -> String -> [String] -> [String]
whenFlag True = (:)
whenFlag False = flip const

-- Attach the correct prefix flag to configure commands,
-- correct --user flag to install commands and no options to other commands.
mkPkgOps :: ConfigFlags -> String -> [String] -> [String]
mkPkgOps cfg "configure" ops
    = let ops' = whenFlag (configUser cfg) "--user" ops
      in maybe id (\p -> (:) ("--prefix="++p)) (configPrefix cfg) ops'
mkPkgOps cfg "install" _ops
    | configUserIns cfg = return "--user"
mkPkgOps _cfg _ _ops
    = []

{-|
  Download, build and install a given package with some given flags.

  The process is divided up in a few steps:

    * The package is downloaded to {config-dir}\/packages\/{pkg-id} (if not already there).

    * The fetched tarball is then moved to a temporary directory (\/tmp on linux) and unpacked.

    * The lowest directory with a .cabal file is located and searched for a \'Setup.lhs\' or
      \'Setup.hs\' file.

    * \'runhaskell [Setup script] configure\' is called with the user specified options, \'--user\'
      if the 'configUser' flag is @True@ and \'--prefix=[PREFIX]\' if 'configPrefix' is not @Nothing@.

    * \'runhaskell [Setup script] build\' is called with no options.

    * \'runhaskell [Setup script] install\' is called with the \'--user\' flag if 'configUserIns' is @True@.

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
             setup setupScript cmd
                 = let (path,script) = splitFileName setupScript
                       cmdOps = mkPkgOps cfg cmd (globalArgs++ops)
                   in do executingCmd output runHc (script:cmd:cmdOps)
                         h <- runProcess runHc (script:cmd:cmdOps)
                                                 (Just (tmpDirPath `joinFileName` path))
                                                 Nothing Nothing (cmdStdout output) (cmdStderr output)
                         oldHandler <- installHandler keyboardSignal (Catch (terminateProcess h)) Nothing
                         e <- waitForProcess h
                         installHandler keyboardSignal oldHandler Nothing 
                         case e of
                           ExitFailure err -> cmdFailed output cmd (script:cmd:cmdOps) err
                           _ -> return ()
         bracket_ (createDirectoryIfMissing True tmpDirPath)
                  (removeDirectoryRecursive tmpDirPath)
                  (do copyFile pkgPath tmpPkgPath
                      extractTarFile tarProg tmpPkgPath
                      installUnpackedPkg cfg pkg tmpPkgPath setup
                      return ())
    where runHc = configRunHc cfg
          tarProg = configTarPath cfg
          output = configOutputGen cfg

installUnpackedPkg :: ConfigFlags -> PackageIdentifier -> FilePath
                   -> (String -> String -> IO ()) -> IO ()
installUnpackedPkg cfg pkgId tarFile setup
    = do tarFiles <- tarballGetFiles tarProg tarFile
         let cabalFile = locateFileExt tarFiles "cabal"
         case cabalFile of
           Just f -> let (path,_) = splitFileName f
                         mbScript = locateFile tarFiles path ["Setup.lhs", "Setup.hs"]
                     in case mbScript of
                          Just script
                              -> do buildingPkg output pkgId
                                    stepConfigPkg output pkgId
                                    setup script "configure"
                                    stepBuildPkg output pkgId
                                    setup script "build"
                                    stepInstallPkg output pkgId
                                    setup script "install"
                                    stepFinishedPkg output pkgId
                                    return ()
                          Nothing
                              -> noSetupScript output pkgId
           Nothing -> noCabalFile output pkgId
    where output = configOutputGen cfg
          tarProg = configTarPath cfg
