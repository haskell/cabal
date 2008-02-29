-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Install
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Hackage.Install
    ( install
    ) where

import Data.Monoid (Monoid(mconcat))
import Control.Exception as Exception (bracket_, handle)
import Control.Monad (when)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing
                        ,removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>),(<.>))

import Hackage.Dependency (resolveDependencies, resolveDependenciesLocal, packagesToInstall)
import Hackage.Fetch (fetchPackage)
import qualified Hackage.Info as Info
import qualified Hackage.IndexUtils as IndexUtils
import qualified Hackage.DepGraph as DepGraph
import Hackage.Setup (InstallFlags(..))
import Hackage.Tar (extractTarGzFile)
import Hackage.Types (UnresolvedDependency(..), PkgInfo(..), FlagAssignment,
                      Repo)
import Hackage.Utils (showDependencies)

import Distribution.Simple.Compiler (Compiler, PackageDB(..))
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Command (commandShowOptions)
import Distribution.Simple.SetupWrapper (setupWrapper)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils (defaultPackageDesc)
import Distribution.Package (showPackageId, PackageIdentifier(..), Package(..))
import Distribution.PackageDescription (GenericPackageDescription(packageDescription))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils as Utils (notice, info, debug, die)
import Distribution.Verbosity (Verbosity)

data BuildResult = DependentFailed PackageIdentifier
                 | UnpackFailed
                 | ConfigureFailed
                 | BuildFailed
                 | InstallFailed
                 | BuildOk

-- |Installs the packages needed to satisfy a list of dependencies.
install :: Verbosity
        -> PackageDB
        -> [Repo]
        -> Compiler
        -> ProgramConfiguration
        -> Cabal.ConfigFlags
        -> InstallFlags
        -> [UnresolvedDependency]
        -> IO ()
install verbosity packageDB repos comp conf configFlags installFlags deps = do
  let dryRun = Cabal.fromFlag (installDryRun installFlags)
  buildResults <- if null deps 
    then installLocalPackage verbosity packageDB repos comp conf configFlags dryRun
    else installRepoPackages verbosity packageDB repos comp conf configFlags dryRun deps
  case filter (buildFailed . snd) buildResults of
    []     -> return () --TODO: return the build results
    failed -> die $ "Error: some packages failed to install:\n"
      ++ unlines
         [ showPackageId pkgid ++ case reason of
           DependentFailed pkgid' -> " depends on " ++ showPackageId pkgid'
                                  ++ " which failed to install."
           UnpackFailed    -> " failed while unpacking the package."
           ConfigureFailed -> " failed during the configure step."
           BuildFailed     -> " failed during the building phase."
           InstallFailed   -> " failed during the final install step."
           _ -> ""
         | (pkgid, reason) <- failed ]

  where buildFailed BuildOk = False
        buildFailed _       = True

-- | Install the unpacked package in the current directory, and all its dependencies.
installLocalPackage :: Verbosity
                    -> PackageDB
                    -> [Repo]
                    -> Compiler
                    -> ProgramConfiguration
                    -> Cabal.ConfigFlags
                    -> Bool -- ^Dry run
                    -> IO [(PackageIdentifier, BuildResult)]
installLocalPackage verbosity packageDB repos comp conf configFlags dryRun =
   do cabalFile <- defaultPackageDesc verbosity
      desc <- readPackageDescription verbosity cabalFile
      Just installed <- getInstalledPackages verbosity comp packageDB conf
      available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
      let resolvedDeps = resolveDependenciesLocal comp installed available desc
                           (Cabal.configConfigurationsFlags configFlags)
      details <- mapM Info.infoPkg (Info.flattenResolvedDependencies resolvedDeps)
      info verbosity $ unlines (map ("  "++) (concat details))
      buildResults <- case packagesToInstall resolvedDeps of
        Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
        Right pkgs   -> do
            if dryRun
              then printDryRun verbosity pkgs >> return []
              else installPackages verbosity configFlags pkgs
      if dryRun
        then return []
        --TODO: don't run if buildResult failed
        else do buildResult <- installUnpackedPkg verbosity configFlags Nothing
                return ((packageId (packageDescription desc), buildResult) : buildResults)

installRepoPackages :: Verbosity
                    -> PackageDB
                    -> [Repo]
                    -> Compiler
                    -> ProgramConfiguration
                    -> Cabal.ConfigFlags
                    -> Bool -- ^Dry run
                    -> [UnresolvedDependency]
                    -> IO [(PackageIdentifier, BuildResult)]
installRepoPackages verbosity packageDB repos comp conf configFlags dryRun deps =
    do Just installed <- getInstalledPackages verbosity comp packageDB conf
       available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
       deps' <- IndexUtils.disambiguateDependencies available deps
       let resolvedDeps = resolveDependencies comp installed available deps'
       details <- mapM Info.infoPkg (Info.flattenResolvedDependencies resolvedDeps)
       info verbosity $ unlines (map ("  "++) (concat details))
       case packagesToInstall resolvedDeps of
         Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
         Right pkgs
           | DepGraph.empty pkgs -> notice verbosity
                     "All requested packages already installed. Nothing to do."
                     >> return []
           | dryRun -> do
                printDryRun verbosity pkgs
                return []
           | otherwise -> installPackages verbosity configFlags pkgs

printDryRun :: Verbosity -> DepGraph.DepGraph -> IO ()
printDryRun verbosity pkgs
  | DepGraph.empty pkgs = notice verbosity "No packages to be installed."
  | otherwise = do
        notice verbosity $ "In order, the following would be installed:\n"
          ++ unlines (map showPackageId (order pkgs))
        where
        order ps
            | DepGraph.empty ps = []
            | otherwise =
                let (DepGraph.ResolvedPackage pkgInfo _ _) = DepGraph.ready ps
                    pkgId = packageId pkgInfo
                in (pkgId : order (DepGraph.removeCompleted pkgId ps))

installPackages :: Verbosity
                -> Cabal.ConfigFlags -- ^Options which will be passed to every package.
                -> DepGraph.DepGraph
                -> IO [(PackageIdentifier, BuildResult)]
installPackages verbosity configFlags = installPackagesErrs []
  where
    installPackagesErrs :: [(PackageIdentifier, BuildResult)]
                        -> DepGraph.DepGraph
                        -> IO [(PackageIdentifier, BuildResult)]
    installPackagesErrs done remaining
      | DepGraph.empty remaining = return (reverse done)
      | otherwise = case DepGraph.ready remaining of
      DepGraph.ResolvedPackage pkg flags _depids -> do--TODO build against exactly these deps
        let pkgid = packageId pkg
        buildResult <- installPkg verbosity configFlags pkg flags
        case buildResult of
          BuildOk ->
            let remaining' = DepGraph.removeCompleted pkgid remaining
             in installPackagesErrs ((pkgid, buildResult):done) remaining'
          _ ->
            let (remaining', _:failed) = DepGraph.removeFailed pkgid remaining
                -- So this first pkgid failed for whatever reason (buildResult)
                -- all the other packages that depended on this pkgid which we
                -- now cannot build (failed :: [ResolvedPackage]) we mark as
                -- failing due to DependentFailed which kind of means it was
                -- not their fault.
                done' = (pkgid, buildResult)
                      : [ (packageId pkg', DependentFailed pkgid)
                        | pkg' <- failed ]
             in installPackagesErrs (done'++done) remaining'

{-|
  Download, build and install a given package with some given flags.

  The process is divided up in a few steps:

    * The package is downloaded to {config-dir}\/packages\/{pkg-id} (if not already there).

    * The fetched tarball is then moved to a temporary directory (\/tmp on linux) and unpacked.

    * setupWrapper (equivalent to cabal-setup) is called with the options
      \'configure\' and the user specified options, \'--user\'
      if the 'configUser' flag is @True@ and install directory flags depending on 
      @configUserInstallDirs@ or @configGlobalInstallDirs@.

    * setupWrapper \'build\' is called with no options.

    * setupWrapper \'install\' is called with the \'--user\' flag if 'configUserInstall' is @True@.

    * The installation finishes by deleting the unpacked tarball.
-} 
installPkg :: Verbosity
           -> Cabal.ConfigFlags -- ^Options which will be parse to every package.
           -> PkgInfo
           -> FlagAssignment
           -> IO BuildResult
installPkg verbosity configFlags pkg flags
    = do pkgPath <- fetchPackage verbosity pkg
         tmp <- getTemporaryDirectory
         let p = packageId pkg
             tmpDirPath = tmp </> ("TMP" ++ showPackageId p)
             path = tmpDirPath </> showPackageId p
         bracket_ (createDirectoryIfMissing True tmpDirPath)
                  (removeDirectoryRecursive tmpDirPath)
                  (do info verbosity $ "Extracting " ++ pkgPath ++ " to " ++ tmpDirPath ++ "..."
                      extractTarGzFile (Just tmpDirPath) pkgPath
                      let descFilePath = tmpDirPath </> showPackageId p </> pkgName p <.> "cabal"
                      e <- doesFileExist descFilePath
                      when (not e) $ die $ "Package .cabal file not found: " ++ show descFilePath
                      let configFlags' = configFlags {
                            Cabal.configConfigurationsFlags =
                              Cabal.configConfigurationsFlags configFlags ++ flags }
                      installUnpackedPkg verbosity configFlags' (Just path))
           `catch` \_ -> return UnpackFailed

installUnpackedPkg :: Verbosity
                   -> Cabal.ConfigFlags -- ^ Arguments for this package
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> IO BuildResult
installUnpackedPkg verbosity configFlags mpath
    = onFailure ConfigureFailed $ do
        setup ("configure" : configureOptions)
        onFailure BuildFailed $ do
          setup ["build"]
          onFailure InstallFailed $ do
            setup ["install"]
            return BuildOk
  where
    configureCommand = Cabal.configureCommand defaultProgramConfiguration
    configureOptions = commandShowOptions configureCommand configFlags
    setup cmds
        = do debug verbosity $
               "setupWrapper in " ++ show mpath ++ " :\n " ++ show cmds
             setupWrapper cmds mpath
    onFailure result = Exception.handle (\_ -> return result)
