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
import Control.Exception as Exception
         ( handle, Exception )
import Control.Monad (when)
import System.Directory
         ( getTemporaryDirectory, doesFileExist )
import System.FilePath ((</>),(<.>))

import Hackage.Dependency (resolveDependencies, resolveDependenciesLocal)
import Hackage.Fetch (fetchPackage)
-- import qualified Hackage.Info as Info
import qualified Hackage.IndexUtils as IndexUtils
import qualified Hackage.DepGraph as DepGraph
import Hackage.Setup (InstallFlags(..))
import Hackage.Tar (extractTarGzFile)
import Hackage.Types (UnresolvedDependency(..), PkgInfo(..), FlagAssignment,
                      Repo)
import Hackage.Utils (showDependencies)
import Paths_cabal_install (getBinDir)

import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDB(..) )
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Command (commandShowOptions)
import Distribution.Simple.SetupWrapper (setupWrapper)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils
         ( defaultPackageDesc, inDir, rawSystemExit, withTempDirectory )
import Distribution.Package
         ( PackageIdentifier(..), Package(..) )
import Distribution.PackageDescription (GenericPackageDescription(packageDescription))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils as Utils (notice, info, debug, die)
import Distribution.System
         ( buildOS, buildArch )
import Distribution.Text
         ( display )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.BuildPaths ( exeExtension )

data BuildResult = DependentFailed PackageIdentifier
                 | UnpackFailed    Exception
                 | ConfigureFailed Exception
                 | BuildFailed     Exception
                 | InstallFailed   Exception
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
      -- ignore --root-cmd if --user.
      rootCmd | Cabal.fromFlag (Cabal.configUserInstall configFlags) = Nothing
              | otherwise = Cabal.flagToMaybe (installRootCmd installFlags)
  buildResults <- if null deps
    then installLocalPackage verbosity
           packageDB repos comp conf configFlags dryRun rootCmd
    else installRepoPackages verbosity
           packageDB repos comp conf configFlags dryRun rootCmd deps
  case filter (buildFailed . snd) buildResults of
    []     -> return () --TODO: return the build results
    failed -> die $ "Error: some packages failed to install:\n"
      ++ unlines
         [ display pkgid ++ case reason of
           DependentFailed pkgid' -> " depends on " ++ display pkgid'
                                  ++ " which failed to install."
           UnpackFailed    e -> " failed while unpacking the package."
                             ++ " The exception was:\n  " ++ show e
           ConfigureFailed e -> " failed during the configure step."
                             ++ " The exception was:\n  " ++ show e
           BuildFailed     e -> " failed during the building phase."
                             ++ " The exception was:\n  " ++ show e
           InstallFailed   e -> " failed during the final install step."
                             ++ " The exception was:\n  " ++ show e
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
                    -> Maybe FilePath -- ^ RootCmd
                    -> IO [(PackageIdentifier, BuildResult)]
installLocalPackage verbosity packageDB repos comp conf configFlags dryRun rootCmd =
   do cabalFile <- defaultPackageDesc verbosity
      desc <- readPackageDescription verbosity cabalFile
      installed <- getInstalledPackages verbosity comp packageDB conf
      available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
      --TODO: print the info again
      -- details <- mapM Info.infoPkg (Info.flattenResolvedDependencies resolvedDeps)
      -- info verbosity $ unlines (map ("  "++) (concat details))
      buildResults <- case resolveDependenciesLocal buildOS buildArch
                             (compilerId comp) installed available desc
                             (Cabal.configConfigurationsFlags configFlags) of
        Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
        Right pkgs   -> do
            if dryRun
              then printDryRun verbosity pkgs >> return []
              else installPackages verbosity configFlags rootCmd pkgs
      if dryRun
        then return []
        --TODO: don't run if buildResult failed
        else do buildResult <- installUnpackedPkg verbosity configFlags Nothing rootCmd
                return ((packageId (packageDescription desc), buildResult) : buildResults)

installRepoPackages :: Verbosity
                    -> PackageDB
                    -> [Repo]
                    -> Compiler
                    -> ProgramConfiguration
                    -> Cabal.ConfigFlags
                    -> Bool -- ^Dry run
                    -> Maybe FilePath -- ^RootCmd
                    -> [UnresolvedDependency]
                    -> IO [(PackageIdentifier, BuildResult)]
installRepoPackages verbosity packageDB repos comp conf configFlags dryRun rootCmd deps =
    do installed <- getInstalledPackages verbosity comp packageDB conf
       available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
       deps' <- IndexUtils.disambiguateDependencies available deps
       -- details <- mapM Info.infoPkg (Info.flattenResolvedDependencies resolvedDeps)
       -- info verbosity $ unlines (map ("  "++) (concat details))
       case resolveDependencies buildOS buildArch (compilerId comp)
              installed available deps' of
         Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
         Right pkgs
           | DepGraph.empty pkgs -> notice verbosity
                     "All requested packages already installed. Nothing to do."
                     >> return []
           | dryRun -> do
                printDryRun verbosity pkgs
                return []
           | otherwise -> installPackages verbosity configFlags rootCmd pkgs

printDryRun :: Verbosity -> DepGraph.DepGraph -> IO ()
printDryRun verbosity pkgs
  | DepGraph.empty pkgs = notice verbosity "No packages to be installed."
  | otherwise = do
        notice verbosity $ "In order, the following would be installed:\n"
          ++ unlines (map display (order pkgs))
        where
        order ps
            | DepGraph.empty ps = []
            | otherwise =
                let (DepGraph.ResolvedPackage pkgInfo _ _) = DepGraph.ready ps
                    pkgId = packageId pkgInfo
                in (pkgId : order (DepGraph.removeCompleted pkgId ps))

installPackages :: Verbosity
                -> Cabal.ConfigFlags -- ^Options which will be passed to every package.
                -> Maybe FilePath    -- ^RootCmd
                -> DepGraph.DepGraph
                -> IO [(PackageIdentifier, BuildResult)]
installPackages verbosity configFlags rootCmd = installPackagesErrs []
  where
    installPackagesErrs :: [(PackageIdentifier, BuildResult)]
                        -> DepGraph.DepGraph
                        -> IO [(PackageIdentifier, BuildResult)]
    installPackagesErrs done remaining
      | DepGraph.empty remaining = return (reverse done)
      | otherwise = case DepGraph.ready remaining of
      DepGraph.ResolvedPackage pkg flags _depids -> do--TODO build against exactly these deps
        let pkgid = packageId pkg
        buildResult <- installPkg verbosity configFlags rootCmd pkg flags
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
           -> Maybe FilePath    -- ^RootCmd
           -> PkgInfo
           -> FlagAssignment
           -> IO BuildResult
installPkg verbosity configFlags rootCmd pkg flags = do
  pkgPath <- fetchPackage verbosity pkg
  tmp <- getTemporaryDirectory
  let pkgid = packageId pkg
      tmpDirPath = tmp </> ("TMP" ++ display pkgid)
      path = tmpDirPath </> display pkgid
  onFailure UnpackFailed $ withTempDirectory verbosity tmpDirPath $ do
    info verbosity $ "Extracting " ++ pkgPath ++ " to " ++ tmpDirPath ++ "..."
    extractTarGzFile tmpDirPath pkgPath
    let descFilePath = tmpDirPath </> display pkgid
                                  </> pkgName pkgid <.> "cabal"
    exists <- doesFileExist descFilePath
    when (not exists) $
      die $ "Package .cabal file not found: " ++ show descFilePath
    let configFlags' = configFlags {
          Cabal.configConfigurationsFlags =
          Cabal.configConfigurationsFlags configFlags ++ flags
        }
    installUnpackedPkg verbosity configFlags' (Just path) rootCmd

installUnpackedPkg :: Verbosity
                   -> Cabal.ConfigFlags -- ^ Arguments for this package
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> Maybe FilePath -- ^ Use this command to gain privileges while running install.
                   -> IO BuildResult
installUnpackedPkg verbosity configFlags mpath rootCmd 
    = onFailure ConfigureFailed $ do
        setup ("configure" : configureOptions)
        onFailure BuildFailed $ do
          setup ["build"]
          onFailure InstallFailed $ do
            case rootCmd of 
              (Just cmd) -> reexec cmd
              Nothing    -> setup ["install"]
            return BuildOk
  where
    configureCommand = Cabal.configureCommand defaultProgramConfiguration
    configureOptions = commandShowOptions configureCommand configFlags
    setup cmds
        = do debug verbosity $
               "setupWrapper in " ++ show mpath ++ " :\n " ++ show cmds
             setupWrapper cmds mpath
    reexec cmd = 
        do bindir <- getBinDir
           let self = bindir </> "cabal" <.> exeExtension
           b <- doesFileExist self
           if b then
               inDir mpath $ 
               rawSystemExit verbosity cmd [self,"install","--only"]
             else 
               die $ "Unable to find cabal executable at: " ++ self 
               
-- helper
onFailure :: (Exception -> BuildResult) -> IO BuildResult -> IO BuildResult
onFailure result = Exception.handle (return . result)
