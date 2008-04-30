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
import Hackage.Types
         ( UnresolvedDependency(..), PkgInfo(..), Repo )
import Hackage.Utils (showDependencies)
import Hackage.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..) )
import Paths_cabal_install (getBinDir)

import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDB(..) )
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Configure (getInstalledPackages)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Setup
         ( flagToMaybe )
import Distribution.Simple.Utils
         ( defaultPackageDesc, inDir, rawSystemExit, withTempDirectory )
import Distribution.Package
         ( PackageIdentifier(..), Package(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(packageDescription), FlagAssignment )
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Version
         ( Version, VersionRange(AnyVersion, ThisVersion) )
import Distribution.Simple.Utils as Utils (notice, info, die)
import Distribution.System
         ( buildOS, buildArch )
import Distribution.Text
         ( display )
import Distribution.Verbosity (Verbosity, showForCabal)
import Distribution.Simple.BuildPaths ( exeExtension )

data BuildResult = DependentFailed PackageIdentifier
                 | UnpackFailed    Exception
                 | ConfigureFailed Exception
                 | BuildFailed     Exception
                 | InstallFailed   Exception
                 | BuildOk

data InstallMisc = InstallMisc {
    dryRun     :: Bool,
    rootCmd    :: Maybe FilePath,
    libVersion :: Maybe Version
  }

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
  buildResults <- if null deps
    then installLocalPackage verbosity
           packageDB repos comp conf miscOptions configFlags
    else installRepoPackages verbosity
           packageDB repos comp conf miscOptions configFlags deps
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
        miscOptions = InstallMisc {
          dryRun     = Cabal.fromFlag (installDryRun installFlags),
          rootCmd    = if Cabal.fromFlag (Cabal.configUserInstall configFlags)
                         then Nothing      -- ignore --root-cmd if --user.
                         else Cabal.flagToMaybe (installRootCmd installFlags),
          libVersion = Cabal.flagToMaybe (installCabalVersion installFlags)
        }

-- | Install the unpacked package in the current directory, and all its dependencies.
installLocalPackage :: Verbosity
                    -> PackageDB
                    -> [Repo]
                    -> Compiler
                    -> ProgramConfiguration
                    -> InstallMisc
                    -> Cabal.ConfigFlags
                    -> IO [(PackageIdentifier, BuildResult)]
installLocalPackage verbosity packageDB repos comp conf miscOptions configFlags =
   do cabalFile <- defaultPackageDesc verbosity
      desc <- readPackageDescription verbosity cabalFile
      installed <- getInstalledPackages verbosity comp packageDB conf
      available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
      let scriptOptions = mkSetupScriptOptions packageDB comp conf miscOptions installed
      --TODO: print the info again
      -- details <- mapM Info.infoPkg (Info.flattenResolvedDependencies resolvedDeps)
      -- info verbosity $ unlines (map ("  "++) (concat details))
      buildResults <- case resolveDependenciesLocal buildOS buildArch
                             (compilerId comp) installed available desc
                             (Cabal.configConfigurationsFlags configFlags) of
        Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
        Right pkgs   -> do
            if dryRun miscOptions
              then printDryRun verbosity pkgs >> return []
              else installPackages verbosity scriptOptions miscOptions configFlags pkgs
      if dryRun miscOptions
        then return []
        --TODO: don't run if buildResult failed
        else do buildResult <- installUnpackedPkg verbosity scriptOptions miscOptions desc configFlags Nothing
                return ((packageId desc, buildResult) : buildResults)

mkSetupScriptOptions :: PackageDB
                     -> Compiler
                     -> ProgramConfiguration
                     -> InstallMisc
                     -> Maybe (PackageIndex InstalledPackageInfo)
                     -> SetupScriptOptions
mkSetupScriptOptions packageDB comp conf miscOptions index =
  SetupScriptOptions {
    useCabalVersion  = maybe AnyVersion ThisVersion (libVersion miscOptions),
    useCompiler      = Just comp,
    usePackageIndex  = if packageDB == UserPackageDB then index else Nothing,
    useProgramConfig = conf
  }

installRepoPackages :: Verbosity
                    -> PackageDB
                    -> [Repo]
                    -> Compiler
                    -> ProgramConfiguration
                    -> InstallMisc
                    -> Cabal.ConfigFlags
                    -> [UnresolvedDependency]
                    -> IO [(PackageIdentifier, BuildResult)]
installRepoPackages verbosity packageDB repos comp conf miscOptions configFlags deps =
    do installed <- getInstalledPackages verbosity comp packageDB conf
       available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
       let scriptOptions = mkSetupScriptOptions packageDB comp conf miscOptions installed
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
           | dryRun miscOptions -> do
                printDryRun verbosity pkgs
                return []
           | otherwise -> installPackages verbosity scriptOptions miscOptions configFlags pkgs

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
                -> SetupScriptOptions
                -> InstallMisc
                -> Cabal.ConfigFlags -- ^Options which will be passed to every package.
                -> DepGraph.DepGraph
                -> IO [(PackageIdentifier, BuildResult)]
installPackages verbosity scriptOptions miscOptions configFlags = installPackagesErrs []
  where
    installPackagesErrs :: [(PackageIdentifier, BuildResult)]
                        -> DepGraph.DepGraph
                        -> IO [(PackageIdentifier, BuildResult)]
    installPackagesErrs done remaining
      | DepGraph.empty remaining = return (reverse done)
      | otherwise = case DepGraph.ready remaining of
      DepGraph.ResolvedPackage pkg flags _depids -> do--TODO build against exactly these deps
        let pkgid = packageId pkg
        buildResult <- installPkg verbosity scriptOptions miscOptions configFlags pkg flags
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
           -> SetupScriptOptions
           -> InstallMisc
           -> Cabal.ConfigFlags -- ^Options which will be parse to every package.
           -> PkgInfo
           -> FlagAssignment
           -> IO BuildResult
installPkg verbosity scriptOptions miscOptions configFlags pkg flags = do
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
    installUnpackedPkg verbosity scriptOptions miscOptions (pkgDesc pkg) configFlags' (Just path)

installUnpackedPkg :: Verbosity
                   -> SetupScriptOptions
                   -> InstallMisc
                   -> GenericPackageDescription
                   -> Cabal.ConfigFlags -- ^ Arguments for this package
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> IO BuildResult
installUnpackedPkg verbosity scriptOptions miscOptions pkg configFlags mpath
    = onFailure ConfigureFailed $ do
        setup configureCommand configFlags
        onFailure BuildFailed $ do
          setup buildCommand Cabal.emptyBuildFlags
          onFailure InstallFailed $ do
            case rootCmd miscOptions of
              (Just cmd) -> reexec cmd
              Nothing    -> setup Cabal.installCommand Cabal.emptyInstallFlags
            return BuildOk
  where
    configureCommand = Cabal.configureCommand defaultProgramConfiguration
    buildCommand     = Cabal.buildCommand     defaultProgramConfiguration
    setup cmd flags  = inDir mpath $
                         setupWrapper verbosity scriptOptions
                           (Just $ packageDescription pkg) cmd flags []
    reexec cmd = do
      -- look for our on executable file and re-exec ourselves using
      -- a helper program like sudo to elevate priviledges:
      bindir <- getBinDir
      let self = bindir </> "cabal" <.> exeExtension
      weExist <- doesFileExist self
      if weExist
        then inDir mpath $
               rawSystemExit verbosity cmd
                 [self, "install", "--only"
                 ,"--verbose=", showForCabal verbosity]
        else die $ "Unable to find cabal executable at: " ++ self
               
-- helper
onFailure :: (Exception -> BuildResult) -> IO BuildResult -> IO BuildResult
onFailure result = Exception.handle (return . result)
