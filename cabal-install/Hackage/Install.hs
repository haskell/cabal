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

import Data.List
         ( unfoldr )
import Data.Monoid (Monoid(mconcat))
import Control.Exception as Exception
         ( handle, Exception )
import Control.Monad
         ( when, unless )
import System.Directory
         ( getTemporaryDirectory, doesFileExist )
import System.FilePath ((</>),(<.>))

import Hackage.Dependency (resolveDependencies)
import Hackage.Fetch (fetchPackage)
-- import qualified Hackage.Info as Info
import qualified Hackage.IndexUtils as IndexUtils
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.InstallPlan (InstallPlan)
import Hackage.Setup (InstallFlags(..))
import Hackage.Tar (extractTarGzFile)
import Hackage.Types as Available
         ( UnresolvedDependency(..), AvailablePackage(..)
         , AvailablePackageSource(..), Repo, ConfiguredPackage(..) )
import Hackage.Utils (showDependencies)
import Hackage.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..) )
import Paths_cabal_install (getBinDir)

import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDB(..) )
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Configure (getInstalledPackages)
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Setup
         ( flagToMaybe )
import Distribution.Simple.Utils
         ( defaultPackageDesc, inDir, rawSystemExit, withTempDirectory )
import Distribution.Package
         ( PackageIdentifier(..), Package(..), Dependency(..) )
import Distribution.PackageDescription as PackageDescription
         ( GenericPackageDescription(packageDescription) )
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
import Distribution.Verbosity (Verbosity, showForCabal, verbose)
import Distribution.Simple.BuildPaths ( exeExtension )

data BuildResult = DependentFailed PackageIdentifier
                 | UnpackFailed    Exception
                 | ConfigureFailed Exception
                 | BuildFailed     Exception
                 | InstallFailed   Exception
                 | BuildOk

data InstallMisc = InstallMisc {
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
  installed <- getInstalledPackages verbosity comp packageDB conf
  available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)

  maybePlan <- if null deps
    then planLocalPackage verbosity comp configFlags installed available
    else planRepoPackages verbosity comp             installed available deps

  info verbosity "Resolving dependencies..."
  case maybePlan of
    Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
    Right installPlan -> do
      when (dryRun || verbosity >= verbose) $
        printDryRun verbosity installPlan

      unless dryRun $ do
        executeInstallPlan installPlan $ \cpkg ->
          installConfiguredPackage configFlags cpkg $ \configFlags' apkg ->
            installAvailablePackage verbosity apkg $
              installUnpackedPackage verbosity (setupScriptOptions installed)
                                     miscOptions configFlags'
        return ()

  let buildResults :: [(PackageIdentifier, BuildResult)]
      buildResults = [] --FIXME: get build results from executeInstallPlan
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

  where
    buildFailed BuildOk = False
    buildFailed _       = True
    setupScriptOptions index = SetupScriptOptions {
      useCabalVersion  = maybe AnyVersion ThisVersion (libVersion miscOptions),
      useCompiler      = Just comp,
      usePackageIndex  = if packageDB == UserPackageDB then index else Nothing,
      useProgramConfig = conf
    }
    dryRun       = Cabal.fromFlag (installDryRun installFlags)
    miscOptions  = InstallMisc {
      rootCmd    = if Cabal.fromFlag (Cabal.configUserInstall configFlags)
                     then Nothing      -- ignore --root-cmd if --user.
                     else Cabal.flagToMaybe (installRootCmd installFlags),
      libVersion = Cabal.flagToMaybe (installCabalVersion installFlags)
    }

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
--
planLocalPackage :: Verbosity
                 -> Compiler
                 -> Cabal.ConfigFlags
                 -> Maybe (PackageIndex InstalledPackageInfo)
                 -> PackageIndex AvailablePackage
                 -> IO (Either [Dependency] (InstallPlan BuildResult))
planLocalPackage verbosity comp configFlags installed available = do
  pkg <- readPackageDescription verbosity =<< defaultPackageDesc verbosity
  let -- The trick is, we add the local package to the available index and
      -- remove it from the installed index. Then we ask to resolve a
      -- dependency on exactly that package. So the resolver ends up having
      -- to pick the local package.
      available' = PackageIndex.insert localPkg available
      installed' = PackageIndex.delete (packageId localPkg) `fmap` installed
      localPkg = AvailablePackage {
        packageInfoId                = packageId pkg,
        Available.packageDescription = pkg,
        packageSource                = LocalUnpackedPackage
      }
      localPkgDep = UnresolvedDependency {
        dependency = let PackageIdentifier n v = packageId localPkg
                      in Dependency n (ThisVersion v),
        depFlags   = Cabal.configConfigurationsFlags configFlags
      }

  return $ resolveDependencies buildOS buildArch (compilerId comp)
                               installed' available' [localPkgDep]

-- | Make an 'InstallPlan' for the given dependencies.
--
planRepoPackages :: Verbosity
                 -> Compiler
                 -> Maybe (PackageIndex InstalledPackageInfo)
                 -> PackageIndex AvailablePackage
                 -> [UnresolvedDependency]
                 -> IO (Either [Dependency] (InstallPlan BuildResult))
planRepoPackages _verbosity comp installed available deps = do
  deps' <- IndexUtils.disambiguateDependencies available deps
  return $ resolveDependencies buildOS buildArch (compilerId comp)
                               installed available deps'

printDryRun :: Verbosity -> InstallPlan BuildResult -> IO ()
printDryRun verbosity plan = case unfoldr next plan of
  []   -> notice verbosity "No packages to be installed."
  pkgs -> notice verbosity $ unlines $
            "In order, the following would be installed:"
          : map display pkgs
  where
    next plan' = case InstallPlan.ready plan' of
      []      -> Nothing
      (pkg:_) -> Just (pkgid, InstallPlan.completed pkgid plan')
        where pkgid = packageId pkg

executeInstallPlan :: Monad m
                   => InstallPlan BuildResult
                   -> (ConfiguredPackage -> m BuildResult)
                   -> m (InstallPlan BuildResult)
executeInstallPlan plan installPkg = case InstallPlan.ready plan of
  [] -> return plan
  (pkg: _) -> do
    buildResult <- installPkg pkg
    let pkgid = packageId pkg
        updatePlan = case buildResult of
          BuildOk -> InstallPlan.completed pkgid
          _       -> InstallPlan.failed    pkgid buildResult depsResult
            where depsResult = DependentFailed pkgid
            -- So this first pkgid failed for whatever reason (buildResult)
            -- all the other packages that depended on this pkgid which we
            -- now cannot build we mark as failing due to DependentFailed
            -- which kind of means it was not their fault.
    executeInstallPlan (updatePlan plan) installPkg

-- | Call an installer for an 'AvailablePackage' but override the configure
-- flags with the ones given by the 'ConfiguredPackage'. In particular the 
-- 'ConfiguredPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
installConfiguredPackage ::  Cabal.ConfigFlags -> ConfiguredPackage
                         -> (Cabal.ConfigFlags -> AvailablePackage  -> a)
                         -> a
installConfiguredPackage configFlags (ConfiguredPackage pkg flags deps)
  installPkg = installPkg configFlags {
    Cabal.configConfigurationsFlags = flags,
    Cabal.configConstraints = [ Dependency name (ThisVersion version)
                              | PackageIdentifier name version  <- deps ]
  } pkg

installAvailablePackage
  :: Verbosity -> AvailablePackage
  -> (GenericPackageDescription -> Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installAvailablePackage _ (AvailablePackage _ pkg LocalUnpackedPackage)
  installPkg = installPkg pkg Nothing

installAvailablePackage verbosity apkg@(AvailablePackage _ pkg _)
  installPkg = do
  pkgPath <- fetchPackage verbosity apkg
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
    installPkg pkg (Just path)

installUnpackedPackage :: Verbosity
                   -> SetupScriptOptions
                   -> InstallMisc
                   -> Cabal.ConfigFlags
                   -> GenericPackageDescription
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> IO BuildResult
installUnpackedPackage verbosity scriptOptions miscOptions configFlags pkg mpath
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
                           (Just $ PackageDescription.packageDescription pkg)
                           cmd flags []
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
