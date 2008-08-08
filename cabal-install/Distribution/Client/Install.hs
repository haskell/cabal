-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Install
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Distribution.Client.Install (
    install,
    upgrade,
  ) where

import Data.List
         ( unfoldr )
import Data.Maybe
         ( isJust )
import Control.Exception as Exception
         ( handle, Exception )
import Control.Monad
         ( when, unless, forM_ )
import System.Directory
         ( getTemporaryDirectory, doesFileExist, createDirectoryIfMissing )
import System.FilePath
         ( (</>), (<.>), takeDirectory )
import System.IO
         ( openFile, IOMode(AppendMode) )

import Distribution.Client.Dependency
         ( resolveDependenciesWithProgress, PackagesVersionPreference(..)
         , upgradableDependencies )
import Distribution.Client.Dependency.Types (Progress(..), foldProgress)
import Distribution.Client.Fetch (fetchPackage)
-- import qualified Distribution.Client.Info as Info
import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages, disambiguateDependencies )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.Setup
         ( InstallFlags(..), configureCommand, filterConfigureFlags )
import Distribution.Client.Config
         ( defaultLogsDir, defaultCabalDir )
import Distribution.Client.Tar (extractTarGzFile)
import Distribution.Client.Types as Available
         ( UnresolvedDependency(..), AvailablePackage(..)
         , AvailablePackageSource(..), Repo(..), ConfiguredPackage(..)
         , BuildResult, BuildFailure(..), BuildSuccess(..)
         , DocsResult(..), TestsResult(..), RemoteRepo(..) )
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import qualified Distribution.Client.BuildReports.Anonymous as BuildReports
import qualified Distribution.Client.BuildReports.Storage as BuildReports
         ( storeAnonymous, storeLocal, fromInstallPlan )
import qualified Distribution.Client.InstallSymlink as InstallSymlink
         ( symlinkBinaries )
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
import Distribution.Simple.InstallDirs
         ( fromPathTemplate, toPathTemplate
         , initialPathTemplateEnv, substPathTemplate )
import Distribution.Package
         ( PackageIdentifier(..), Package(..), thisPackageVersion )
import Distribution.PackageDescription as PackageDescription
         ( GenericPackageDescription(packageDescription)
         , readPackageDescription )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Version
         ( Version, VersionRange(AnyVersion, ThisVersion) )
import Distribution.Simple.Utils as Utils
         ( notice, info, warn, die, intercalate )
import Distribution.System
         ( buildOS, buildArch )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, showForCabal, verbose )
import Distribution.Simple.BuildPaths ( exeExtension )

data InstallMisc = InstallMisc {
    rootCmd    :: Maybe FilePath,
    libVersion :: Maybe Version
  }

-- |Installs the packages needed to satisfy a list of dependencies.
install, upgrade
  :: Verbosity
  -> PackageDB
  -> [Repo]
  -> Compiler
  -> ProgramConfiguration
  -> Cabal.ConfigFlags
  -> InstallFlags
  -> [UnresolvedDependency]
  -> IO ()
install verbosity packageDB repos comp conf configFlags installFlags deps =
  installWithPlanner planner
        verbosity packageDB repos comp conf configFlags installFlags
  where
    planner :: Planner
    planner | null deps = planLocalPackage verbosity comp configFlags
            | otherwise = planRepoPackages PreferLatestForSelected comp deps

upgrade verbosity packageDB repos comp conf configFlags installFlags deps =
  installWithPlanner planner
        verbosity packageDB repos comp conf configFlags installFlags
  where
    planner :: Planner
    planner | null deps = planUpgradePackages comp
            | otherwise = planRepoPackages PreferAllLatest comp deps

type Planner = Maybe (PackageIndex InstalledPackageInfo)
            -> PackageIndex AvailablePackage
            -> IO (Progress String String InstallPlan)

-- |Installs the packages generated by a planner.
installWithPlanner ::
           Planner
        -> Verbosity
        -> PackageDB
        -> [Repo]
        -> Compiler
        -> ProgramConfiguration
        -> Cabal.ConfigFlags
        -> InstallFlags
        -> IO ()
installWithPlanner planner verbosity packageDB repos comp conf configFlags installFlags = do
  installed <- getInstalledPackages verbosity comp packageDB conf
  available <- getAvailablePackages verbosity repos

  progress <- planner installed available

  notice verbosity "Resolving dependencies..."
  maybePlan <- foldProgress (\message rest -> info verbosity message >> rest)
                            (return . Left) (return . Right) progress
  case maybePlan of
    Left message -> die message
    Right installPlan -> do
      when (dryRun || verbosity >= verbose) $
        printDryRun verbosity installPlan

      unless dryRun $ do
        logsDir <- defaultLogsDir
        installPlan' <-
          executeInstallPlan installPlan $ \cpkg ->
            installConfiguredPackage configFlags cpkg $ \configFlags' apkg ->
              installAvailablePackage verbosity apkg $ \pkg mpath ->
                installUnpackedPackage verbosity (setupScriptOptions installed)
                                       miscOptions configFlags' installFlags
                                       pkg mpath (useLogFile logsDir)

        let buildReports = BuildReports.fromInstallPlan installPlan'
        BuildReports.storeAnonymous buildReports
        BuildReports.storeLocal     buildReports
        storeDetailedBuildReports logsDir buildReports
        symlinkBinaries verbosity configFlags installFlags installPlan'
        printBuildFailures installPlan'

  where
    setupScriptOptions index = SetupScriptOptions {
      useCabalVersion  = maybe AnyVersion ThisVersion (libVersion miscOptions),
      useCompiler      = Just comp,
      usePackageIndex  = if packageDB == UserPackageDB then index else Nothing,
      useProgramConfig = conf,
      useDistPref      = Cabal.fromFlagOrDefault
                           (useDistPref defaultSetupScriptOptions)
                           (Cabal.configDistPref configFlags),
      useLoggingHandle = Nothing,
      useWorkingDir    = Nothing
    }
    useLogFile :: FilePath -> Maybe (PackageIdentifier -> FilePath)
    useLogFile logsDir = fmap substLogFileName logFileTemplate
      where
        logFileTemplate
          | Cabal.fromFlagOrDefault False (installBuildReports installFlags)
	  = Just $ logsDir </> "$pkgid" <.> "log"
          | otherwise = Cabal.flagToMaybe (installLogFile installFlags)
    substLogFileName path pkg = fromPathTemplate
                              . substPathTemplate env
                              . toPathTemplate
                              $ path
      where env = initialPathTemplateEnv (packageId pkg) (compilerId comp)
    dryRun       = Cabal.fromFlag (installDryRun installFlags)
    miscOptions  = InstallMisc {
      rootCmd    = if Cabal.fromFlag (Cabal.configUserInstall configFlags)
                     then Nothing      -- ignore --root-cmd if --user.
                     else Cabal.flagToMaybe (installRootCmd installFlags),
      libVersion = Cabal.flagToMaybe (installCabalVersion installFlags)
    }

storeDetailedBuildReports :: FilePath -> [(BuildReports.BuildReport, Repo)] -> IO ()
storeDetailedBuildReports logsDir reports
    = forM_ reports $ \(report,repo) ->
      do buildLog <- readFile (logsDir </> display (BuildReports.package report) <.> "log")
         case repoKind repo of
           Left remoteRepo
                -> do dotCabal <- defaultCabalDir
                      let destDir = dotCabal </> "reports" </> remoteRepoName remoteRepo
                          dest = destDir </> display (BuildReports.package report) <.> "log"
                      createDirectoryIfMissing True destDir -- FIXME
                      writeFile dest (show (BuildReports.show report, buildLog))
           Right{} -> return ()
         

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
--
planLocalPackage :: Verbosity -> Compiler -> Cabal.ConfigFlags -> Planner
planLocalPackage verbosity comp configFlags installed available = do
  pkg <- readPackageDescription verbosity =<< defaultPackageDesc verbosity
  let -- The trick is, we add the local package to the available index and
      -- remove it from the installed index. Then we ask to resolve a
      -- dependency on exactly that package. So the resolver ends up having
      -- to pick the local package.
      available' = PackageIndex.insert localPkg available
      installed' = PackageIndex.deletePackageId (packageId localPkg) `fmap` installed
      localPkg = AvailablePackage {
        packageInfoId                = packageId pkg,
        Available.packageDescription = pkg,
        packageSource                = LocalUnpackedPackage
      }
      localPkgDep = UnresolvedDependency {
        dependency = thisPackageVersion (packageId localPkg),
        depFlags   = Cabal.configConfigurationsFlags configFlags
      }

  return $ resolveDependenciesWithProgress buildOS buildArch (compilerId comp)
             installed' available' PreferLatestForSelected [localPkgDep]

-- | Make an 'InstallPlan' for the given dependencies.
--
planRepoPackages :: PackagesVersionPreference -> Compiler
                 -> [UnresolvedDependency] -> Planner
planRepoPackages pref comp deps installed available = do
  deps' <- IndexUtils.disambiguateDependencies available deps
  return $ resolveDependenciesWithProgress buildOS buildArch (compilerId comp)
             installed available pref deps'

planUpgradePackages :: Compiler -> Planner
planUpgradePackages comp (Just installed) available = return $
  resolveDependenciesWithProgress buildOS buildArch (compilerId comp)
    (Just installed) available PreferAllLatest
    [ UnresolvedDependency dep []
    | dep <- upgradableDependencies installed available ]
planUpgradePackages comp _ _ =
  die $ display (compilerId comp)
     ++ " does not track installed packages so cabal cannot figure out what"
     ++ " packages need to be upgraded."

printDryRun :: Verbosity -> InstallPlan -> IO ()
printDryRun verbosity plan = case unfoldr next plan of
  []   -> notice verbosity "No packages to be installed."
  pkgs -> notice verbosity $ unlines $
            "In order, the following would be installed:"
          : map display pkgs
  where
    next plan' = case InstallPlan.ready plan' of
      []      -> Nothing
      (pkg:_) -> Just (pkgid, InstallPlan.completed pkgid result plan')
        where pkgid = packageId pkg
              result = BuildOk DocsNotTried TestsNotTried
              --FIXME: This is a bit of a hack,
              -- pretending that each package is installed

symlinkBinaries :: Verbosity
                -> Cabal.ConfigFlags
                -> InstallFlags
                -> InstallPlan -> IO ()
symlinkBinaries verbosity configFlags installFlags plan = do
  failed <- InstallSymlink.symlinkBinaries configFlags installFlags plan
  case failed of
    [] -> return ()
    [(_, exe, path)] ->
      warn verbosity $
           "could not create a symlink in " ++ bindir ++ " for "
        ++ exe ++ " because the file exists there already but is not "
        ++ "managed by cabal. You can create a symlink for this executable "
        ++ "manually if you wish. The executable file has been installed at "
        ++ path
    exes ->
      warn verbosity $
           "could not create symlinks in " ++ bindir ++ " for "
        ++ intercalate ", " [ exe | (_, exe, _) <- exes ]
        ++ " because the files exist there already and are not "
        ++ "managed by cabal. You can create symlinks for these executables "
        ++ "manually if you wish. The executable files have been installed at "
        ++ intercalate ", " [ path | (_, _, path) <- exes ]
  where
    bindir = Cabal.fromFlag (installSymlinkBinDir installFlags)

printBuildFailures :: InstallPlan -> IO ()
printBuildFailures plan =
  case [ (pkg, reason)
       | InstallPlan.Failed pkg reason <- InstallPlan.toList plan ] of
    []     -> return ()
    failed -> die . unlines
            $ "Error: some packages failed to install:"
            : [ display (packageId pkg) ++ printFailureReason reason
              | (pkg, reason) <- failed ]
  where
    printFailureReason reason = case reason of
      DependentFailed pkgid -> " depends on " ++ display pkgid
                            ++ " which failed to install."
      UnpackFailed    e -> " failed while unpacking the package."
                        ++ " The exception was:\n  " ++ show e
      ConfigureFailed e -> " failed during the configure step."
                        ++ " The exception was:\n  " ++ show e
      BuildFailed     e -> " failed during the building phase."
                        ++ " The exception was:\n  " ++ show e
      InstallFailed   e -> " failed during the final install step."
                        ++ " The exception was:\n  " ++ show e

executeInstallPlan :: Monad m
                   => InstallPlan
                   -> (ConfiguredPackage -> m BuildResult)
                   -> m InstallPlan
executeInstallPlan plan installPkg = case InstallPlan.ready plan of
  []       -> return plan
  (pkg: _) -> do buildResult <- installPkg pkg
                 let plan' = updatePlan (packageId pkg) buildResult plan
                 executeInstallPlan plan' installPkg
  where
    updatePlan pkgid (Right buildSuccess) =
      InstallPlan.completed pkgid buildSuccess

    updatePlan pkgid (Left buildFailure) =
      InstallPlan.failed    pkgid buildFailure depsFailure
      where
        depsFailure = DependentFailed pkgid
        -- So this first pkgid failed for whatever reason (buildFailure).
        -- All the other packages that depended on this pkgid, which we
        -- now cannot build, we mark as failing due to 'DependentFailed'
        -- which kind of means it was not their fault.

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
    Cabal.configConstraints = map thisPackageVersion deps
  } pkg

installAvailablePackage
  :: Verbosity -> AvailablePackage
  -> (GenericPackageDescription -> Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installAvailablePackage _ (AvailablePackage _ pkg LocalUnpackedPackage)
  installPkg = installPkg pkg Nothing

installAvailablePackage verbosity
  (AvailablePackage _ pkg (RepoTarballPackage repo)) installPkg = do
  let pkgid = packageId pkg
  pkgPath <- fetchPackage verbosity repo pkgid
  tmp <- getTemporaryDirectory
  let tmpDirPath = tmp </> ("TMP" ++ display pkgid)
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
                   -> InstallFlags
                   -> GenericPackageDescription
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> Maybe (PackageIdentifier -> FilePath) -- ^ File to log output to (if any)
                   -> IO BuildResult
installUnpackedPackage verbosity scriptOptions miscOptions
                       configFlags installConfigFlags
                       pkg workingDir useLogFile =

  -- Configure phase
  onFailure ConfigureFailed $ do
    setup configureCommand configureFlags

  -- Build phase
    onFailure BuildFailed $ do
      setup buildCommand buildFlags

  -- Doc generation phase
      docsResult <- if shouldHaddock
        then Exception.handle (\_ -> return DocsFailed) $ do
               setup Cabal.haddockCommand haddockFlags
               return DocsOk
        else return DocsNotTried

  -- Tests phase
      testsResult <- return TestsNotTried  --TODO: add optional tests

  -- Install phase
      onFailure InstallFailed $ do
        case rootCmd miscOptions of
          (Just cmd) -> reexec cmd
          Nothing    -> setup Cabal.installCommand installFlags
        return (Right (BuildOk docsResult testsResult))

  where
    configureFlags   = filterConfigureFlags configFlags {
      Cabal.configVerbosity = Cabal.toFlag verbosity'
    } 
    buildCommand     = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = Cabal.emptyBuildFlags {
      Cabal.buildDistPref  = Cabal.configDistPref configFlags,
      Cabal.buildVerbosity = Cabal.toFlag verbosity'
    }
    shouldHaddock    = Cabal.fromFlagOrDefault False
                         (installDocumentation installConfigFlags)
    haddockFlags _   = Cabal.emptyHaddockFlags {
      Cabal.haddockDistPref  = Cabal.configDistPref configFlags,
      Cabal.haddockVerbosity = Cabal.toFlag verbosity'
    }
    installFlags _   = Cabal.emptyInstallFlags {
      Cabal.installDistPref  = Cabal.configDistPref configFlags,
      Cabal.installVerbosity = Cabal.toFlag verbosity'
    }
    verbosity' | isJust useLogFile = max Verbosity.verbose verbosity
               | otherwise         = verbosity
    setup cmd flags  = do
      logFileHandle <- case useLogFile of
        Nothing          -> return Nothing
        Just mkLogFileName -> do
	  let logFileName = mkLogFileName (packageId pkg)
	      logDir      = takeDirectory logFileName
	  unless (null logDir) $ createDirectoryIfMissing True logDir
	  logFile <- openFile logFileName AppendMode
	  return (Just logFile)

      setupWrapper verbosity
        scriptOptions { useLoggingHandle = logFileHandle
                      , useWorkingDir    = workingDir }
        (Just $ PackageDescription.packageDescription pkg)
        cmd flags []
    reexec cmd = do
      -- look for our on executable file and re-exec ourselves using
      -- a helper program like sudo to elevate priviledges:
      bindir <- getBinDir
      let self = bindir </> "cabal" <.> exeExtension
      weExist <- doesFileExist self
      if weExist
        then inDir workingDir $
               rawSystemExit verbosity cmd
                 [self, "install", "--only"
                 ,"--verbose=" ++ showForCabal verbosity]
        else die $ "Unable to find cabal executable at: " ++ self

-- helper
onFailure :: (Exception -> BuildFailure) -> IO BuildResult -> IO BuildResult
onFailure result = Exception.handle (return . Left . result)
