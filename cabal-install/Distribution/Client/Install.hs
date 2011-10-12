{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Install
-- Copyright   :  (c) 2005 David Himmelstrup
--                    2007 Bjorn Bringert
--                    2007-2010 Duncan Coutts
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
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
         ( unfoldr, find, nub, sort )
import Data.Maybe
         ( isJust, fromMaybe )
import Control.Exception as Exception
         ( handleJust )
#if MIN_VERSION_base(4,0,0)
import Control.Exception as Exception
         ( Exception(toException), catches, Handler(Handler), IOException )
import System.Exit
         ( ExitCode )
#else
import Control.Exception as Exception
         ( Exception(IOException, ExitException) )
#endif
import Distribution.Compat.Exception
         ( SomeException, catchIO, catchExit )
import Control.Monad
         ( when, unless )
import System.Directory
         ( getTemporaryDirectory, doesFileExist, createDirectoryIfMissing )
import System.FilePath
         ( (</>), (<.>), takeDirectory )
import System.IO
         ( openFile, IOMode(AppendMode) )
import System.IO.Error
         ( isDoesNotExistError, ioeGetFileName )

import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.FetchUtils
import qualified Distribution.Client.Haddock as Haddock (regenerateHaddockIndex)
-- import qualified Distribution.Client.Info as Info
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.Setup
         ( GlobalFlags(..)
         , ConfigFlags(..), configureCommand, filterConfigureFlags
         , ConfigExFlags(..), InstallFlags(..) )
import Distribution.Client.Config
         ( defaultCabalDir )
import Distribution.Client.Tar (extractTarGzFile)
import Distribution.Client.Types as Source
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import qualified Distribution.Client.BuildReports.Anonymous as BuildReports
import qualified Distribution.Client.BuildReports.Storage as BuildReports
         ( storeAnonymous, storeLocal, fromInstallPlan )
import qualified Distribution.Client.InstallSymlink as InstallSymlink
         ( symlinkBinaries )
import qualified Distribution.Client.Win32SelfUpgrade as Win32SelfUpgrade
import qualified Distribution.Client.World as World
import Paths_cabal_install (getBinDir)

import Distribution.Simple.Compiler
         ( CompilerId(..), Compiler(compilerId), compilerFlavor
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Client.PackageIndex (PackageIndex)
import Distribution.Simple.Setup
         ( haddockCommand, HaddockFlags(..), emptyHaddockFlags
         , buildCommand, BuildFlags(..), emptyBuildFlags
         , toFlag, fromFlag, fromFlagOrDefault, flagToMaybe )
import qualified Distribution.Simple.Setup as Cabal
         ( installCommand, InstallFlags(..), emptyInstallFlags )
import Distribution.Simple.Utils
         ( rawSystemExit, comparing )
import Distribution.Simple.InstallDirs as InstallDirs
         ( PathTemplate, fromPathTemplate, toPathTemplate, substPathTemplate
         , initialPathTemplateEnv, installDirsTemplateEnv )
import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion
         , Package(..), PackageFixedDeps(..)
         , Dependency(..), thisPackageVersion )
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription
         ( Benchmark(..), PackageDescription, GenericPackageDescription(..)
         , TestSuite(..) )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription, mapTreeData )
import Distribution.Version
         ( Version, anyVersion, thisVersion )
import Distribution.Simple.Utils as Utils
         ( notice, info, debug, warn, die, intercalate, withTempDirectory )
import Distribution.Client.Utils
         ( inDir, mergeBy, MergeResult(..) )
import Distribution.System
         ( Platform, buildPlatform, OS(Windows), buildOS )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, showForCabal, verbose )
import Distribution.Simple.BuildPaths ( exeExtension )

--TODO:
-- * assign flags to packages individually
--   * complain about flags that do not apply to any package given as target
--     so flags do not apply to dependencies, only listed, can use flag
--     constraints for dependencies
--   * only record applicable flags in world file
-- * allow flag constraints
-- * allow installed constraints
-- * allow flag and installed preferences
-- * change world file to use cabal section syntax
--   * allow persistent configure flags for each package individually

-- ------------------------------------------------------------
-- * Top level user actions
-- ------------------------------------------------------------

-- | Installs the packages needed to satisfy a list of dependencies.
--
install, upgrade
  :: Verbosity
  -> PackageDBStack
  -> [Repo]
  -> Compiler
  -> ProgramConfiguration
  -> GlobalFlags
  -> ConfigFlags
  -> ConfigExFlags
  -> InstallFlags
  -> [UserTarget]
  -> IO ()
install verbosity packageDBs repos comp conf
  globalFlags configFlags configExFlags installFlags userTargets0 = do

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs conf
    sourcePkgDb       <- getSourcePackages    verbosity repos

    let -- For install, if no target is given it means we use the
        -- current directory as the single target
        userTargets | null userTargets0 = [UserTargetLocalDir "."]
                    | otherwise         = userTargets0

    pkgSpecifiers <- resolveUserTargets verbosity
                       (fromFlag $ globalWorldFile globalFlags)
                       (packageIndex sourcePkgDb)
                       userTargets

    notice verbosity "Resolving dependencies..."
    installPlan   <- foldProgress logMsg die return $
                       planPackages
                         comp configFlags configExFlags installFlags
                         installedPkgIndex sourcePkgDb pkgSpecifiers

    printPlanMessages verbosity installedPkgIndex installPlan dryRun

    unless dryRun $ do
      installPlan' <- performInstallations verbosity
                        context installedPkgIndex installPlan
      postInstallActions verbosity context userTargets installPlan'

  where
    context :: InstallContext
    context = (packageDBs, repos, comp, conf,
               globalFlags, configFlags, configExFlags, installFlags)

    dryRun      = fromFlag (installDryRun installFlags)
    logMsg message rest = debug verbosity message >> rest


upgrade _ _ _ _ _ _ _ _ _ _ = die $
    "Use the 'cabal install' command instead of 'cabal upgrade'.\n"
 ++ "You can install the latest version of a package using 'cabal install'. "
 ++ "The 'cabal upgrade' command has been removed because people found it "
 ++ "confusing and it often led to broken packages.\n"
 ++ "If you want the old upgrade behaviour then use the install command "
 ++ "with the --upgrade-dependencies flag (but check first with --dry-run "
 ++ "to see what would happen). This will try to pick the latest versions "
 ++ "of all dependencies, rather than the usual behaviour of trying to pick "
 ++ "installed versions of all dependencies. If you do use "
 ++ "--upgrade-dependencies, it is recommended that you do not upgrade core "
 ++ "packages (e.g. by using appropriate --constraint= flags)."

type InstallContext = ( PackageDBStack
                      , [Repo]
                      , Compiler
                      , ProgramConfiguration
                      , GlobalFlags
                      , ConfigFlags
                      , ConfigExFlags
                      , InstallFlags )

-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

planPackages :: Compiler
             -> ConfigFlags
             -> ConfigExFlags
             -> InstallFlags
             -> PackageIndex InstalledPackage
             -> SourcePackageDb
             -> [PackageSpecifier SourcePackage]
             -> Progress String String InstallPlan
planPackages comp configFlags configExFlags installFlags
             installedPkgIndex sourcePkgDb pkgSpecifiers =

        resolveDependencies
          buildPlatform (compilerId comp)
          resolverParams

    >>= if onlyDeps then adjustPlanOnlyDeps else return

  where
    resolverParams =

        setPreferenceDefault (if upgradeDeps then PreferAllLatest
                                             else PreferLatestForSelected)

      . addPreferences
          -- preferences from the config file or command line
          [ PackageVersionPreference name ver
          | Dependency name ver <- configPreferences configExFlags ]

      . addConstraints
          -- version constraints from the config file or command line
            (map userToPackageConstraint (configExConstraints configExFlags))

      . addConstraints
          --FIXME: this just applies all flags to all targets which
          -- is silly. We should check if the flags are appropriate
          [ PackageConstraintFlags (pkgSpecifierTarget pkgSpecifier) flags
          | let flags = configConfigurationsFlags configFlags
          , not (null flags)
          , pkgSpecifier <- pkgSpecifiers'' ]

      . (if reinstall then reinstallTargets else id)

      $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers''

    -- Mark test suites as enabled if invoked with '--enable-tests'. This
    -- ensures that test suite dependencies are included.
    pkgSpecifiers' = map enableTests pkgSpecifiers
    testsEnabled = fromFlagOrDefault False $ configTests configFlags
    enableTests (SpecificSourcePackage pkg) =
        let pkgDescr = Source.packageDescription pkg
            suites = condTestSuites pkgDescr
            enable = mapTreeData (\t -> t { testEnabled = testsEnabled })
        in SpecificSourcePackage $ pkg { Source.packageDescription = pkgDescr
            { condTestSuites = map (\(n, t) -> (n, enable t)) suites } }
    enableTests x = x

    -- Mark benchmarks as enabled if invoked with
    -- '--enable-benchmarks'. This ensures that benchmark dependencies
    -- are included.
    pkgSpecifiers'' = map enableBenchmarks pkgSpecifiers'
    benchmarksEnabled = fromFlagOrDefault False $ configBenchmarks configFlags
    enableBenchmarks (SpecificSourcePackage pkg) =
        let pkgDescr = Source.packageDescription pkg
            bms = condBenchmarks pkgDescr
            enable = mapTreeData (\t -> t { benchmarkEnabled = benchmarksEnabled })
        in SpecificSourcePackage $ pkg { Source.packageDescription = pkgDescr
            { condBenchmarks = map (\(n, t) -> (n, enable t)) bms } }
    enableBenchmarks x = x

    --TODO: this is a general feature and should be moved to D.C.Dependency
    -- Also, the InstallPlan.remove should return info more precise to the
    -- problem, rather than the very general PlanProblem type.
    adjustPlanOnlyDeps :: InstallPlan -> Progress String String InstallPlan
    adjustPlanOnlyDeps =
        either (Fail . explain) Done
      . InstallPlan.remove isTarget
      where
        isTarget pkg = packageName pkg `elem` targetnames
        targetnames  = map pkgSpecifierTarget pkgSpecifiers
        
        explain :: [InstallPlan.PlanProblem] -> String
        explain problems =
            "Cannot select only the dependencies (as requested by the "
         ++ "'--only-dependencies' flag), "
         ++ (case pkgids of
               [pkgid] -> "the package " ++ display pkgid ++ " is "
               _       -> "the packages "
                       ++ intercalate ", " (map display pkgids) ++ " are ")
         ++ "required by a dependency of one of the other targets."
          where
            pkgids =
              nub [ depid
                  | InstallPlan.PackageMissingDeps _ depids <- problems
                  , depid <- depids
                  , packageName depid `elem` targetnames ]

    reinstall   = fromFlag (installReinstall installFlags)
    upgradeDeps = fromFlag (installUpgradeDeps installFlags)
    onlyDeps    = fromFlag (installOnlyDeps installFlags)

-- ------------------------------------------------------------
-- * Informational messages
-- ------------------------------------------------------------

printPlanMessages :: Verbosity
                  -> PackageIndex InstalledPackage
                  -> InstallPlan
                  -> Bool
                  -> IO ()
printPlanMessages verbosity installed installPlan dryRun = do

  when nothingToInstall $
    notice verbosity $
         "No packages to be installed. All the requested packages are "
      ++ "already installed.\n If you want to reinstall anyway then use "
      ++ "the --reinstall flag."

  when (dryRun || verbosity >= verbose) $
    printDryRun verbosity installed installPlan

  where
    nothingToInstall = null (InstallPlan.ready installPlan)


printDryRun :: Verbosity
            -> PackageIndex InstalledPackage
            -> InstallPlan
            -> IO ()
printDryRun verbosity installedPkgIndex plan = case unfoldr next plan of
  []   -> return ()
  pkgs
    | verbosity >= Verbosity.verbose -> notice verbosity $ unlines $
        "In order, the following would be installed:"
      : map showPkgAndReason pkgs
    | otherwise -> notice verbosity $ unlines $
        "In order, the following would be installed (use -v for more details):"
      : map (display . packageId) pkgs
  where
    next plan' = case InstallPlan.ready plan' of
      []      -> Nothing
      (pkg:_) -> Just (pkg, InstallPlan.completed pkgid result plan')
        where pkgid = packageId pkg
              result = BuildOk DocsNotTried TestsNotTried
              --FIXME: This is a bit of a hack,
              -- pretending that each package is installed

    showPkgAndReason pkg' = display (packageId pkg') ++ " " ++
          case PackageIndex.lookupPackageName installedPkgIndex
                                              (packageName pkg') of
            [] -> "(new package)"
            ps ->  case find ((==packageId pkg') . packageId) ps of
              Nothing  -> "(new version)"
              Just pkg -> "(reinstall)" ++ case changes pkg pkg' of
                []   -> ""
                diff -> " changes: "  ++ intercalate ", " diff
    changes pkg pkg' = map change . filter changed
                     $ mergeBy (comparing packageName)
                         (nub . sort . depends $ pkg)
                         (nub . sort . depends $ pkg')
    change (OnlyInLeft pkgid)        = display pkgid ++ " removed"
    change (InBoth     pkgid pkgid') = display pkgid ++ " -> "
                                    ++ display (packageVersion pkgid')
    change (OnlyInRight      pkgid') = display pkgid' ++ " added"
    changed (InBoth    pkgid pkgid') = pkgid /= pkgid'
    changed _                        = True

-- ------------------------------------------------------------
-- * Post installation stuff
-- ------------------------------------------------------------

-- | Various stuff we do after successful or unsuccessfully installing a bunch
-- of packages. This includes:
--
--  * build reporting, local and remote
--  * symlinking binaries
--  * updating indexes
--  * updating world file
--  * error reporting
--
postInstallActions :: Verbosity
                   -> InstallContext
                   -> [UserTarget]
                   -> InstallPlan
                   -> IO ()
postInstallActions verbosity
  (packageDBs, _, comp, conf, globalFlags, configFlags, _, installFlags)
  targets installPlan = do

  unless oneShot $
    World.insert verbosity worldFile
      --FIXME: does not handle flags
      [ World.WorldPkgInfo dep []
      | UserTargetNamed dep <- targets ]

  let buildReports = BuildReports.fromInstallPlan installPlan
  BuildReports.storeLocal (installSummaryFile installFlags) buildReports
  when (reportingLevel >= AnonymousReports) $
    BuildReports.storeAnonymous buildReports
  when (reportingLevel == DetailedReports) $
    storeDetailedBuildReports verbosity logsDir buildReports

  regenerateHaddockIndex verbosity packageDBs comp conf
                         configFlags installFlags installPlan

  symlinkBinaries verbosity configFlags installFlags installPlan

  printBuildFailures installPlan

  where
    reportingLevel = fromFlag (installBuildReports installFlags)
    logsDir        = fromFlag (globalLogsDir globalFlags)
    oneShot        = fromFlag (installOneShot installFlags)
    worldFile      = fromFlag $ globalWorldFile globalFlags

storeDetailedBuildReports :: Verbosity -> FilePath
                          -> [(BuildReports.BuildReport, Repo)] -> IO ()
storeDetailedBuildReports verbosity logsDir reports = sequence_
  [ do dotCabal <- defaultCabalDir
       let logFileName = display (BuildReports.package report) <.> "log"
           logFile     = logsDir </> logFileName
           reportsDir  = dotCabal </> "reports" </> remoteRepoName remoteRepo
           reportFile  = reportsDir </> logFileName

       handleMissingLogFile $ do
         buildLog <- readFile logFile
         createDirectoryIfMissing True reportsDir -- FIXME
         writeFile reportFile (show (BuildReports.show report, buildLog))

  | (report, Repo { repoKind = Left remoteRepo }) <- reports
  , isLikelyToHaveLogFile (BuildReports.installOutcome report) ]

  where
    isLikelyToHaveLogFile BuildReports.ConfigureFailed {} = True
    isLikelyToHaveLogFile BuildReports.BuildFailed     {} = True
    isLikelyToHaveLogFile BuildReports.InstallFailed   {} = True
    isLikelyToHaveLogFile BuildReports.InstallOk       {} = True
    isLikelyToHaveLogFile _                               = False

    handleMissingLogFile = Exception.handleJust missingFile $ \ioe ->
      warn verbosity $ "Missing log file for build report: "
                    ++ fromMaybe ""  (ioeGetFileName ioe)

#if MIN_VERSION_base(4,0,0)
    missingFile ioe
#else
    missingFile (IOException ioe)
#endif
      | isDoesNotExistError ioe  = Just ioe
    missingFile _                = Nothing


regenerateHaddockIndex :: Verbosity
                       -> [PackageDB]
                       -> Compiler
                       -> ProgramConfiguration
                       -> ConfigFlags
                       -> InstallFlags
                       -> InstallPlan
                       -> IO ()
regenerateHaddockIndex verbosity packageDBs comp conf
                       configFlags installFlags installPlan
  | haddockIndexFileIsRequested && shouldRegenerateHaddockIndex = do

  defaultDirs <- InstallDirs.defaultInstallDirs
                   (compilerFlavor comp)
                   (fromFlag (configUserInstall configFlags))
                   True
  let indexFileTemplate = fromFlag (installHaddockIndex installFlags)
      indexFile = substHaddockIndexFileName defaultDirs indexFileTemplate

  notice verbosity $
     "Updating documentation index " ++ indexFile

  --TODO: might be nice if the install plan gave us the new InstalledPackageInfo
  installedPkgIndex <- getInstalledPackages verbosity comp packageDBs conf
  Haddock.regenerateHaddockIndex verbosity installedPkgIndex conf indexFile

  | otherwise = return ()
  where
    haddockIndexFileIsRequested =
         fromFlag (installDocumentation installFlags)
      && isJust (flagToMaybe (installHaddockIndex installFlags))

    -- We want to regenerate the index if some new documentation was actually
    -- installed. Since the index is per-user, we don't do it for global
    -- installs or special cases where we're installing into a specific db.
    shouldRegenerateHaddockIndex = normalUserInstall
                                && someDocsWereInstalled installPlan
      where
        someDocsWereInstalled = any installedDocs . InstallPlan.toList
        normalUserInstall     = (UserPackageDB `elem` packageDBs)
                             && all (not . isSpecificPackageDB) packageDBs

        installedDocs (InstallPlan.Installed _ (BuildOk DocsOk _)) = True
        installedDocs _                                            = False
        isSpecificPackageDB (SpecificPackageDB _) = True
        isSpecificPackageDB _                     = False

    substHaddockIndexFileName defaultDirs = fromPathTemplate
                                          . substPathTemplate env
      where
        env  = env0 ++ installDirsTemplateEnv absoluteDirs
        env0 = InstallDirs.compilerTemplateEnv (compilerId comp)
            ++ InstallDirs.platformTemplateEnv (buildPlatform)
        absoluteDirs = InstallDirs.substituteInstallDirTemplates
                         env0 templateDirs
        templateDirs = InstallDirs.combineInstallDirs fromFlagOrDefault
                         defaultDirs (configInstallDirs configFlags)


symlinkBinaries :: Verbosity
                -> ConfigFlags
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
    bindir = fromFlag (installSymlinkBinDir installFlags)


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
      DownloadFailed  e -> " failed while downloading the package."
                        ++ " The exception was:\n  " ++ show e
      UnpackFailed    e -> " failed while unpacking the package."
                        ++ " The exception was:\n  " ++ show e
      ConfigureFailed e -> " failed during the configure step."
                        ++ " The exception was:\n  " ++ show e
      BuildFailed     e -> " failed during the building phase."
                        ++ " The exception was:\n  " ++ show e
      InstallFailed   e -> " failed during the final install step."
                        ++ " The exception was:\n  " ++ show e


-- ------------------------------------------------------------
-- * Actually do the installations
-- ------------------------------------------------------------

data InstallMisc = InstallMisc {
    rootCmd    :: Maybe FilePath,
    libVersion :: Maybe Version
  }

performInstallations :: Verbosity
                     -> InstallContext
                     -> PackageIndex InstalledPackage
                     -> InstallPlan
                     -> IO InstallPlan
performInstallations verbosity
  (packageDBs, _, comp, conf,
   globalFlags, configFlags, configExFlags, installFlags)
  installedPkgIndex installPlan = do

  executeInstallPlan installPlan $ \cpkg ->
    installConfiguredPackage platform compid configFlags
                             cpkg $ \configFlags' src pkg ->
      fetchSourcePackage verbosity src $ \src' ->
        installLocalPackage verbosity (packageId pkg) src' $ \mpath ->
          installUnpackedPackage verbosity
                                 (setupScriptOptions installedPkgIndex)
                                 miscOptions configFlags' installFlags
                                 compid pkg mpath useLogFile

  where
    platform = InstallPlan.planPlatform installPlan
    compid   = InstallPlan.planCompiler installPlan

    setupScriptOptions index = SetupScriptOptions {
      useCabalVersion  = maybe anyVersion thisVersion (libVersion miscOptions),
      useCompiler      = Just comp,
      -- Hack: we typically want to allow the UserPackageDB for finding the
      -- Cabal lib when compiling any Setup.hs even if we're doing a global
      -- install. However we also allow looking in a specific package db.
      usePackageDB     = if UserPackageDB `elem` packageDBs
                           then packageDBs
                           else let (db@GlobalPackageDB:dbs) = packageDBs
                                 in db : UserPackageDB : dbs,
                                --TODO: use Ord instance:
                                -- insert UserPackageDB packageDBs
      usePackageIndex  = if UserPackageDB `elem` packageDBs
                           then Just index
                           else Nothing,
      useProgramConfig = conf,
      useDistPref      = fromFlagOrDefault
                           (useDistPref defaultSetupScriptOptions)
                           (configDistPref configFlags),
      useLoggingHandle = Nothing,
      useWorkingDir    = Nothing
    }
    reportingLevel = fromFlag (installBuildReports installFlags)
    logsDir        = fromFlag (globalLogsDir globalFlags)
    useLogFile :: Maybe (PackageIdentifier -> FilePath)
    useLogFile = fmap substLogFileName logFileTemplate
      where
        logFileTemplate :: Maybe PathTemplate
        logFileTemplate --TODO: separate policy from mechanism
          | reportingLevel == DetailedReports
          = Just $ toPathTemplate $ logsDir </> "$pkgid" <.> "log"
          | otherwise
          = flagToMaybe (installLogFile installFlags)
    substLogFileName template pkg = fromPathTemplate
                                  . substPathTemplate env
                                  $ template
      where env = initialPathTemplateEnv (packageId pkg) (compilerId comp)
    miscOptions  = InstallMisc {
      rootCmd    = if fromFlag (configUserInstall configFlags)
                     then Nothing      -- ignore --root-cmd if --user.
                     else flagToMaybe (installRootCmd installFlags),
      libVersion = flagToMaybe (configCabalVersion configExFlags)
    }


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


-- | Call an installer for an 'SourcePackage' but override the configure
-- flags with the ones given by the 'ConfiguredPackage'. In particular the
-- 'ConfiguredPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
installConfiguredPackage :: Platform -> CompilerId
                         ->  ConfigFlags -> ConfiguredPackage
                         -> (ConfigFlags -> PackageLocation (Maybe FilePath)
                                         -> PackageDescription -> a)
                         -> a
installConfiguredPackage platform comp configFlags
  (ConfiguredPackage (SourcePackage _ gpkg source) flags deps)
  installPkg = installPkg configFlags {
    configConfigurationsFlags = flags,
    configConstraints = map thisPackageVersion deps
  } source pkg
  where
    pkg = case finalizePackageDescription flags
           (const True)
           platform comp [] gpkg of
      Left _ -> error "finalizePackageDescription ConfiguredPackage failed"
      Right (desc, _) -> desc

fetchSourcePackage
  :: Verbosity
  -> PackageLocation (Maybe FilePath)
  -> (PackageLocation FilePath -> IO BuildResult)
  -> IO BuildResult
fetchSourcePackage verbosity src installPkg = do
  fetched <- checkFetched src
  case fetched of
    Just src' -> installPkg src'
    Nothing   -> onFailure DownloadFailed $
                   fetchPackage verbosity src >>= installPkg


installLocalPackage
  :: Verbosity -> PackageIdentifier -> PackageLocation FilePath
  -> (Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installLocalPackage verbosity pkgid location installPkg = case location of

    LocalUnpackedPackage dir ->
      installPkg (Just dir)

    LocalTarballPackage tarballPath ->
      installLocalTarballPackage verbosity pkgid tarballPath installPkg

    RemoteTarballPackage _ tarballPath ->
      installLocalTarballPackage verbosity pkgid tarballPath installPkg

    RepoTarballPackage _ _ tarballPath ->
      installLocalTarballPackage verbosity pkgid tarballPath installPkg


installLocalTarballPackage
  :: Verbosity -> PackageIdentifier -> FilePath
  -> (Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installLocalTarballPackage verbosity pkgid tarballPath installPkg = do
  tmp <- getTemporaryDirectory
  withTempDirectory verbosity tmp (display pkgid) $ \tmpDirPath ->
    onFailure UnpackFailed $ do
      info verbosity $ "Extracting " ++ tarballPath
                    ++ " to " ++ tmpDirPath ++ "..."
      let relUnpackedPath = display pkgid
          absUnpackedPath = tmpDirPath </> relUnpackedPath
          descFilePath = absUnpackedPath
                     </> display (packageName pkgid) <.> "cabal"
      extractTarGzFile tmpDirPath relUnpackedPath tarballPath
      exists <- doesFileExist descFilePath
      when (not exists) $
        die $ "Package .cabal file not found: " ++ show descFilePath
      installPkg (Just absUnpackedPath)


installUnpackedPackage :: Verbosity
                   -> SetupScriptOptions
                   -> InstallMisc
                   -> ConfigFlags
                   -> InstallFlags
                   -> CompilerId
                   -> PackageDescription
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> Maybe (PackageIdentifier -> FilePath) -- ^ File to log output to (if any)
                   -> IO BuildResult
installUnpackedPackage verbosity scriptOptions miscOptions
                       configFlags installConfigFlags
                       compid pkg workingDir useLogFile =

  -- Configure phase
  onFailure ConfigureFailed $ do
    setup configureCommand configureFlags

  -- Build phase
    onFailure BuildFailed $ do
      setup buildCommand' buildFlags

  -- Doc generation phase
      docsResult <- if shouldHaddock
        then (do setup haddockCommand haddockFlags
                 return DocsOk)
               `catchIO`   (\_ -> return DocsFailed)
               `catchExit` (\_ -> return DocsFailed)
        else return DocsNotTried

  -- Tests phase
      testsResult <- return TestsNotTried  --TODO: add optional tests

  -- Install phase
      onFailure InstallFailed $
        withWin32SelfUpgrade verbosity configFlags compid pkg $ do
          case rootCmd miscOptions of
            (Just cmd) -> reexec cmd
            Nothing    -> setup Cabal.installCommand installFlags
          return (Right (BuildOk docsResult testsResult))

  where
    configureFlags   = filterConfigureFlags configFlags {
      configVerbosity = toFlag verbosity'
    }
    buildCommand'    = buildCommand defaultProgramConfiguration
    buildFlags   _   = emptyBuildFlags {
      buildDistPref  = configDistPref configFlags,
      buildVerbosity = toFlag verbosity'
    }
    shouldHaddock    = fromFlag (installDocumentation installConfigFlags)
    haddockFlags _   = emptyHaddockFlags {
      haddockDistPref  = configDistPref configFlags,
      haddockVerbosity = toFlag verbosity'
    }
    installFlags _   = Cabal.emptyInstallFlags {
      Cabal.installDistPref  = configDistPref configFlags,
      Cabal.installVerbosity = toFlag verbosity'
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
        (Just pkg)
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
onFailure :: (SomeException -> BuildFailure) -> IO BuildResult -> IO BuildResult
onFailure result action =
#if MIN_VERSION_base(4,0,0)
  action `catches`
    [ Handler $ \ioe  -> handler (ioe  :: IOException)
    , Handler $ \exit -> handler (exit :: ExitCode)
    ]
  where
    handler :: Exception e => e -> IO BuildResult
    handler = return . Left . result . toException
#else
  action
    `catchIO`   (return . Left . result . IOException)
    `catchExit` (return . Left . result . ExitException)
#endif


-- ------------------------------------------------------------
-- * Wierd windows hacks
-- ------------------------------------------------------------

withWin32SelfUpgrade :: Verbosity
                     -> ConfigFlags
                     -> CompilerId
                     -> PackageDescription
                     -> IO a -> IO a
withWin32SelfUpgrade _ _ _ _ action | buildOS /= Windows = action
withWin32SelfUpgrade verbosity configFlags compid pkg action = do

  defaultDirs <- InstallDirs.defaultInstallDirs
                   compFlavor
                   (fromFlag (configUserInstall configFlags))
                   (PackageDescription.hasLibs pkg)

  Win32SelfUpgrade.possibleSelfUpgrade verbosity
    (exeInstallPaths defaultDirs) action

  where
    pkgid = packageId pkg
    (CompilerId compFlavor _) = compid

    exeInstallPaths defaultDirs =
      [ InstallDirs.bindir absoluteDirs </> exeName <.> exeExtension
      | exe <- PackageDescription.executables pkg
      , PackageDescription.buildable (PackageDescription.buildInfo exe)
      , let exeName = prefix ++ PackageDescription.exeName exe ++ suffix
            prefix  = substTemplate prefixTemplate
            suffix  = substTemplate suffixTemplate ]
      where
        fromFlagTemplate = fromFlagOrDefault (InstallDirs.toPathTemplate "")
        prefixTemplate = fromFlagTemplate (configProgPrefix configFlags)
        suffixTemplate = fromFlagTemplate (configProgSuffix configFlags)
        templateDirs   = InstallDirs.combineInstallDirs fromFlagOrDefault
                           defaultDirs (configInstallDirs configFlags)
        absoluteDirs   = InstallDirs.absoluteInstallDirs
                           pkgid compid InstallDirs.NoCopyDest templateDirs
        substTemplate  = InstallDirs.fromPathTemplate
                       . InstallDirs.substPathTemplate env
          where env = InstallDirs.initialPathTemplateEnv pkgid compid
