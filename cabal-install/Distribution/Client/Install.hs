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
         ( unfoldr, find, nub, sort, partition )
import Data.Maybe
         ( isJust, fromMaybe )
import qualified Data.Map as Map
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

import Distribution.Client.Dependency
         ( resolveDependenciesWithProgress
         , PackageConstraint(..), dependencyConstraints, dependencyTargets
         , PackagesPreference(..), PackagesPreferenceDefault(..)
         , PackagePreference(..)
         , Progress(..), foldProgress, )
import Distribution.Client.Fetch
         ( fetchPackage )
import Distribution.Client.HttpUtils
         ( downloadURI )
import qualified Distribution.Client.Haddock as Haddock (regenerateHaddockIndex)
-- import qualified Distribution.Client.Info as Info
import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages, disambiguateDependencies
         , getInstalledPackages )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.Setup
         ( GlobalFlags(..)
         , ConfigFlags(..), configureCommand, filterConfigureFlags
         , ConfigExFlags(..), InstallFlags(..) )
import Distribution.Client.Config
         ( defaultCabalDir )
import Distribution.Client.Tar (extractTarGzFile)
import Distribution.Client.Types as Available
         ( UnresolvedDependency(..), AvailablePackage(..)
         , AvailablePackageSource(..), AvailablePackageDb(..)
         , Repo(..), ConfiguredPackage(..)
         , BuildResult, BuildFailure(..), BuildSuccess(..)
         , DocsResult(..), TestsResult(..), RemoteRepo(..)
         , InstalledPackage )
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
         ( defaultPackageDesc, rawSystemExit, comparing )
import Distribution.Simple.InstallDirs as InstallDirs
         ( PathTemplate, fromPathTemplate, toPathTemplate, substPathTemplate
         , initialPathTemplateEnv, installDirsTemplateEnv )
import Distribution.Package
         ( PackageName(..), PackageIdentifier, packageName, packageVersion
         , Package(..), PackageFixedDeps(..)
         , Dependency(..), thisPackageVersion )
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription
         ( PackageDescription )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Version
         ( Version, VersionRange, anyVersion, thisVersion )
import Distribution.Simple.Utils as Utils
         ( notice, info, warn, die, intercalate, withTempDirectory )
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
-- * add --upgrade-deps flag
-- * eliminate upgrade, replaced by --upgrade-deps and world target
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

-- | An installation target given by the user. At the moment this
-- is just a named package, possibly with a version constraint.
-- It should be generalised to handle other targets like http or dirs.
--
type InstallTarget = UnresolvedDependency

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
  -> [InstallTarget]
  -> IO ()
install verbosity packageDB repos comp conf
  globalFlags configFlags configExFlags installFlags targets =

    installWithPlanner verbosity context (planner onlyDeps) targets

  where
    context :: InstallContext
    context = (packageDB, repos, comp, conf,
               globalFlags, configFlags, configExFlags, installFlags)

    onlyDeps = fromFlag (installOnlyDeps installFlags)

    planner :: Bool -> Planner
    planner
      | null targets = planLocalPackage verbosity
                         comp configFlags configExFlags

      | otherwise    = planRepoPackages defaultPref
                         comp globalFlags configFlags configExFlags
                         installFlags targets

    defaultPref
      | fromFlag (installUpgradeDeps installFlags) = PreferAllLatest
      | otherwise                                  = PreferLatestForSelected


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


type Planner = PackageIndex InstalledPackage
            -> AvailablePackageDb
            -> IO (Progress String String InstallPlan)

type InstallContext = ( PackageDBStack
                      , [Repo]
                      , Compiler
                      , ProgramConfiguration
                      , GlobalFlags
                      , ConfigFlags
                      , ConfigExFlags
                      , InstallFlags )

-- | Top-level orchestration. Installs the packages generated by a planner.
--
installWithPlanner :: Verbosity
                   -> InstallContext
                   -> Planner
                   -> [UnresolvedDependency]
                   -> IO ()
installWithPlanner verbosity
  context@(packageDBs, repos, comp, conf, _, _, _, installFlags)
  planner targets = do

  installed   <- getInstalledPackages verbosity comp packageDBs conf
  available   <- getAvailablePackages verbosity repos

  notice verbosity "Resolving dependencies..."
  installPlan <- foldProgress logMsg die return =<< planner installed available

  printPlanMessages verbosity installed installPlan dryRun

  unless dryRun $
        performInstallations verbosity context installed installPlan
    >>= postInstallActions verbosity context targets

  where
    dryRun = fromFlag (installDryRun installFlags)
    logMsg message rest = info verbosity message >> rest

-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
--
planLocalPackage :: Verbosity
                 -> Compiler
                 -> ConfigFlags
                 -> ConfigExFlags
                 -> Bool
                 -> Planner
planLocalPackage verbosity comp configFlags configExFlags onlyDeps installed
  (AvailablePackageDb available availablePrefs) = do
  pkg <- readPackageDescription verbosity =<< defaultPackageDesc verbosity
  let -- The trick is, we add the local package to the available index and
      -- remove it from the installed index. Then we ask to resolve a
      -- dependency on exactly that package. So the resolver ends up having
      -- to pick the local package.
      available' = PackageIndex.insert localPkg available
      installed' = PackageIndex.deletePackageId (packageId localPkg) installed
      localPkg = AvailablePackage {
        packageInfoId                = packageId pkg,
        Available.packageDescription = pkg,
        packageSource                = LocalUnpackedPackage Nothing
      }
      targets     = [packageName pkg]
      constraints = [PackageVersionConstraint (packageName pkg)
                       (thisVersion (packageVersion pkg))
                    ,PackageFlagsConstraint   (packageName pkg)
                       (configConfigurationsFlags configFlags)]
                 ++ [ PackageVersionConstraint name ver
                    | Dependency name ver <- configConstraints configFlags ]
      preferences = mergePackagePrefs PreferLatestForSelected
                                      availablePrefs configExFlags

      plan = resolveDependenciesWithProgress buildPlatform
             (compilerId comp)
             installed' available' preferences constraints targets

      isLocalPackage = (== localPackageName) . packageName
      localPackageName = packageName pkg

  -- If we only want dependencies, then remove the local package from
  -- the install plan after we have built it.
  return $ if onlyDeps then removeFromPlan isLocalPackage plan else plan

-- | Make an 'InstallPlan' for the given dependencies.
--
planRepoPackages :: PackagesPreferenceDefault
                 -> Compiler
                 -> GlobalFlags
                 -> ConfigFlags
                 -> ConfigExFlags
                 -> InstallFlags
                 -> [UnresolvedDependency]
                 -> Bool
                 -> Planner
planRepoPackages defaultPref comp
  globalFlags configFlags configExFlags installFlags
  deps onlyDeps installed (AvailablePackageDb available availablePrefs) = do

  deps' <- addWorldPackages deps
       >>= IndexUtils.disambiguateDependencies available

  let installed'
        | fromFlag (installReinstall installFlags)
                    = hideGivenDeps deps' installed
        | otherwise = installed
      targets     = dependencyTargets deps'
      constraints = dependencyConstraints deps'
                 ++ [ PackageVersionConstraint name ver
                    | Dependency name ver <- configConstraints configFlags ]
      preferences = mergePackagePrefs defaultPref availablePrefs configExFlags
      plan = resolveDependenciesWithProgress buildPlatform (compilerId comp)
             installed' available preferences constraints targets
  return $ if onlyDeps then removeFromPlan (isGivenDep deps) plan else plan
  where
    hideGivenDeps pkgs index =
      foldr PackageIndex.deletePackageName index (givenDepNames pkgs)

    givenDepNames pkgs =
        [ name | UnresolvedDependency (Dependency name _) _ <- pkgs ]

    isGivenDep pkgs = (`elem` givenDepNames pkgs) . packageName

    addWorldPackages :: [UnresolvedDependency] -> IO [UnresolvedDependency]
    addWorldPackages targets = case partition World.isWorldTarget targets of
      ([], _)               -> return targets
      (world, otherTargets) -> do
        unless (all World.isGoodWorldTarget world) $
          die $ "The virtual package 'world' does not take any version "
             ++ "or configuration flags."
        worldTargets <- World.getContents worldFile
        --TODO: should we warn if there are no world targets?
        return (otherTargets ++ worldTargets)
      where
        worldFile = fromFlag $ globalWorldFile globalFlags


-- | Adapt InstallPlan.removePackages to work on Progress so that it
-- can be integrated with the planners
--
removeFromPlan :: (InstallPlan.PlanPackage -> Bool)
               -> Progress step String InstallPlan
               -> Progress step String InstallPlan
removeFromPlan shouldRemove =
    foldProgress Step Fail $ \plan ->
    case InstallPlan.removePackages shouldRemove plan of
      Right plan'   -> Done plan'
      Left problems ->
          Fail $ unlines $ map InstallPlan.showPlanProblem problems

mergePackagePrefs :: PackagesPreferenceDefault
                  -> Map.Map PackageName VersionRange
                  -> ConfigExFlags
                  -> PackagesPreference
mergePackagePrefs defaultPref availablePrefs configExFlags =
  PackagesPreference defaultPref $
       -- The preferences that come from the hackage index
       [ PackageVersionPreference name ver
       | (name, ver) <- Map.toList availablePrefs ]
       -- additional preferences from the config file or command line
    ++ [ PackageVersionPreference name ver
       | Dependency name ver <- configPreferences configExFlags ]


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
printDryRun verbosity installed plan = case unfoldr next plan of
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
          case PackageIndex.lookupPackageName installed (packageName pkg') of
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
                   -> [InstallTarget]
                   -> InstallPlan
                   -> IO ()
postInstallActions verbosity
  (packageDBs, _, comp, conf, globalFlags, configFlags, _, installFlags)
  targets installPlan = do

  unless oneShot $
    World.insert verbosity worldFile targets'

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
    targets'       = filter (not . World.isWorldTarget) targets

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
  installed <- getInstalledPackages verbosity comp packageDBs conf
  Haddock.regenerateHaddockIndex verbosity installed conf indexFile

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
  installed installPlan = do

  executeInstallPlan installPlan $ \cpkg ->
    installConfiguredPackage platform compid configFlags
                             cpkg $ \configFlags' src pkg ->
      installAvailablePackage verbosity (packageId pkg) src $ \mpath ->
        installUnpackedPackage verbosity (setupScriptOptions installed)
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


-- | Call an installer for an 'AvailablePackage' but override the configure
-- flags with the ones given by the 'ConfiguredPackage'. In particular the
-- 'ConfiguredPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
installConfiguredPackage :: Platform -> CompilerId
                         ->  ConfigFlags -> ConfiguredPackage
                         -> (ConfigFlags -> AvailablePackageSource
                                         -> PackageDescription -> a)
                         -> a
installConfiguredPackage platform comp configFlags
  (ConfiguredPackage (AvailablePackage _ gpkg source) flags deps)
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


installAvailablePackage
  :: Verbosity -> PackageIdentifier -> AvailablePackageSource
  -> (Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installAvailablePackage _ _ (LocalUnpackedPackage dir) installPkg =
  installPkg dir

installAvailablePackage verbosity pkgid
                        (LocalTarballPackage tarballPath) installPkg = do
  tmp <- getTemporaryDirectory
  withTempDirectory verbosity tmp (display pkgid) $ \tmpDirPath ->
    installLocalTarballPackage verbosity pkgid
                               tarballPath tmpDirPath installPkg

installAvailablePackage verbosity pkgid
                        (RemoteTarballPackage tarballURL) installPkg = do
  tmp <- getTemporaryDirectory
  withTempDirectory verbosity tmp (display pkgid) $ \tmpDirPath ->
    onFailure DownloadFailed $ do
      let tarballPath = tmpDirPath </> display pkgid <.> "tar.gz"
      --TODO: perhaps we've already had to download this to a local cache
      --      so we even know what package version it is. So might be able
      --      to get it from the local cache rather than from remote.
      downloadURI verbosity tarballURL tarballPath
      installLocalTarballPackage verbosity pkgid
                                 tarballPath tmpDirPath installPkg

installAvailablePackage verbosity pkgid (RepoTarballPackage repo) installPkg =
  onFailure DownloadFailed $ do
    tarballPath <- fetchPackage verbosity repo pkgid
    tmp <- getTemporaryDirectory
    withTempDirectory verbosity tmp (display pkgid) $ \tmpDirPath ->
      installLocalTarballPackage verbosity pkgid
                                 tarballPath tmpDirPath installPkg

installLocalTarballPackage
  :: Verbosity -> PackageIdentifier -> FilePath -> FilePath
  -> (Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installLocalTarballPackage verbosity pkgid tarballPath tmpDirPath installPkg =
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
