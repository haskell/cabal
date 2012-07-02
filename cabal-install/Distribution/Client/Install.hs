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
         ( unfoldr, nub, sort, (\\) )
import Data.Maybe
         ( isJust, fromMaybe, maybeToList )
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
         ( openFile, IOMode(AppendMode), stdout, hFlush )
import System.IO.Error
         ( isDoesNotExistError, ioeGetFileName )

import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
         ( Solver(..) )
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
import qualified Distribution.InstalledPackageInfo as Installed
import Paths_cabal_install (getBinDir)
import Distribution.Client.JobControl

import Distribution.Simple.Compiler
         ( CompilerId(..), Compiler(compilerId), compilerFlavor
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Setup
         ( haddockCommand, HaddockFlags(..)
         , buildCommand, BuildFlags(..), emptyBuildFlags
         , toFlag, fromFlag, fromFlagOrDefault, flagToMaybe )
import qualified Distribution.Simple.Setup as Cabal
         ( installCommand, InstallFlags(..), emptyInstallFlags
         , emptyTestFlags, testCommand )
import Distribution.Simple.Utils
         ( rawSystemExit, comparing )
import Distribution.Simple.InstallDirs as InstallDirs
         ( PathTemplate, fromPathTemplate, toPathTemplate, substPathTemplate
         , initialPathTemplateEnv, installDirsTemplateEnv )
import Distribution.Package
         ( PackageIdentifier, PackageId, packageName, packageVersion
         , Package(..), PackageFixedDeps(..)
         , Dependency(..), thisPackageVersion, InstalledPackageId )
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription
         ( PackageDescription, GenericPackageDescription(..), Flag(..)
         , FlagName(..), FlagAssignment )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Version
         ( Version, anyVersion, thisVersion )
import Distribution.Simple.Utils as Utils
         ( notice, info, warn, die, intercalate, withTempDirectory )
import Distribution.Client.Utils
         ( inDir, mergeBy, MergeResult(..) )
import Distribution.System
         ( Platform, buildPlatform, OS(Windows), buildOS )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, showForCabal, verbose, deafening )
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
  -> HaddockFlags
  -> [UserTarget]
  -> IO ()
install verbosity packageDBs repos comp conf
  globalFlags configFlags configExFlags installFlags haddockFlags userTargets0 = do

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs conf
    sourcePkgDb       <- getSourcePackages    verbosity repos

    solver <- chooseSolver verbosity (fromFlag (configSolver  configExFlags)) (compilerId comp)

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
                         comp solver configFlags configExFlags installFlags
                         installedPkgIndex sourcePkgDb pkgSpecifiers

    checkPrintPlan verbosity installedPkgIndex installPlan installFlags pkgSpecifiers

    unless dryRun $ do
      installPlan' <- performInstallations verbosity
                        context installedPkgIndex installPlan
      postInstallActions verbosity context userTargets installPlan'

  where
    context :: InstallContext
    context = (packageDBs, repos, comp, conf,
               globalFlags, configFlags, configExFlags, installFlags, haddockFlags)

    dryRun      = fromFlag (installDryRun installFlags)
    logMsg message rest = debugNoWrap message >> rest
    -- Solver debug output really looks better without automatic
    -- line wrapping. TODO: This should probably be moved into
    -- the utilities module.
    debugNoWrap xs = when (verbosity >= deafening) (putStrLn xs >> hFlush stdout)

upgrade _ _ _ _ _ _ _ _ _ _ _ = die $
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
                      , InstallFlags
                      , HaddockFlags )

-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

planPackages :: Compiler
             -> Solver
             -> ConfigFlags
             -> ConfigExFlags
             -> InstallFlags
             -> PackageIndex
             -> SourcePackageDb
             -> [PackageSpecifier SourcePackage]
             -> Progress String String InstallPlan
planPackages comp solver configFlags configExFlags installFlags
             installedPkgIndex sourcePkgDb pkgSpecifiers =

        resolveDependencies
          buildPlatform (compilerId comp)
          solver
          resolverParams

    >>= if onlyDeps then adjustPlanOnlyDeps else return

  where
    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . setAvoidReinstalls avoidReinstalls

      . setShadowPkgs shadowPkgs

      . setPreferenceDefault (if upgradeDeps then PreferAllLatest
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
          , pkgSpecifier <- pkgSpecifiers ]

      . addConstraints
          [ PackageConstraintStanzas (pkgSpecifierTarget pkgSpecifier) stanzas
          | pkgSpecifier <- pkgSpecifiers ]

      . (if reinstall then reinstallTargets else id)

      $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

    stanzas = concat
        [ if testsEnabled then [TestStanzas] else []
        , if benchmarksEnabled then [BenchStanzas] else []
        ]
    testsEnabled = fromFlagOrDefault False $ configTests configFlags
    benchmarksEnabled = fromFlagOrDefault False $ configBenchmarks configFlags

    --TODO: this is a general feature and should be moved to D.C.Dependency
    -- Also, the InstallPlan.remove should return info more precise to the
    -- problem, rather than the very general PlanProblem type.
    adjustPlanOnlyDeps :: InstallPlan -> Progress String String InstallPlan
    adjustPlanOnlyDeps =
        either (Fail . explain) Done
      . InstallPlan.remove (isTarget pkgSpecifiers)
      where
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

        targetnames  = map pkgSpecifierTarget pkgSpecifiers


    reinstall        = fromFlag (installReinstall        installFlags)
    reorderGoals     = fromFlag (installReorderGoals     installFlags)
    independentGoals = fromFlag (installIndependentGoals installFlags)
    avoidReinstalls  = fromFlag (installAvoidReinstalls  installFlags)
    shadowPkgs       = fromFlag (installShadowPkgs       installFlags)
    maxBackjumps     = fromFlag (installMaxBackjumps     installFlags)
    upgradeDeps      = fromFlag (installUpgradeDeps      installFlags)
    onlyDeps         = fromFlag (installOnlyDeps         installFlags)

-- ------------------------------------------------------------
-- * Informational messages
-- ------------------------------------------------------------

-- | Perform post-solver checks of the install plan and print it if
-- either requested or needed.
checkPrintPlan :: Verbosity
               -> PackageIndex
               -> InstallPlan
               -> InstallFlags
               -> [PackageSpecifier SourcePackage]
               -> IO ()
checkPrintPlan verbosity installed installPlan installFlags pkgSpecifiers = do

  -- User targets that are already installed.
  let preExistingTargets =
        [ p | let tgts = map pkgSpecifierTarget pkgSpecifiers,
              InstallPlan.PreExisting p <- InstallPlan.toList installPlan,
              packageName p `elem` tgts ]

  -- If there's nothing to install, we print the already existing
  -- target packages as an explanation.
  when nothingToInstall $
    notice verbosity $ unlines $
         "All the requested packages are already installed:"
       : map (display . packageId) preExistingTargets
      ++ ["Use --reinstall if you want to reinstall anyway."]

  let lPlan = linearizeInstallPlan installed installPlan
  -- Are any packages classified as reinstalls?
  let reinstalledPkgs = concatMap (extractReinstalls . snd) lPlan
  -- Packages that are already broken.
  let oldBrokenPkgs =
          map Installed.installedPackageId
        . PackageIndex.reverseDependencyClosure installed
        . map (Installed.installedPackageId . fst)
        . PackageIndex.brokenPackages
        $ installed
  let excluded = reinstalledPkgs ++ oldBrokenPkgs
  -- Packages that are reverse dependencies of replaced packages are very
  -- likely to be broken. We exclude packages that are already broken.
  let newBrokenPkgs =
        filter (\ p -> not (Installed.installedPackageId p `elem` excluded))
               (PackageIndex.reverseDependencyClosure installed reinstalledPkgs)
  let containsReinstalls = not (null reinstalledPkgs)
  let breaksPkgs         = not (null newBrokenPkgs)

  let adaptedVerbosity
        | containsReinstalls && not overrideReinstall = verbosity `max` verbose
        | otherwise                                   = verbosity

  -- We print the install plan if we are in a dry-run or if we are confronted
  -- with a dangerous install plan.
  when (dryRun || containsReinstalls && not overrideReinstall) $
    printPlan (dryRun || breaksPkgs && not overrideReinstall) adaptedVerbosity lPlan

  -- If the install plan is dangerous, we print various warning messages. In
  -- particular, if we can see that packages are likely to be broken, we even
  -- bail out (unless installation has been forced with --force-reinstalls).
  when containsReinstalls $ do
    if breaksPkgs
      then do
        (if dryRun || overrideReinstall then warn verbosity else die) $ unlines $
            "The following packages are likely to be broken by the reinstalls:"
          : map (display . Installed.sourcePackageId) newBrokenPkgs
          ++ if overrideReinstall
               then if dryRun then [] else
                 ["Continuing even though the plan contains dangerous reinstalls."]
               else
                 ["Use --force-reinstalls if you want to install anyway."]
      else unless dryRun $ warn verbosity
             "Note that reinstalls are always dangerous. Continuing anyway..."

  where
    nothingToInstall = null (InstallPlan.ready installPlan)

    dryRun            = fromFlag (installDryRun            installFlags)
    overrideReinstall = fromFlag (installOverrideReinstall installFlags)

linearizeInstallPlan :: PackageIndex
                     -> InstallPlan
                     -> [(ConfiguredPackage, PackageStatus)]
linearizeInstallPlan installedPkgIndex plan =
    unfoldr next plan
  where
    next plan' = case InstallPlan.ready plan' of
      []      -> Nothing
      (pkg:_) -> Just ((pkg, status), plan'')
        where
          pkgid  = packageId pkg
          status = packageStatus installedPkgIndex pkg
          plan'' = InstallPlan.completed pkgid
                     (BuildOk DocsNotTried TestsNotTried)
                     (InstallPlan.processing [pkg] plan')
          --FIXME: This is a bit of a hack,
          -- pretending that each package is installed

data PackageStatus = NewPackage
                   | NewVersion [Version]
                   | Reinstall  [InstalledPackageId] [PackageChange]

type PackageChange = MergeResult PackageIdentifier PackageIdentifier

isTarget :: Package pkg => [PackageSpecifier SourcePackage] -> pkg -> Bool
isTarget pkgSpecifiers pkg = packageName pkg `elem` targetnames
  where
    targetnames  = map pkgSpecifierTarget pkgSpecifiers

extractReinstalls :: PackageStatus -> [InstalledPackageId]
extractReinstalls (Reinstall ipids _) = ipids
extractReinstalls _                   = []

packageStatus :: PackageIndex -> ConfiguredPackage -> PackageStatus
packageStatus installedPkgIndex cpkg =
  case PackageIndex.lookupPackageName installedPkgIndex
                                      (packageName cpkg) of
    [] -> NewPackage
    ps ->  case filter ((==packageId cpkg) . Installed.sourcePackageId) (concatMap snd ps) of
      []           -> NewVersion (map fst ps)
      pkgs@(pkg:_) -> Reinstall (map Installed.installedPackageId pkgs)
                                (changes pkg cpkg)

  where

    changes :: Installed.InstalledPackageInfo
            -> ConfiguredPackage
            -> [MergeResult PackageIdentifier PackageIdentifier]
    changes pkg pkg' = filter changed
                     $ mergeBy (comparing packageName)
                         -- get dependencies of installed package (convert to source pkg ids via index)
                         (nub . sort . concatMap (maybeToList .
                                                  fmap Installed.sourcePackageId .
                                                  PackageIndex.lookupInstalledPackageId installedPkgIndex) .
                                                  Installed.depends $ pkg)
                         -- get dependencies of configured package
                         (nub . sort . depends $ pkg')

    changed (InBoth    pkgid pkgid') = pkgid /= pkgid'
    changed _                        = True

printPlan :: Bool -- is dry run
          -> Verbosity
          -> [(ConfiguredPackage, PackageStatus)]
          -> IO ()
printPlan dryRun verbosity plan = case plan of
  []   -> return ()
  pkgs
    | verbosity >= Verbosity.verbose -> notice verbosity $ unlines $
        ("In order, the following " ++ wouldWill ++ " be installed:")
      : map showPkgAndReason pkgs
    | otherwise -> notice verbosity $ unlines $
        ("In order, the following " ++ wouldWill ++ " be installed (use -v for more details):")
      : map (display . packageId) (map fst pkgs)
  where
    wouldWill | dryRun    = "would"
              | otherwise = "will"

    showPkgAndReason (pkg', pr) = display (packageId pkg') ++
          showFlagAssignment (nonDefaultFlags pkg') ++
          showStanzas (stanzas pkg') ++ " " ++
          case pr of
            NewPackage     -> "(new package)"
            NewVersion _   -> "(new version)"
            Reinstall _ cs -> "(reinstall)" ++ case cs of
                []   -> ""
                diff -> " changes: "  ++ intercalate ", " (map change diff)

    toFlagAssignment :: [Flag] -> FlagAssignment
    toFlagAssignment = map (\ f -> (flagName f, flagDefault f))

    nonDefaultFlags :: ConfiguredPackage -> FlagAssignment
    nonDefaultFlags (ConfiguredPackage spkg fa _ _) =
      let defaultAssignment =
            toFlagAssignment
             (genPackageFlags (Source.packageDescription spkg))
      in  fa \\ defaultAssignment

    stanzas :: ConfiguredPackage -> [OptionalStanza]
    stanzas (ConfiguredPackage _ _ sts _) = sts

    showStanzas :: [OptionalStanza] -> String
    showStanzas = concatMap ((' ' :) . showStanza)
    showStanza TestStanzas  = "*test"
    showStanza BenchStanzas = "*bench"

    -- FIXME: this should be a proper function in a proper place
    showFlagAssignment :: FlagAssignment -> String
    showFlagAssignment = concatMap ((' ' :) . showFlagValue)
    showFlagValue (f, True)   = '+' : showFlagName f
    showFlagValue (f, False)  = '-' : showFlagName f
    showFlagName (FlagName f) = f

    change (OnlyInLeft pkgid)        = display pkgid ++ " removed"
    change (InBoth     pkgid pkgid') = display pkgid ++ " -> "
                                    ++ display (packageVersion pkgid')
    change (OnlyInRight      pkgid') = display pkgid' ++ " added"

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
  (packageDBs, _, comp, conf, globalFlags, configFlags, _, installFlags, _)
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
      TestsFailed     e -> " failed during the tests phase."
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

-- | If logging is enabled, contains location of the log file and the verbosity
-- level for logging.
type UseLogFile = Maybe (PackageIdentifier -> FilePath, Verbosity)

performInstallations :: Verbosity
                     -> InstallContext
                     -> PackageIndex
                     -> InstallPlan
                     -> IO InstallPlan
performInstallations verbosity
  (packageDBs, _, comp, conf,
   globalFlags, configFlags, configExFlags, installFlags, haddockFlags)
  installedPkgIndex installPlan = do

  jobControl   <- if parallelBuild then newParallelJobControl
                                   else newSerialJobControl
  buildLimit   <- newJobLimit numJobs
  fetchLimit   <- newJobLimit (min numJobs numFetchJobs)
  installLock  <- newLock -- serialise installation
  cacheLock    <- newLock -- serialise access to setup exe cache

  executeInstallPlan verbosity jobControl useLogFile installPlan $ \cpkg ->
    installConfiguredPackage platform compid configFlags
                             cpkg $ \configFlags' src pkg ->
      fetchSourcePackage verbosity fetchLimit src $ \src' ->
        installLocalPackage verbosity buildLimit (packageId pkg) src' $ \mpath ->
          installUnpackedPackage verbosity buildLimit installLock numJobs
                                 (setupScriptOptions installedPkgIndex cacheLock)
                                 miscOptions configFlags' installFlags haddockFlags
                                 compid pkg mpath useLogFile

  where
    platform = InstallPlan.planPlatform installPlan
    compid   = InstallPlan.planCompiler installPlan

    numJobs  = fromFlag (installNumJobs installFlags)
    numFetchJobs = 2
    parallelBuild = numJobs >= 2

    setupScriptOptions index lock = SetupScriptOptions {
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
      useWorkingDir    = Nothing,
      forceExternalSetupMethod = parallelBuild,
      setupCacheLock   = Just lock
    }
    reportingLevel = fromFlag (installBuildReports installFlags)
    logsDir        = fromFlag (globalLogsDir globalFlags)

    -- Should the build output be written to a log file instead of stdout?
    useLogFile :: UseLogFile
    useLogFile = fmap ((\f -> (f, loggingVerbosity)) . substLogFileName)
                 logFileTemplate
      where
        installLogFile' = flagToMaybe $ installLogFile installFlags
        defaultTemplate = toPathTemplate $ logsDir </> "$pkgid" <.> "log"

        -- If the user has specified --remote-build-reporting=detailed, use the
        -- default log file location. If the --build-log option is set, use the
        -- provided location. Otherwise don't use logging, unless building in
        -- parallel (in which case the default location is used).
        logFileTemplate :: Maybe PathTemplate
        logFileTemplate
          | useDefaultTemplate = Just defaultTemplate
          | otherwise          = installLogFile'

        -- If the user has specified --remote-build-reporting=detailed or
        -- --build-log, use more verbose logging.
        loggingVerbosity :: Verbosity
        loggingVerbosity | overrideVerbosity = max Verbosity.verbose verbosity
                         | otherwise         = verbosity

        useDefaultTemplate :: Bool
        useDefaultTemplate
          | reportingLevel == DetailedReports = True
          | isJust installLogFile'            = False
          | numJobs > 1                       = True
          | otherwise                         = False

        overrideVerbosity :: Bool
        overrideVerbosity
          | reportingLevel == DetailedReports = True
          | isJust installLogFile'            = True
          | numJobs > 1                       = False
          | otherwise                         = False

    substLogFileName :: PathTemplate -> PackageIdentifier -> FilePath
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


executeInstallPlan :: Verbosity
                   -> JobControl IO (PackageId, BuildResult)
                   -> UseLogFile
                   -> InstallPlan
                   -> (ConfiguredPackage -> IO BuildResult)
                   -> IO InstallPlan
executeInstallPlan verbosity jobCtl useLogFile plan0 installPkg =
    tryNewTasks 0 plan0
  where
    tryNewTasks taskCount plan = do
      case InstallPlan.ready plan of
        [] | taskCount == 0 -> return plan
           | otherwise      -> waitForTasks taskCount plan
        pkgs                -> do
          sequence_
            [ do info verbosity $ "Ready to install " ++ display pkgid
                 spawnJob jobCtl $ do
                   buildResult <- installPkg pkg
                   return (packageId pkg, buildResult)
            | pkg <- pkgs
            , let pkgid = packageId pkg]

          let taskCount' = taskCount + length pkgs
              plan'      = InstallPlan.processing pkgs plan
          waitForTasks taskCount' plan'

    waitForTasks taskCount plan = do
      info verbosity $ "Waiting for install task to finish..."
      (pkgid, buildResult) <- collectJob jobCtl
      printBuildResult pkgid buildResult
      let taskCount' = taskCount-1
          plan'      = updatePlan pkgid buildResult plan
      tryNewTasks taskCount' plan'

    updatePlan :: PackageIdentifier -> BuildResult -> InstallPlan -> InstallPlan
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

    -- Print last 10 lines of the build log if something went wrong, and
    -- 'Installed $PKGID' otherwise.
    printBuildResult :: PackageId -> BuildResult -> IO ()
    printBuildResult pkgid buildResult = case buildResult of
        (Right _) -> notice verbosity $ "Installed " ++ display pkgid
        (Left _)  -> do
          notice verbosity $ "Failed to install " ++ display pkgid
          case useLogFile of
            Nothing                   -> return ()
            Just (mkLogFileName, _) -> do
              let (logName, n) = (mkLogFileName pkgid, 10)
              notice verbosity $ "Last " ++ (show n)
                ++ " lines of the build log ( " ++ logName ++ " ):"
              printLastNLines logName n

    printLastNLines :: FilePath -> Int -> IO ()
    printLastNLines path n = do
      lns <- fmap lines $ readFile path
      let len = length lns
      let toDrop = if len > n && n > 0 then (len - n) else 0
      mapM_ (notice verbosity) (drop toDrop lns)

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
  (ConfiguredPackage (SourcePackage _ gpkg source) flags stanzas deps)
  installPkg = installPkg configFlags {
    configConfigurationsFlags = flags,
    configConstraints = map thisPackageVersion deps,
    configBenchmarks = toFlag False,
    configTests = toFlag (TestStanzas `elem` stanzas)
  } source pkg
  where
    pkg = case finalizePackageDescription flags
           (const True)
           platform comp [] (enableStanzas stanzas gpkg) of
      Left _ -> error "finalizePackageDescription ConfiguredPackage failed"
      Right (desc, _) -> desc

fetchSourcePackage
  :: Verbosity
  -> JobLimit
  -> PackageLocation (Maybe FilePath)
  -> (PackageLocation FilePath -> IO BuildResult)
  -> IO BuildResult
fetchSourcePackage verbosity fetchLimit src installPkg = do
  fetched <- checkFetched src
  case fetched of
    Just src' -> installPkg src'
    Nothing   -> onFailure DownloadFailed $ do
                   loc <- withJobLimit fetchLimit $
                            fetchPackage verbosity src
                   installPkg loc


installLocalPackage
  :: Verbosity
  -> JobLimit
  -> PackageIdentifier -> PackageLocation FilePath
  -> (Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installLocalPackage verbosity jobLimit pkgid location installPkg =

  case location of

    LocalUnpackedPackage dir ->
      installPkg (Just dir)

    LocalTarballPackage tarballPath ->
      installLocalTarballPackage verbosity jobLimit
        pkgid tarballPath installPkg

    RemoteTarballPackage _ tarballPath ->
      installLocalTarballPackage verbosity jobLimit
        pkgid tarballPath installPkg

    RepoTarballPackage _ _ tarballPath ->
      installLocalTarballPackage verbosity jobLimit
        pkgid tarballPath installPkg


installLocalTarballPackage
  :: Verbosity
  -> JobLimit
  -> PackageIdentifier -> FilePath
  -> (Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installLocalTarballPackage verbosity jobLimit pkgid tarballPath installPkg = do
  tmp <- getTemporaryDirectory
  withTempDirectory verbosity tmp (display pkgid) $ \tmpDirPath ->
    onFailure UnpackFailed $ do
      let relUnpackedPath = display pkgid
          absUnpackedPath = tmpDirPath </> relUnpackedPath
          descFilePath = absUnpackedPath
                     </> display (packageName pkgid) <.> "cabal"
      withJobLimit jobLimit $ do
        info verbosity $ "Extracting " ++ tarballPath
                      ++ " to " ++ tmpDirPath ++ "..."
        extractTarGzFile tmpDirPath relUnpackedPath tarballPath
        exists <- doesFileExist descFilePath
        when (not exists) $
          die $ "Package .cabal file not found: " ++ show descFilePath
      installPkg (Just absUnpackedPath)


installUnpackedPackage
  :: Verbosity
  -> JobLimit
  -> Lock
  -> Int
  -> SetupScriptOptions
  -> InstallMisc
  -> ConfigFlags
  -> InstallFlags
  -> HaddockFlags
  -> CompilerId
  -> PackageDescription
  -> Maybe FilePath -- ^ Directory to change to before starting the installation.
  -> UseLogFile -- ^ File to log output to (if any)
  -> IO BuildResult
installUnpackedPackage verbosity buildLimit installLock numJobs
                       scriptOptions miscOptions
                       configFlags installConfigFlags haddockFlags
                       compid pkg workingDir useLogFile =

  -- Configure phase
  onFailure ConfigureFailed $ withJobLimit buildLimit $ do
    when (numJobs > 1) $ notice verbosity $
      "Configuring " ++ display pkgid ++ "..."
    setup configureCommand configureFlags

  -- Build phase
    onFailure BuildFailed $ do
      when (numJobs > 1) $ notice verbosity $
        "Building " ++ display pkgid ++ "..."
      setup buildCommand' buildFlags

  -- Doc generation phase
      docsResult <- if shouldHaddock
        then (do setup haddockCommand haddockFlags'
                 return DocsOk)
               `catchIO`   (\_ -> return DocsFailed)
               `catchExit` (\_ -> return DocsFailed)
        else return DocsNotTried

  -- Tests phase
      onFailure TestsFailed $ do
        when (testsEnabled && PackageDescription.hasTests pkg) $
            setup Cabal.testCommand testFlags

        let testsResult | testsEnabled = TestsOk
                        | otherwise = TestsNotTried

      -- Install phase
        onFailure InstallFailed $ criticalSection installLock $
          withWin32SelfUpgrade verbosity configFlags compid pkg $ do
            case rootCmd miscOptions of
              (Just cmd) -> reexec cmd
              Nothing    -> setup Cabal.installCommand installFlags
            return (Right (BuildOk docsResult testsResult))

  where
    pkgid            = packageId pkg
    configureFlags   = filterConfigureFlags configFlags {
      configVerbosity = toFlag verbosity'
    }
    buildCommand'    = buildCommand defaultProgramConfiguration
    buildFlags   _   = emptyBuildFlags {
      buildDistPref  = configDistPref configFlags,
      buildVerbosity = toFlag verbosity'
    }
    shouldHaddock    = fromFlag (installDocumentation installConfigFlags)
    haddockFlags' _   = haddockFlags {
      haddockVerbosity = toFlag verbosity'
    }
    testsEnabled = fromFlag (configTests configFlags)
    testFlags _ = Cabal.emptyTestFlags
    installFlags _   = Cabal.emptyInstallFlags {
      Cabal.installDistPref  = configDistPref configFlags,
      Cabal.installVerbosity = toFlag verbosity'
    }
    verbosity' = maybe verbosity snd useLogFile

    setup cmd flags  = do
      logFileHandle <- case useLogFile of
        Nothing                   -> return Nothing
        Just (mkLogFileName, _) -> do
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
