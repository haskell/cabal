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
    -- * High-level interface
    install,

    -- * Lower-level interface that allows to manipulate the install plan
    makeInstallContext,
    makeInstallPlan,
    processInstallPlan,
    InstallArgs,
    InstallContext,

    -- * Prune certain packages from the install plan
    pruneInstallPlan
  ) where

import Data.List
         ( unfoldr, nub, sort, (\\) )
import qualified Data.Set as S
import Data.Maybe
         ( isJust, fromMaybe, maybeToList )
import Control.Exception as Exception
         ( bracket, handleJust )
import Control.Exception as Exception
         ( Exception(toException), catches, Handler(Handler), IOException )
import System.Exit
         ( ExitCode )
import Distribution.Compat.Exception
         ( SomeException, catchIO, catchExit )
import Control.Monad
         ( when, unless )
import System.Directory
         ( getTemporaryDirectory, doesFileExist, createDirectoryIfMissing )
import System.FilePath
         ( (</>), (<.>), takeDirectory )
import System.IO
         ( openFile, IOMode(WriteMode), hClose )
import System.IO.Error
         ( isDoesNotExistError, ioeGetFileName )

import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
         ( Solver(..) )
import Distribution.Client.FetchUtils
import qualified Distribution.Client.Haddock as Haddock (regenerateHaddockIndex)
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
import Distribution.Client.Sandbox.Timestamp
         ( withUpdateTimestamps )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..), UseSandbox(..), isUseSandbox )
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
import qualified Distribution.Client.PackageIndex as SourcePackageIndex
import qualified Distribution.Client.Win32SelfUpgrade as Win32SelfUpgrade
import qualified Distribution.Client.World as World
import qualified Distribution.InstalledPackageInfo as Installed
import Paths_cabal_install (getBinDir)
import Distribution.Client.JobControl

import Distribution.Simple.Compiler
         ( CompilerId(..), Compiler(compilerId), compilerFlavor
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramConfiguration,
                                    defaultProgramConfiguration)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Setup
         ( haddockCommand, HaddockFlags(..)
         , buildCommand, BuildFlags(..), emptyBuildFlags
         , toFlag, fromFlag, fromFlagOrDefault, flagToMaybe )
import qualified Distribution.Simple.Setup as Cabal
         ( installCommand, InstallFlags(..), TestFlags(..), emptyInstallFlags
         , emptyTestFlags, testCommand, Flag(..) )
import Distribution.Simple.Utils
         ( rawSystemExit, comparing, writeFileAtomic )
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
         ( notice, info, warn, debugNoWrap, die, intercalate, withTempDirectory )
import Distribution.Client.Utils
         ( numberOfProcessors, inDir, mergeBy, MergeResult(..)
         , tryCanonicalizePath )
import Distribution.System
         ( Platform, OS(Windows), buildOS )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, showForCabal, normal, verbose )
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
install
  :: Verbosity
  -> PackageDBStack
  -> [Repo]
  -> Compiler
  -> Platform
  -> ProgramConfiguration
  -> UseSandbox
  -> Maybe SandboxPackageInfo
  -> GlobalFlags
  -> ConfigFlags
  -> ConfigExFlags
  -> InstallFlags
  -> HaddockFlags
  -> [UserTarget]
  -> IO ()
install verbosity packageDBs repos comp platform conf useSandbox mSandboxPkgInfo
  globalFlags configFlags configExFlags installFlags haddockFlags
  userTargets0 = do

    installContext <- makeInstallContext verbosity args (Just userTargets0)
    installPlan    <- foldProgress logMsg die return =<<
                      makeInstallPlan verbosity args installContext

    processInstallPlan verbosity args installContext installPlan
  where
    args :: InstallArgs
    args = (packageDBs, repos, comp, platform, conf, useSandbox, mSandboxPkgInfo,
            globalFlags, configFlags, configExFlags, installFlags,
            haddockFlags)

    logMsg message rest = debugNoWrap verbosity message >> rest

-- TODO: Make InstallContext a proper datatype with documented fields.
-- | Common context for makeInstallPlan and processInstallPlan.
type InstallContext = ( PackageIndex, SourcePackageDb
                      , [UserTarget], [PackageSpecifier SourcePackage] )

-- TODO: Make InstallArgs a proper datatype with documented fields or just get
-- rid of it completely.
-- | Initial arguments given to 'install' or 'makeInstallContext'.
type InstallArgs = ( PackageDBStack
                   , [Repo]
                   , Compiler
                   , Platform
                   , ProgramConfiguration
                   , UseSandbox
                   , Maybe SandboxPackageInfo
                   , GlobalFlags
                   , ConfigFlags
                   , ConfigExFlags
                   , InstallFlags
                   , HaddockFlags )

-- | Make an install context given install arguments.
makeInstallContext :: Verbosity -> InstallArgs -> Maybe [UserTarget]
                      -> IO InstallContext
makeInstallContext verbosity
  (packageDBs, repos, comp, _, conf,_,_,
   globalFlags, _, _, _, _) mUserTargets = do

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs conf
    sourcePkgDb       <- getSourcePackages    verbosity repos

    (userTargets, pkgSpecifiers) <- case mUserTargets of
      Nothing           ->
        -- We want to distinguish between the case where the user has given an
        -- empty list of targets on the command-line and the case where we
        -- specifically want to have an empty list of targets.
        return ([], [])
      Just userTargets0 -> do
        -- For install, if no target is given it means we use the current
        -- directory as the single target.
        let userTargets | null userTargets0 = [UserTargetLocalDir "."]
                        | otherwise         = userTargets0

        pkgSpecifiers <- resolveUserTargets verbosity
                         (fromFlag $ globalWorldFile globalFlags)
                         (packageIndex sourcePkgDb)
                         userTargets
        return (userTargets, pkgSpecifiers)

    return (installedPkgIndex, sourcePkgDb, userTargets, pkgSpecifiers)

-- | Make an install plan given install context and install arguments.
makeInstallPlan :: Verbosity -> InstallArgs -> InstallContext
                -> IO (Progress String String InstallPlan)
makeInstallPlan verbosity
  (_, _, comp, platform, _, _, mSandboxPkgInfo,
   _, configFlags, configExFlags, installFlags,
   _)
  (installedPkgIndex, sourcePkgDb,
   _, pkgSpecifiers) = do

    solver <- chooseSolver verbosity (fromFlag (configSolver configExFlags))
              (compilerId comp)
    notice verbosity "Resolving dependencies..."
    return $ planPackages comp platform mSandboxPkgInfo solver
      configFlags configExFlags installFlags
      installedPkgIndex sourcePkgDb pkgSpecifiers

-- | Given an install plan, perform the actual installations.
processInstallPlan :: Verbosity -> InstallArgs -> InstallContext
                   -> InstallPlan
                   -> IO ()
processInstallPlan verbosity
  args@(_,_, _, _, _, _, _, _, _, _, installFlags, _)
  (installedPkgIndex, sourcePkgDb,
   userTargets, pkgSpecifiers) installPlan = do
    checkPrintPlan verbosity installedPkgIndex installPlan sourcePkgDb
      installFlags pkgSpecifiers

    unless dryRun $ do
      installPlan' <- performInstallations verbosity
                      args installedPkgIndex installPlan
      postInstallActions verbosity args userTargets installPlan'
  where
    dryRun = fromFlag (installDryRun installFlags)

-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

planPackages :: Compiler
             -> Platform
             -> Maybe SandboxPackageInfo
             -> Solver
             -> ConfigFlags
             -> ConfigExFlags
             -> InstallFlags
             -> PackageIndex
             -> SourcePackageDb
             -> [PackageSpecifier SourcePackage]
             -> Progress String String InstallPlan
planPackages comp platform mSandboxPkgInfo solver
             configFlags configExFlags installFlags
             installedPkgIndex sourcePkgDb pkgSpecifiers =

        resolveDependencies
          platform (compilerId comp)
          solver
          resolverParams

    >>= if onlyDeps then pruneInstallPlan pkgSpecifiers else return

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

      . maybe id applySandboxInstallPolicy mSandboxPkgInfo

      . (if reinstall then reinstallTargets else id)

      $ standardInstallPolicy
        installedPkgIndex sourcePkgDb pkgSpecifiers

    stanzas = concat
        [ if testsEnabled then [TestStanzas] else []
        , if benchmarksEnabled then [BenchStanzas] else []
        ]
    testsEnabled = fromFlagOrDefault False $ configTests configFlags
    benchmarksEnabled = fromFlagOrDefault False $ configBenchmarks configFlags

    reinstall        = fromFlag (installReinstall        installFlags)
    reorderGoals     = fromFlag (installReorderGoals     installFlags)
    independentGoals = fromFlag (installIndependentGoals installFlags)
    avoidReinstalls  = fromFlag (installAvoidReinstalls  installFlags)
    shadowPkgs       = fromFlag (installShadowPkgs       installFlags)
    maxBackjumps     = fromFlag (installMaxBackjumps     installFlags)
    upgradeDeps      = fromFlag (installUpgradeDeps      installFlags)
    onlyDeps         = fromFlag (installOnlyDeps         installFlags)

-- | Remove the provided targets from the install plan.
pruneInstallPlan :: Package pkg => [PackageSpecifier pkg] -> InstallPlan
                    -> Progress String String InstallPlan
pruneInstallPlan pkgSpecifiers =
  -- TODO: this is a general feature and should be moved to D.C.Dependency
  -- Also, the InstallPlan.remove should return info more precise to the
  -- problem, rather than the very general PlanProblem type.
  either (Fail . explain) Done
  . InstallPlan.remove (\pkg -> packageName pkg `elem` targetnames)
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

-- ------------------------------------------------------------
-- * Informational messages
-- ------------------------------------------------------------

-- | Perform post-solver checks of the install plan and print it if
-- either requested or needed.
checkPrintPlan :: Verbosity
               -> PackageIndex
               -> InstallPlan
               -> SourcePackageDb
               -> InstallFlags
               -> [PackageSpecifier SourcePackage]
               -> IO ()
checkPrintPlan verbosity installed installPlan sourcePkgDb
  installFlags pkgSpecifiers = do

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
    printPlan (dryRun || breaksPkgs && not overrideReinstall)
      adaptedVerbosity lPlan sourcePkgDb

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

extractReinstalls :: PackageStatus -> [InstalledPackageId]
extractReinstalls (Reinstall ipids _) = ipids
extractReinstalls _                   = []

packageStatus :: PackageIndex -> ConfiguredPackage -> PackageStatus
packageStatus installedPkgIndex cpkg =
  case PackageIndex.lookupPackageName installedPkgIndex
                                      (packageName cpkg) of
    [] -> NewPackage
    ps ->  case filter ((==packageId cpkg)
                        . Installed.sourcePackageId) (concatMap snd ps) of
      []           -> NewVersion (map fst ps)
      pkgs@(pkg:_) -> Reinstall (map Installed.installedPackageId pkgs)
                                (changes pkg cpkg)

  where

    changes :: Installed.InstalledPackageInfo
            -> ConfiguredPackage
            -> [MergeResult PackageIdentifier PackageIdentifier]
    changes pkg pkg' =
      filter changed
      $ mergeBy (comparing packageName)
        -- get dependencies of installed package (convert to source pkg ids via
        -- index)
        (nub . sort . concatMap
         (maybeToList . fmap Installed.sourcePackageId .
          PackageIndex.lookupInstalledPackageId installedPkgIndex) .
         Installed.depends $ pkg)
        -- get dependencies of configured package
        (nub . sort . depends $ pkg')

    changed (InBoth    pkgid pkgid') = pkgid /= pkgid'
    changed _                        = True

printPlan :: Bool -- is dry run
          -> Verbosity
          -> [(ConfiguredPackage, PackageStatus)]
          -> SourcePackageDb
          -> IO ()
printPlan dryRun verbosity plan sourcePkgDb = case plan of
  []   -> return ()
  pkgs
    | verbosity >= Verbosity.verbose -> notice verbosity $ unlines $
        ("In order, the following " ++ wouldWill ++ " be installed:")
      : map showPkgAndReason pkgs
    | otherwise -> notice verbosity $ unlines $
        ("In order, the following " ++ wouldWill
         ++ " be installed (use -v for more details):")
      : map showPkg pkgs
  where
    wouldWill | dryRun    = "would"
              | otherwise = "will"

    showPkg (pkg, _) = display (packageId pkg) ++
                       showLatest (pkg)

    showPkgAndReason (pkg', pr) = display (packageId pkg') ++
          showLatest pkg' ++
          showFlagAssignment (nonDefaultFlags pkg') ++
          showStanzas (stanzas pkg') ++ " " ++
          case pr of
            NewPackage     -> "(new package)"
            NewVersion _   -> "(new version)"
            Reinstall _ cs -> "(reinstall)" ++ case cs of
                []   -> ""
                diff -> " changes: "  ++ intercalate ", " (map change diff)

    showLatest :: ConfiguredPackage -> String
    showLatest pkg = case mLatestVersion of
        Just latestVersion ->
            if pkgVersion /= latestVersion
            then (" (latest: " ++ display latestVersion ++ ")")
            else ""
        Nothing -> ""
      where
        pkgVersion    = packageVersion pkg
        mLatestVersion :: Maybe Version
        mLatestVersion = case SourcePackageIndex.lookupPackageName
                                (packageIndex sourcePkgDb)
                                (packageName pkg) of
            [] -> Nothing
            x -> Just $ packageVersion $ last x

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
                   -> InstallArgs
                   -> [UserTarget]
                   -> InstallPlan
                   -> IO ()
postInstallActions verbosity
  (packageDBs, _, comp, platform, conf, useSandbox, mSandboxPkgInfo
  ,globalFlags, configFlags, _, installFlags, _)
  targets installPlan = do

  unless oneShot $
    World.insert verbosity worldFile
      --FIXME: does not handle flags
      [ World.WorldPkgInfo dep []
      | UserTargetNamed dep <- targets ]

  let buildReports = BuildReports.fromInstallPlan installPlan
  BuildReports.storeLocal (installSummaryFile installFlags) buildReports
    (InstallPlan.planPlatform installPlan)
  when (reportingLevel >= AnonymousReports) $
    BuildReports.storeAnonymous buildReports
  when (reportingLevel == DetailedReports) $
    storeDetailedBuildReports verbosity logsDir buildReports

  regenerateHaddockIndex verbosity packageDBs comp platform conf
                         configFlags installFlags installPlan

  symlinkBinaries verbosity configFlags installFlags installPlan

  printBuildFailures installPlan

  updateSandboxTimestampsFile useSandbox mSandboxPkgInfo
                              comp platform installPlan

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

    missingFile ioe
      | isDoesNotExistError ioe  = Just ioe
    missingFile _                = Nothing


regenerateHaddockIndex :: Verbosity
                       -> [PackageDB]
                       -> Compiler
                       -> Platform
                       -> ProgramConfiguration
                       -> ConfigFlags
                       -> InstallFlags
                       -> InstallPlan
                       -> IO ()
regenerateHaddockIndex verbosity packageDBs comp platform conf
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
            ++ InstallDirs.platformTemplateEnv platform
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

-- | If we're working inside a sandbox and some add-source deps were installed,
-- update the timestamps of those deps.
updateSandboxTimestampsFile :: UseSandbox -> Maybe SandboxPackageInfo
                        -> Compiler -> Platform -> InstallPlan
                        -> IO ()
updateSandboxTimestampsFile (UseSandbox sandboxDir)
                            (Just (SandboxPackageInfo _ _ _ allAddSourceDeps))
                            comp platform installPlan =
  withUpdateTimestamps sandboxDir (compilerId comp) platform $ \_ -> do
    let allInstalled = [ pkg | InstallPlan.Installed pkg _
                            <- InstallPlan.toList installPlan ]
        allSrcPkgs   = [ pkg | ConfiguredPackage pkg _ _ _ <- allInstalled ]
        allPaths     = [ pth | LocalUnpackedPackage pth
                            <- map packageSource allSrcPkgs]
    allPathsCanonical <- mapM tryCanonicalizePath allPaths
    return $! filter (`S.member` allAddSourceDeps) allPathsCanonical

updateSandboxTimestampsFile _ _ _ _ _ = return ()

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
                     -> InstallArgs
                     -> PackageIndex
                     -> InstallPlan
                     -> IO InstallPlan
performInstallations verbosity
  (packageDBs, _, comp, _, conf, useSandbox, _,
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
                             cpkg $ \configFlags' src pkg pkgoverride ->
      fetchSourcePackage verbosity fetchLimit src $ \src' ->
        installLocalPackage verbosity buildLimit (packageId pkg) src' $ \mpath ->
          installUnpackedPackage verbosity buildLimit installLock numJobs
                                 (setupScriptOptions installedPkgIndex cacheLock)
                                 miscOptions configFlags' installFlags haddockFlags
                                 compid platform pkg pkgoverride mpath useLogFile

  where
    platform = InstallPlan.planPlatform installPlan
    compid   = InstallPlan.planCompiler installPlan

    numJobs  = case installNumJobs installFlags of
      Cabal.NoFlag        -> 1
      Cabal.Flag Nothing  -> numberOfProcessors
      Cabal.Flag (Just n) -> n
    numFetchJobs  = 2
    parallelBuild = numJobs >= 2

    setupScriptOptions index lock = SetupScriptOptions {
      useCabalVersion  = maybe anyVersion thisVersion (libVersion miscOptions),
      useCompiler      = Just comp,
      usePlatform      = Just platform,
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
          | parallelBuild                     = True
          | otherwise                         = False

        overrideVerbosity :: Bool
        overrideVerbosity
          | reportingLevel == DetailedReports = True
          | isJust installLogFile'            = True
          | parallelBuild                     = False
          | otherwise                         = False

    substLogFileName :: PathTemplate -> PackageIdentifier -> FilePath
    substLogFileName template pkg = fromPathTemplate
                                  . substPathTemplate env
                                  $ template
      where env = initialPathTemplateEnv (packageId pkg)
                  (compilerId comp) platform

    miscOptions  = InstallMisc {
      rootCmd    = if fromFlag (configUserInstall configFlags)
                      || (isUseSandbox useSandbox)
                     then Nothing      -- ignore --root-cmd if --user
                                       -- or working inside a sandbox.
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
          when (verbosity >= normal) $
            case useLogFile of
              Nothing                 -> return ()
              Just (mkLogFileName, _) -> do
                let logName = mkLogFileName pkgid
                    n       = 10
                putStr $ "Last " ++ (show n)
                  ++ " lines of the build log ( " ++ logName ++ " ):\n"
                printLastNLines logName n

    printLastNLines :: FilePath -> Int -> IO ()
    printLastNLines path n = do
      lns <- fmap lines $ readFile path
      let len = length lns
      let toDrop = if (len > n && n > 0) then (len - n) else 0
      mapM_ putStrLn (drop toDrop lns)

-- | Call an installer for an 'SourcePackage' but override the configure
-- flags with the ones given by the 'ConfiguredPackage'. In particular the
-- 'ConfiguredPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
installConfiguredPackage :: Platform -> CompilerId
                         ->  ConfigFlags -> ConfiguredPackage
                         -> (ConfigFlags -> PackageLocation (Maybe FilePath)
                                         -> PackageDescription
                                         -> PackageDescriptionOverride -> a)
                         -> a
installConfiguredPackage platform comp configFlags
  (ConfiguredPackage (SourcePackage _ gpkg source pkgoverride)
   flags stanzas deps)
  installPkg = installPkg configFlags {
    configConfigurationsFlags = flags,
    configConstraints = map thisPackageVersion deps,
    configBenchmarks = toFlag False,
    configTests = toFlag (TestStanzas `elem` stanzas)
  } source pkg pkgoverride
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
  withTempDirectory verbosity False tmp (display pkgid) $ \tmpDirPath ->
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
  -> Platform
  -> PackageDescription
  -> PackageDescriptionOverride
  -> Maybe FilePath -- ^ Directory to change to before starting the installation.
  -> UseLogFile -- ^ File to log output to (if any)
  -> IO BuildResult
installUnpackedPackage verbosity buildLimit installLock numJobs
                       scriptOptions miscOptions
                       configFlags installConfigFlags haddockFlags
                       compid platform pkg pkgoverride workingDir useLogFile = do

  -- Override the .cabal file if necessary
  case pkgoverride of
    Nothing     -> return ()
    Just pkgtxt -> do
      let descFilePath = fromMaybe "." workingDir
                     </> display (packageName pkgid) <.> "cabal"
      info verbosity $
        "Updating " ++ display (packageName pkgid) <.> "cabal"
                    ++ " with the latest revision from the index."
      writeFileAtomic descFilePath pkgtxt

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
          withWin32SelfUpgrade verbosity configFlags compid platform pkg $ do
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
      haddockVerbosity = toFlag verbosity',
      haddockDistPref  = configDistPref configFlags
    }
    testsEnabled = fromFlag (configTests configFlags)
    testFlags _ = Cabal.emptyTestFlags {
      Cabal.testDistPref = configDistPref configFlags
    }
    installFlags _   = Cabal.emptyInstallFlags {
      Cabal.installDistPref  = configDistPref configFlags,
      Cabal.installVerbosity = toFlag verbosity'
    }
    verbosity' = maybe verbosity snd useLogFile

    setup cmd flags  = do
      Exception.bracket
              (case useLogFile of
               Nothing                   -> return Nothing
               Just (mkLogFileName, _) -> do
                 let logFileName = mkLogFileName (packageId pkg)
                     logDir      = takeDirectory logFileName
                 unless (null logDir) $ createDirectoryIfMissing True logDir
                 logFile <- openFile logFileName WriteMode
                 return (Just logFile))
              (\mHandle -> case mHandle of
                           Just handle -> hClose handle
                           Nothing -> return ())
              (\logFileHandle ->
               setupWrapper verbosity
                 scriptOptions { useLoggingHandle = logFileHandle
                               , useWorkingDir    = workingDir }
                 (Just pkg)
                 cmd flags [])
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
  action `catches`
    [ Handler $ \ioe  -> handler (ioe  :: IOException)
    , Handler $ \exit -> handler (exit :: ExitCode)
    ]
  where
    handler :: Exception e => e -> IO BuildResult
    handler = return . Left . result . toException


-- ------------------------------------------------------------
-- * Wierd windows hacks
-- ------------------------------------------------------------

withWin32SelfUpgrade :: Verbosity
                     -> ConfigFlags
                     -> CompilerId
                     -> Platform
                     -> PackageDescription
                     -> IO a -> IO a
withWin32SelfUpgrade _ _ _ _ _ action | buildOS /= Windows = action
withWin32SelfUpgrade verbosity configFlags compid platform pkg action = do

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
                           pkgid compid InstallDirs.NoCopyDest
                           platform templateDirs
        substTemplate  = InstallDirs.fromPathTemplate
                       . InstallDirs.substPathTemplate env
          where env = InstallDirs.initialPathTemplateEnv pkgid compid platform
