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

import Data.Foldable
         ( traverse_ )
import Data.List
         ( isPrefixOf, unfoldr, nub, sort, (\\) )
import qualified Data.Set as S
import Data.Maybe
         ( catMaybes, isJust, isNothing, fromMaybe, mapMaybe )
import Control.Exception as Exception
         ( Exception(toException), bracket, catches
         , Handler(Handler), handleJust, IOException, SomeException )
#ifndef mingw32_HOST_OS
import Control.Exception as Exception
         ( Exception(fromException) )
#endif
import System.Exit
         ( ExitCode(..) )
import Distribution.Compat.Exception
         ( catchIO, catchExit )
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
         ( (<$>) )
import Data.Traversable
         ( traverse )
#endif
import Control.Monad
         ( filterM, forM_, when, unless )
import System.Directory
         ( getTemporaryDirectory, doesDirectoryExist, doesFileExist,
           createDirectoryIfMissing, removeFile, renameDirectory )
import System.FilePath
         ( (</>), (<.>), equalFilePath, takeDirectory )
import System.IO
         ( openFile, IOMode(AppendMode), hClose )
import System.IO.Error
         ( isDoesNotExistError, ioeGetFileName )

import Distribution.Client.Targets
import Distribution.Client.Configure
         ( chooseCabalVersion, configureSetupScript )
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
         ( Solver(..) )
import Distribution.Client.FetchUtils
import Distribution.Client.HttpUtils
         ( configureTransport, HttpTransport (..) )
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
         ( defaultCabalDir, defaultUserInstall )
import Distribution.Client.Sandbox.Timestamp
         ( withUpdateTimestamps )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..), UseSandbox(..), isUseSandbox
         , whenUsingSandbox )
import Distribution.Client.Tar (extractTarGzFile)
import Distribution.Client.Types as Source
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.PackageIndex
         ( PackageFixedDeps(..) )
import qualified Distribution.Client.BuildReports.Anonymous as BuildReports
import qualified Distribution.Client.BuildReports.Storage as BuildReports
         ( storeAnonymous, storeLocal, fromInstallPlan, fromPlanningFailure )
import qualified Distribution.Client.InstallSymlink as InstallSymlink
         ( symlinkBinaries )
import qualified Distribution.Client.PackageIndex as SourcePackageIndex
import qualified Distribution.Client.Win32SelfUpgrade as Win32SelfUpgrade
import qualified Distribution.Client.World as World
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Client.Compat.ExecutablePath
import Distribution.Client.JobControl
import qualified Distribution.Client.ComponentDeps as CD

import Distribution.Utils.NubList
import Distribution.Simple.Compiler
         ( CompilerId(..), Compiler(compilerId), compilerFlavor
         , CompilerInfo(..), compilerInfo, PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramConfiguration,
                                    defaultProgramConfiguration)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Setup
         ( haddockCommand, HaddockFlags(..)
         , buildCommand, BuildFlags(..), emptyBuildFlags
         , toFlag, fromFlag, fromFlagOrDefault, flagToMaybe, defaultDistPref )
import qualified Distribution.Simple.Setup as Cabal
         ( Flag(..)
         , copyCommand, CopyFlags(..), emptyCopyFlags
         , registerCommand, RegisterFlags(..), emptyRegisterFlags
         , testCommand, TestFlags(..), emptyTestFlags )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, rawSystemExit, comparing
         , writeFileAtomic, withTempFile , withUTF8FileContents )
import Distribution.Simple.InstallDirs as InstallDirs
         ( PathTemplate, fromPathTemplate, toPathTemplate, substPathTemplate
         , initialPathTemplateEnv, installDirsTemplateEnv )
import Distribution.Package
         ( PackageIdentifier(..), PackageId, packageName, packageVersion
         , Package(..), LibraryName
         , Dependency(..), thisPackageVersion, InstalledPackageId, installedPackageId )
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription
         ( PackageDescription, GenericPackageDescription(..), Flag(..)
         , FlagName(..), FlagAssignment )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.ParseUtils
         ( showPWarning )
import Distribution.Version
         ( Version, VersionRange, foldVersionRange )
import Distribution.Simple.Utils as Utils
         ( notice, info, warn, debug, debugNoWrap, die
         , intercalate, withTempDirectory )
import Distribution.Client.Utils
         ( determineNumJobs, inDir, mergeBy, MergeResult(..)
         , tryCanonicalizePath )
import Distribution.System
         ( Platform, OS(Windows), buildOS )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, showForCabal, normal, verbose )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.GHC.ImplInfo ( ghcVersionImplInfo, supportsMultInst )
import Distribution.Simple.Compiler ( compilerVersion )

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
    planResult     <- foldProgress logMsg (return . Left) (return . Right) =<<
                      makeInstallPlan verbosity args installContext

    case planResult of
        Left message -> do
            reportPlanningFailure verbosity args installContext message
            die' message
        Right installPlan ->
            processInstallPlan verbosity args installContext installPlan
  where
    args :: InstallArgs
    args = (packageDBs, repos, comp, platform, conf, useSandbox, mSandboxPkgInfo,
            globalFlags, configFlags, configExFlags, installFlags,
            haddockFlags)

    die' message = die (message ++ if isUseSandbox useSandbox
                                   then installFailedInSandbox else [])
    -- TODO: use a better error message, remove duplication.
    installFailedInSandbox =
      "\nNote: when using a sandbox, all packages are required to have "
      ++ "consistent dependencies. "
      ++ "Try reinstalling/unregistering the offending packages or "
      ++ "recreating the sandbox."
    logMsg message rest = debugNoWrap verbosity message >> rest

-- TODO: Make InstallContext a proper data type with documented fields.
-- | Common context for makeInstallPlan and processInstallPlan.
type InstallContext = ( InstalledPackageIndex, SourcePackageDb
                      , [UserTarget], [PackageSpecifier SourcePackage], HttpTransport )

-- TODO: Make InstallArgs a proper data type with documented fields or just get
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
    transport <- configureTransport verbosity (flagToMaybe (globalHttpTransport globalFlags))

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

        pkgSpecifiers <- resolveUserTargets transport verbosity
                         (fromFlag $ globalWorldFile globalFlags)
                         (packageIndex sourcePkgDb)
                         userTargets
        return (userTargets, pkgSpecifiers)

    return (installedPkgIndex, sourcePkgDb, userTargets, pkgSpecifiers, transport)

-- | Make an install plan given install context and install arguments.
makeInstallPlan :: Verbosity -> InstallArgs -> InstallContext
                -> IO (Progress String String InstallPlan)
makeInstallPlan verbosity
  (_, _, comp, platform, _, _, mSandboxPkgInfo,
   _, configFlags, configExFlags, installFlags,
   _)
  (installedPkgIndex, sourcePkgDb,
   _, pkgSpecifiers, _) = do

    solver <- chooseSolver verbosity (fromFlag (configSolver configExFlags))
              (compilerInfo comp)
    notice verbosity "Resolving dependencies..."
    return $ planPackages comp platform mSandboxPkgInfo solver
      configFlags configExFlags installFlags
      installedPkgIndex sourcePkgDb pkgSpecifiers

-- | Given an install plan, perform the actual installations.
processInstallPlan :: Verbosity -> InstallArgs -> InstallContext
                   -> InstallPlan
                   -> IO ()
processInstallPlan verbosity
  args@(_,_, comp, _, _, _, _, _, _, _, installFlags, _)
  (installedPkgIndex, sourcePkgDb,
   userTargets, pkgSpecifiers, _) installPlan = do
    checkPrintPlan verbosity comp installedPkgIndex installPlan sourcePkgDb
      installFlags pkgSpecifiers

    unless (dryRun || nothingToInstall) $ do
      installPlan' <- performInstallations verbosity
                      args installedPkgIndex installPlan
      postInstallActions verbosity args userTargets installPlan'
  where
    dryRun = fromFlag (installDryRun installFlags)
    nothingToInstall = null (InstallPlan.ready installPlan)

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
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> [PackageSpecifier SourcePackage]
             -> Progress String String InstallPlan
planPackages comp platform mSandboxPkgInfo solver
             configFlags configExFlags installFlags
             installedPkgIndex sourcePkgDb pkgSpecifiers =

        resolveDependencies
          platform (compilerInfo comp)
          solver
          resolverParams

    >>= if onlyDeps then pruneInstallPlan pkgSpecifiers else return

  where
    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . (if (supportsMultInst . ghcVersionImplInfo . compilerVersion $ comp)
            then id
            else setAvoidReinstalls avoidReinstalls)

      . setShadowPkgs shadowPkgs

      . setStrongFlags strongFlags

      . setPreferenceDefault (if upgradeDeps then PreferAllLatest
                                             else PreferLatestForSelected)

      . removeUpperBounds allowNewer

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
    strongFlags      = fromFlag (installStrongFlags      installFlags)
    maxBackjumps     = fromFlag (installMaxBackjumps     installFlags)
    upgradeDeps      = fromFlag (installUpgradeDeps      installFlags)
    onlyDeps         = fromFlag (installOnlyDeps         installFlags)
    allowNewer       = fromFlag (configAllowNewer        configExFlags)

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
               -> Compiler
               -> InstalledPackageIndex
               -> InstallPlan
               -> SourcePackageDb
               -> InstallFlags
               -> [PackageSpecifier SourcePackage]
               -> IO ()
checkPrintPlan verbosity comp installed installPlan sourcePkgDb
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

  let lPlan = linearizeInstallPlan comp installed installPlan
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
  -- As with supportsMultInst packages cannot break when installing new packages
  -- we are not printing anything or exiting the program
  when (containsReinstalls && not (supportsMultInst . ghcVersionImplInfo . compilerVersion $ comp)) $ do
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

  -- If we are explicitly told to not download anything, check that all packages
  -- are already fetched.
  let offline = fromFlagOrDefault False (installOfflineMode installFlags)
  when offline $ do
    let pkgs = [ sourcePkg
               | InstallPlan.Configured (ConfiguredPackage sourcePkg _ _ _)
                 <- InstallPlan.toList installPlan ]
    notFetched <- fmap (map packageInfoId)
                  . filterM (fmap isNothing . checkFetched . packageSource)
                  $ pkgs
    unless (null notFetched) $
      die $ "Can't download packages in offline mode. "
      ++ "Must download the following packages to proceed:\n"
      ++ intercalate ", " (map display notFetched)
      ++ "\nTry using 'cabal fetch'."

  where
    nothingToInstall = null (InstallPlan.ready installPlan)

    dryRun            = fromFlag (installDryRun            installFlags)
    overrideReinstall = fromFlag (installOverrideReinstall installFlags)

linearizeInstallPlan :: Compiler
                     -> InstalledPackageIndex
                     -> InstallPlan
                     -> [(ReadyPackage, PackageStatus)]
linearizeInstallPlan comp installedPkgIndex plan =
    unfoldr next plan
  where
    next plan' = case InstallPlan.ready plan' of
      []      -> Nothing
      (pkg:_) -> Just ((pkg, status), plan'')
        where
          pkgid  = installedPackageId pkg
          status = packageStatus comp installedPkgIndex pkg
          plan'' = InstallPlan.completed pkgid
                     (BuildOk DocsNotTried TestsNotTried
                              (Just $ Installed.emptyInstalledPackageInfo
                              { Installed.sourcePackageId = packageId pkg
                              , Installed.installedPackageId = pkgid }))
                     (InstallPlan.processing [pkg] plan')
          --FIXME: This is a bit of a hack,
          -- pretending that each package is installed
          -- It's doubly a hack because the installed package ID
          -- didn't get updated...

data PackageStatus = NewPackage
                   | NewVersion [Version]
                   | Reinstall  [InstalledPackageId] [PackageChange]

type PackageChange = MergeResult PackageIdentifier PackageIdentifier

extractReinstalls :: PackageStatus -> [InstalledPackageId]
extractReinstalls (Reinstall ipids _) = ipids
extractReinstalls _                   = []

packageStatus :: Compiler -> InstalledPackageIndex -> ReadyPackage -> PackageStatus
packageStatus _comp installedPkgIndex cpkg =
  case PackageIndex.lookupPackageName installedPkgIndex
                                      (packageName cpkg) of
    [] -> NewPackage
    ps ->  case filter ((== packageId cpkg)
                        . Installed.sourcePackageId) (concatMap snd ps) of
      []           -> NewVersion (map fst ps)
      pkgs@(pkg:_) -> Reinstall (map Installed.installedPackageId pkgs)
                                (changes pkg cpkg)

  where

    changes :: Installed.InstalledPackageInfo
            -> ReadyPackage
            -> [MergeResult PackageIdentifier PackageIdentifier]
    changes pkg pkg' = filter changed $
      mergeBy (comparing packageName)
        (resolveInstalledIds $ Installed.depends pkg)          -- deps of installed pkg
        (resolveInstalledIds $ CD.nonSetupDeps (depends pkg')) -- deps of configured pkg

    -- convert to source pkg ids via index
    resolveInstalledIds :: [InstalledPackageId] -> [PackageIdentifier]
    resolveInstalledIds =
        nub
      . sort
      . map Installed.sourcePackageId
      . catMaybes
      . map (PackageIndex.lookupInstalledPackageId installedPkgIndex)

    changed (InBoth    pkgid pkgid') = pkgid /= pkgid'
    changed _                        = True

printPlan :: Bool -- is dry run
          -> Verbosity
          -> [(ReadyPackage, PackageStatus)]
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

    showLatest :: ReadyPackage -> String
    showLatest pkg = case mLatestVersion of
        Just latestVersion ->
            if packageVersion pkg < latestVersion
            then (" (latest: " ++ display latestVersion ++ ")")
            else ""
        Nothing -> ""
      where
        mLatestVersion :: Maybe Version
        mLatestVersion = case SourcePackageIndex.lookupPackageName
                                (packageIndex sourcePkgDb)
                                (packageName pkg) of
            [] -> Nothing
            x -> Just $ packageVersion $ last x

    toFlagAssignment :: [Flag] -> FlagAssignment
    toFlagAssignment = map (\ f -> (flagName f, flagDefault f))

    nonDefaultFlags :: ReadyPackage -> FlagAssignment
    nonDefaultFlags (ReadyPackage spkg fa _ _) =
      let defaultAssignment =
            toFlagAssignment
             (genPackageFlags (Source.packageDescription spkg))
      in  fa \\ defaultAssignment

    stanzas :: ReadyPackage -> [OptionalStanza]
    stanzas (ReadyPackage _ _ sts _) = sts

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

-- | Report a solver failure. This works slightly differently to
-- 'postInstallActions', as (by definition) we don't have an install plan.
reportPlanningFailure :: Verbosity -> InstallArgs -> InstallContext -> String -> IO ()
reportPlanningFailure verbosity
  (_, _, comp, platform, _, _, _
  ,_, configFlags, _, installFlags, _)
  (_, sourcePkgDb, _, pkgSpecifiers, _)
  message = do

  when reportFailure $ do

    -- Only create reports for explicitly named packages
    let pkgids =
          filter (SourcePackageIndex.elemByPackageId (packageIndex sourcePkgDb)) $
          mapMaybe theSpecifiedPackage pkgSpecifiers

        buildReports = BuildReports.fromPlanningFailure platform (compilerId comp)
          pkgids (configConfigurationsFlags configFlags)

    when (not (null buildReports)) $
      info verbosity $
        "Solver failure will be reported for "
        ++ intercalate "," (map display pkgids)

    -- Save reports
    BuildReports.storeLocal (compilerInfo comp)
                            (fromNubList $ installSummaryFile installFlags) buildReports platform

    -- Save solver log
    case logFile of
      Nothing -> return ()
      Just template -> forM_ pkgids $ \pkgid ->
        let env = initialPathTemplateEnv pkgid dummyLibraryName
                    (compilerInfo comp) platform
            path = fromPathTemplate $ substPathTemplate env template
        in  writeFile path message

  where
    reportFailure = fromFlag (installReportPlanningFailure installFlags)
    logFile = flagToMaybe (installLogFile installFlags)

    -- A LibraryName is calculated from the transitive closure of
    -- dependencies, but when the solver fails we don't have that.
    -- So we fail.
    dummyLibraryName = error "reportPlanningFailure: library name not available"

-- | If a 'PackageSpecifier' refers to a single package, return Just that package.
theSpecifiedPackage :: Package pkg => PackageSpecifier pkg -> Maybe PackageId
theSpecifiedPackage pkgSpec =
  case pkgSpec of
    NamedPackage name [PackageConstraintVersion name' version]
      | name == name' -> PackageIdentifier name <$> trivialRange version
    NamedPackage _ _ -> Nothing
    SpecificSourcePackage pkg -> Just $ packageId pkg
  where
    -- | If a range includes only a single version, return Just that version.
    trivialRange :: VersionRange -> Maybe Version
    trivialRange = foldVersionRange
        Nothing
        Just     -- "== v"
        (\_ -> Nothing)
        (\_ -> Nothing)
        (\_ _ -> Nothing)
        (\_ _ -> Nothing)

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
  BuildReports.storeLocal (compilerInfo comp) (fromNubList $ installSummaryFile installFlags) buildReports
    (InstallPlan.planPlatform installPlan)
  when (reportingLevel >= AnonymousReports) $
    BuildReports.storeAnonymous buildReports
  when (reportingLevel == DetailedReports) $
    storeDetailedBuildReports verbosity logsDir buildReports

  regenerateHaddockIndex verbosity packageDBs comp platform conf useSandbox
                         configFlags installFlags installPlan

  symlinkBinaries verbosity comp configFlags installFlags installPlan

  printBuildFailures installPlan

  updateSandboxTimestampsFile useSandbox mSandboxPkgInfo
                              comp platform installPlan

  where
    reportingLevel = fromFlag (installBuildReports installFlags)
    logsDir        = fromFlag (globalLogsDir globalFlags)
    oneShot        = fromFlag (installOneShot installFlags)
    worldFile      = fromFlag $ globalWorldFile globalFlags

storeDetailedBuildReports :: Verbosity -> FilePath
                          -> [(BuildReports.BuildReport, Maybe Repo)] -> IO ()
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

  | (report, Just Repo { repoKind = Left remoteRepo }) <- reports
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
                       -> UseSandbox
                       -> ConfigFlags
                       -> InstallFlags
                       -> InstallPlan
                       -> IO ()
regenerateHaddockIndex verbosity packageDBs comp platform conf useSandbox
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
    -- installed. Since the index can be only per-user or per-sandbox (see
    -- #1337), we don't do it for global installs or special cases where we're
    -- installing into a specific db.
    shouldRegenerateHaddockIndex = (isUseSandbox useSandbox || normalUserInstall)
                                && someDocsWereInstalled installPlan
      where
        someDocsWereInstalled = any installedDocs . InstallPlan.toList
        normalUserInstall     = (UserPackageDB `elem` packageDBs)
                             && all (not . isSpecificPackageDB) packageDBs

        installedDocs (InstallPlan.Installed _ (BuildOk DocsOk _ _)) = True
        installedDocs _                                              = False
        isSpecificPackageDB (SpecificPackageDB _) = True
        isSpecificPackageDB _                     = False

    substHaddockIndexFileName defaultDirs = fromPathTemplate
                                          . substPathTemplate env
      where
        env  = env0 ++ installDirsTemplateEnv absoluteDirs
        env0 = InstallDirs.compilerTemplateEnv (compilerInfo comp)
            ++ InstallDirs.platformTemplateEnv platform
            ++ InstallDirs.abiTemplateEnv (compilerInfo comp) platform
        absoluteDirs = InstallDirs.substituteInstallDirTemplates
                         env0 templateDirs
        templateDirs = InstallDirs.combineInstallDirs fromFlagOrDefault
                         defaultDirs (configInstallDirs configFlags)


symlinkBinaries :: Verbosity
                -> Compiler
                -> ConfigFlags
                -> InstallFlags
                -> InstallPlan -> IO ()
symlinkBinaries verbosity comp configFlags installFlags plan = do
  failed <- InstallSymlink.symlinkBinaries comp configFlags installFlags plan
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
                        ++ showException e
      UnpackFailed    e -> " failed while unpacking the package."
                        ++ showException e
      ConfigureFailed e -> " failed during the configure step."
                        ++ showException e
      BuildFailed     e -> " failed during the building phase."
                        ++ showException e
      TestsFailed     e -> " failed during the tests phase."
                        ++ showException e
      InstallFailed   e -> " failed during the final install step."
                        ++ showException e

      -- This will never happen, but we include it for completeness
      PlanningFailed -> " failed during the planning phase."

    showException e   =  " The exception was:\n  " ++ show e ++ maybeOOM e
#ifdef mingw32_HOST_OS
    maybeOOM _        = ""
#else
    maybeOOM e                    = maybe "" onExitFailure (fromException e)
    onExitFailure (ExitFailure n)
      | n == 9 || n == -9         =
      "\nThis may be due to an out-of-memory condition."
    onExitFailure _               = ""
#endif


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
        allSrcPkgs   = [ pkg | ReadyPackage pkg _ _ _ <- allInstalled ]
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
type UseLogFile = Maybe (PackageIdentifier -> LibraryName -> FilePath, Verbosity)

performInstallations :: Verbosity
                     -> InstallArgs
                     -> InstalledPackageIndex
                     -> InstallPlan
                     -> IO InstallPlan
performInstallations verbosity
  (packageDBs, _, comp, _, conf, useSandbox, _,
   globalFlags, configFlags, configExFlags, installFlags, haddockFlags)
  installedPkgIndex installPlan = do

  -- With 'install -j' it can be a bit hard to tell whether a sandbox is used.
  whenUsingSandbox useSandbox $ \sandboxDir ->
    when parallelInstall $
      notice verbosity $ "Notice: installing into a sandbox located at "
                         ++ sandboxDir

  jobControl   <- if parallelInstall then newParallelJobControl
                                     else newSerialJobControl
  buildLimit   <- newJobLimit numJobs
  fetchLimit   <- newJobLimit (min numJobs numFetchJobs)
  installLock  <- newLock -- serialise installation
  cacheLock    <- newLock -- serialise access to setup exe cache

  transport <- configureTransport verbosity (flagToMaybe (globalHttpTransport globalFlags))

  executeInstallPlan verbosity comp jobControl useLogFile installPlan $ \rpkg ->
    -- Calculate the package key (ToDo: Is this right for source install)
    let libname = readyLibraryName comp rpkg in
    installReadyPackage platform cinfo configFlags
                        rpkg $ \configFlags' src pkg pkgoverride ->
      fetchSourcePackage transport verbosity fetchLimit src $ \src' ->
        installLocalPackage verbosity buildLimit
                            (packageId pkg) src' distPref $ \mpath ->
          installUnpackedPackage verbosity buildLimit installLock numJobs libname
                                 (setupScriptOptions installedPkgIndex cacheLock rpkg)
                                 miscOptions configFlags' installFlags haddockFlags
                                 cinfo platform pkg pkgoverride mpath useLogFile Cabal.NoFlag

  where
    platform = InstallPlan.planPlatform installPlan
    cinfo    = InstallPlan.planCompiler installPlan

    numJobs         = determineNumJobs (installNumJobs installFlags)
    numFetchJobs    = 2
    parallelInstall = numJobs >= 2
    distPref        = fromFlagOrDefault (useDistPref defaultSetupScriptOptions)
                      (configDistPref configFlags)

    setupScriptOptions index lock rpkg =
      configureSetupScript
        packageDBs
        comp
        platform
        conf
        distPref
        (chooseCabalVersion configExFlags (libVersion miscOptions))
        (Just lock)
        parallelInstall
        index
        (Just rpkg)

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
          | parallelInstall                   = True
          | otherwise                         = False

        overrideVerbosity :: Bool
        overrideVerbosity
          | reportingLevel == DetailedReports = True
          | isJust installLogFile'            = True
          | parallelInstall                   = False
          | otherwise                         = False

    substLogFileName :: PathTemplate -> PackageIdentifier -> LibraryName -> FilePath
    substLogFileName template pkg libname = fromPathTemplate
                                          . substPathTemplate env
                                          $ template
      where env = initialPathTemplateEnv (packageId pkg) libname
                  (compilerInfo comp) platform

    miscOptions  = InstallMisc {
      rootCmd    = if fromFlag (configUserInstall configFlags)
                      || (isUseSandbox useSandbox)
                     then Nothing      -- ignore --root-cmd if --user
                                       -- or working inside a sandbox.
                     else flagToMaybe (installRootCmd installFlags),
      libVersion = flagToMaybe (configCabalVersion configExFlags)
    }


executeInstallPlan :: Verbosity
                   -> Compiler
                   -> JobControl IO (PackageId, LibraryName, BuildResult)
                   -> UseLogFile
                   -> InstallPlan
                   -> (ReadyPackage -> IO BuildResult)
                   -> IO InstallPlan
executeInstallPlan verbosity comp jobCtl useLogFile plan0 installPkg =
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
                   return (packageId pkg, libname, buildResult)
            | pkg <- pkgs
            , let pkgid = packageId pkg
                  libname = readyLibraryName comp pkg ]

          let taskCount' = taskCount + length pkgs
              plan'      = InstallPlan.processing pkgs plan
          waitForTasks taskCount' plan'

    waitForTasks taskCount plan = do
      info verbosity $ "Waiting for install task to finish..."
      (pkgid, libname, buildResult) <- collectJob jobCtl
      printBuildResult pkgid libname buildResult
      let taskCount' = taskCount-1
          plan'      = updatePlan pkgid buildResult plan
      tryNewTasks taskCount' plan'

    updatePlan :: PackageIdentifier -> BuildResult -> InstallPlan -> InstallPlan
    updatePlan pkgid (Right buildSuccess) =
      InstallPlan.completed (Source.fakeInstalledPackageId pkgid) buildSuccess

    updatePlan pkgid (Left buildFailure) =
      InstallPlan.failed    (Source.fakeInstalledPackageId pkgid) buildFailure depsFailure
      where
        depsFailure = DependentFailed pkgid
        -- So this first pkgid failed for whatever reason (buildFailure).
        -- All the other packages that depended on this pkgid, which we
        -- now cannot build, we mark as failing due to 'DependentFailed'
        -- which kind of means it was not their fault.

    -- Print build log if something went wrong, and 'Installed $PKGID'
    -- otherwise.
    printBuildResult :: PackageId -> LibraryName -> BuildResult -> IO ()
    printBuildResult pkgid libname buildResult = case buildResult of
        (Right _) -> notice verbosity $ "Installed " ++ display pkgid
        (Left _)  -> do
          notice verbosity $ "Failed to install " ++ display pkgid
          when (verbosity >= normal) $
            case useLogFile of
              Nothing                 -> return ()
              Just (mkLogFileName, _) -> do
                let logName = mkLogFileName pkgid libname
                putStr $ "Build log ( " ++ logName ++ " ):\n"
                printFile logName

    printFile :: FilePath -> IO ()
    printFile path = readFile path >>= putStr

-- | Call an installer for an 'SourcePackage' but override the configure
-- flags with the ones given by the 'ReadyPackage'. In particular the
-- 'ReadyPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
-- NB: when updating this function, don't forget to also update
-- 'configurePackage' in D.C.Configure.
installReadyPackage :: Platform -> CompilerInfo
                       -> ConfigFlags
                       -> ReadyPackage
                       -> (ConfigFlags -> PackageLocation (Maybe FilePath)
                                       -> PackageDescription
                                       -> PackageDescriptionOverride -> a)
                       -> a
installReadyPackage platform cinfo configFlags
  (ReadyPackage (SourcePackage _ gpkg source pkgoverride)
   flags stanzas deps)
  installPkg = installPkg configFlags {
    configConfigurationsFlags = flags,
    -- We generate the legacy constraints as well as the new style precise deps.
    -- In the end only one set gets passed to Setup.hs configure, depending on
    -- the Cabal version we are talking to.
    configConstraints  = [ thisPackageVersion (packageId deppkg)
                         | deppkg <- CD.nonSetupDeps deps ],
    configDependencies = [ (packageName (Installed.sourcePackageId deppkg),
                            Installed.installedPackageId deppkg)
                         | deppkg <- CD.nonSetupDeps deps ],
    -- Use '--exact-configuration' if supported.
    configExactConfiguration = toFlag True,
    configBenchmarks         = toFlag False,
    configTests              = toFlag (TestStanzas `elem` stanzas)
  } source pkg pkgoverride
  where
    pkg = case finalizePackageDescription flags
           (const True)
           platform cinfo [] (enableStanzas stanzas gpkg) of
      Left _ -> error "finalizePackageDescription ReadyPackage failed"
      Right (desc, _) -> desc

fetchSourcePackage
  :: HttpTransport
  -> Verbosity
  -> JobLimit
  -> PackageLocation (Maybe FilePath)
  -> (PackageLocation FilePath -> IO BuildResult)
  -> IO BuildResult
fetchSourcePackage transport verbosity fetchLimit src installPkg = do
  fetched <- checkFetched src
  case fetched of
    Just src' -> installPkg src'
    Nothing   -> onFailure DownloadFailed $ do
                   loc <- withJobLimit fetchLimit $
                            fetchPackage transport verbosity src
                   installPkg loc


installLocalPackage
  :: Verbosity
  -> JobLimit
  -> PackageIdentifier -> PackageLocation FilePath -> FilePath
  -> (Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installLocalPackage verbosity jobLimit pkgid location distPref installPkg =

  case location of

    LocalUnpackedPackage dir ->
      installPkg (Just dir)

    LocalTarballPackage tarballPath ->
      installLocalTarballPackage verbosity jobLimit
        pkgid tarballPath distPref installPkg

    RemoteTarballPackage _ tarballPath ->
      installLocalTarballPackage verbosity jobLimit
        pkgid tarballPath distPref installPkg

    RepoTarballPackage _ _ tarballPath ->
      installLocalTarballPackage verbosity jobLimit
        pkgid tarballPath distPref installPkg


installLocalTarballPackage
  :: Verbosity
  -> JobLimit
  -> PackageIdentifier -> FilePath -> FilePath
  -> (Maybe FilePath -> IO BuildResult)
  -> IO BuildResult
installLocalTarballPackage verbosity jobLimit pkgid
                           tarballPath distPref installPkg = do
  tmp <- getTemporaryDirectory
  withTempDirectory verbosity tmp "cabal-tmp" $ \tmpDirPath ->
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
        maybeRenameDistDir absUnpackedPath

      installPkg (Just absUnpackedPath)

  where
    -- 'cabal sdist' puts pre-generated files in the 'dist'
    -- directory. This fails when a nonstandard build directory name
    -- is used (as is the case with sandboxes), so we need to rename
    -- the 'dist' dir here.
    --
    -- TODO: 'cabal get happy && cd sandbox && cabal install ../happy' still
    -- fails even with this workaround. We probably can live with that.
    maybeRenameDistDir :: FilePath -> IO ()
    maybeRenameDistDir absUnpackedPath = do
      let distDirPath    = absUnpackedPath </> defaultDistPref
          distDirPathTmp = absUnpackedPath </> (defaultDistPref ++ "-tmp")
          distDirPathNew = absUnpackedPath </> distPref
      distDirExists <- doesDirectoryExist distDirPath
      when (distDirExists
            && (not $ distDirPath `equalFilePath` distDirPathNew)) $ do
        -- NB: we need to handle the case when 'distDirPathNew' is a
        -- subdirectory of 'distDirPath' (e.g. the former is
        -- 'dist/dist-sandbox-3688fbc2' and the latter is 'dist').
        debug verbosity $ "Renaming '" ++ distDirPath ++ "' to '"
          ++ distDirPathTmp ++ "'."
        renameDirectory distDirPath distDirPathTmp
        when (distDirPath `isPrefixOf` distDirPathNew) $
          createDirectoryIfMissingVerbose verbosity False distDirPath
        debug verbosity $ "Renaming '" ++ distDirPathTmp ++ "' to '"
          ++ distDirPathNew ++ "'."
        renameDirectory distDirPathTmp distDirPathNew

installUnpackedPackage
  :: Verbosity
  -> JobLimit
  -> Lock
  -> Int
  -> LibraryName
  -> SetupScriptOptions
  -> InstallMisc
  -> ConfigFlags
  -> InstallFlags
  -> HaddockFlags
  -> CompilerInfo
  -> Platform
  -> PackageDescription
  -> PackageDescriptionOverride
  -> Maybe FilePath -- ^ Directory to change to before starting the installation.
  -> UseLogFile -- ^ File to log output to (if any)
  -> Cabal.Flag String
  -> IO BuildResult
installUnpackedPackage verbosity buildLimit installLock numJobs libname
                       scriptOptions miscOptions
                       configFlags installFlags haddockFlags
                       cinfo platform pkg pkgoverride workingDir
                       useLogFile view = do

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

  -- Make sure that we pass --libsubdir etc to 'setup configure' (necessary if
  -- the setup script was compiled against an old version of the Cabal lib).
  configFlags' <- addDefaultInstallDirs configFlags
  -- Filter out flags not supported by the old versions of the Cabal lib.
  let configureFlags :: Version -> ConfigFlags
      configureFlags  = filterConfigureFlags configFlags' {
        configVerbosity = toFlag verbosity'
      }

  -- Path to the optional log file.
  mLogPath <- maybeLogPath

  -- Configure phase
  onFailure ConfigureFailed $ withJobLimit buildLimit $ do
    when (numJobs > 1) $ notice verbosity $
      "Configuring " ++ display pkgid ++ "..."
    setup configureCommand configureFlags mLogPath

  -- Build phase
    onFailure BuildFailed $ do
      when (numJobs > 1) $ notice verbosity $
        "Building " ++ display pkgid ++ "..."
      setup buildCommand' buildFlags mLogPath

  -- Doc generation phase
      docsResult <- if shouldHaddock
        then (do setup haddockCommand haddockFlags' mLogPath
                 return DocsOk)
               `catchIO`   (\_ -> return DocsFailed)
               `catchExit` (\_ -> return DocsFailed)
        else return DocsNotTried

  -- Tests phase
      onFailure TestsFailed $ do
        when (testsEnabled && PackageDescription.hasTests pkg) $
            setup Cabal.testCommand testFlags mLogPath

        let testsResult | testsEnabled = TestsOk
                        | otherwise = TestsNotTried

      -- Install phase
        onFailure InstallFailed $ criticalSection installLock $ do
          -- Capture installed package configuration file
          maybePkgConf <- maybeGenPkgConf mLogPath

          -- Actual installation
          withWin32SelfUpgrade verbosity libname configFlags cinfo platform pkg $ do
            case rootCmd miscOptions of
              (Just cmd) -> reexec cmd
              Nothing    -> do
                setup Cabal.copyCommand copyFlags mLogPath
                when shouldRegister $ do
                  setup Cabal.registerCommand registerFlags mLogPath
          return (Right (BuildOk docsResult testsResult maybePkgConf))

  where
    pkgid            = packageId pkg
    buildCommand'    = buildCommand defaultProgramConfiguration
    buildFlags   _   = emptyBuildFlags {
      buildDistPref  = configDistPref configFlags,
      buildVerbosity = toFlag verbosity'
    }
    shouldHaddock    = fromFlag (installDocumentation installFlags)
    haddockFlags' _   = haddockFlags {
      haddockVerbosity = toFlag verbosity',
      haddockDistPref  = configDistPref configFlags
    }
    testsEnabled = fromFlag (configTests configFlags)
                   && fromFlagOrDefault False (installRunTests installFlags)
    testFlags _ = Cabal.emptyTestFlags {
      Cabal.testDistPref = configDistPref configFlags
    }
    copyFlags _ = Cabal.emptyCopyFlags {
      Cabal.copyDistPref   = configDistPref configFlags,
      Cabal.copyDest       = toFlag InstallDirs.NoCopyDest,
      Cabal.copyVerbosity  = toFlag verbosity'
    }
    shouldRegister = PackageDescription.hasLibs pkg
    registerFlags _ = Cabal.emptyRegisterFlags {
      Cabal.regDistPref   = configDistPref configFlags,
      Cabal.regVerbosity  = toFlag verbosity',
      Cabal.regView       = configView configFlags
    }
    verbosity' = maybe verbosity snd useLogFile
    tempTemplate name = name ++ "-" ++ display pkgid

    addDefaultInstallDirs :: ConfigFlags -> IO ConfigFlags
    addDefaultInstallDirs configFlags' = do
      defInstallDirs <- InstallDirs.defaultInstallDirs flavor userInstall False
      return $ configFlags' {
          configInstallDirs = fmap Cabal.Flag .
                              InstallDirs.substituteInstallDirTemplates env $
                              InstallDirs.combineInstallDirs fromFlagOrDefault
                              defInstallDirs (configInstallDirs configFlags)
          }
        where
          CompilerId flavor _ = compilerInfoId cinfo
          env         = initialPathTemplateEnv pkgid libname cinfo platform
          userInstall = fromFlagOrDefault defaultUserInstall
                        (configUserInstall configFlags')

    maybeGenPkgConf :: Maybe FilePath
                    -> IO (Maybe Installed.InstalledPackageInfo)
    maybeGenPkgConf mLogPath =
      if shouldRegister then do
        tmp <- getTemporaryDirectory
        withTempFile tmp (tempTemplate "pkgConf") $ \pkgConfFile handle -> do
          hClose handle
          let registerFlags' version = (registerFlags version) {
                Cabal.regGenPkgConf = toFlag (Just pkgConfFile)
              }
          setup Cabal.registerCommand registerFlags' mLogPath
          withUTF8FileContents pkgConfFile $ \pkgConfText ->
            case Installed.parseInstalledPackageInfo pkgConfText of
              Installed.ParseFailed perror    -> pkgConfParseFailed perror
              Installed.ParseOk warns pkgConf -> do
                unless (null warns) $
                  warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)
                return (Just pkgConf)
      else return Nothing

    pkgConfParseFailed :: Installed.PError -> IO a
    pkgConfParseFailed perror =
      die $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
            ++ show perror

    maybeLogPath :: IO (Maybe FilePath)
    maybeLogPath =
      case useLogFile of
         Nothing                 -> return Nothing
         Just (mkLogFileName, _) -> do
           let logFileName = mkLogFileName (packageId pkg) libname
               logDir      = takeDirectory logFileName
           unless (null logDir) $ createDirectoryIfMissing True logDir
           logFileExists <- doesFileExist logFileName
           when logFileExists $ removeFile logFileName
           return (Just logFileName)

    setup cmd flags mLogPath =
      Exception.bracket
      (traverse (\path -> openFile path AppendMode) mLogPath)
      (traverse_ hClose)
      (\logFileHandle ->
        setupWrapper verbosity
          scriptOptions { useLoggingHandle = logFileHandle
                        , useWorkingDir    = workingDir }
          (Just pkg)
          cmd flags [])

    reexec cmd = do
      -- look for our own executable file and re-exec ourselves using a helper
      -- program like sudo to elevate privileges:
      self <- getExecutablePath
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
-- * Weird windows hacks
-- ------------------------------------------------------------

withWin32SelfUpgrade :: Verbosity
                     -> LibraryName
                     -> ConfigFlags
                     -> CompilerInfo
                     -> Platform
                     -> PackageDescription
                     -> IO a -> IO a
withWin32SelfUpgrade _ _ _ _ _ _ action | buildOS /= Windows = action
withWin32SelfUpgrade verbosity libname configFlags cinfo platform pkg action = do

  defaultDirs <- InstallDirs.defaultInstallDirs
                   compFlavor
                   (fromFlag (configUserInstall configFlags))
                   (PackageDescription.hasLibs pkg)

  Win32SelfUpgrade.possibleSelfUpgrade verbosity
    (exeInstallPaths defaultDirs) action

  where
    pkgid = packageId pkg
    (CompilerId compFlavor _) = compilerInfoId cinfo

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
                           pkgid libname
                           cinfo InstallDirs.NoCopyDest
                           platform templateDirs
        substTemplate  = InstallDirs.fromPathTemplate
                       . InstallDirs.substPathTemplate env
          where env = InstallDirs.initialPathTemplateEnv pkgid libname cinfo platform
