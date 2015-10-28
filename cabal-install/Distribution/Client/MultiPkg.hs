{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

-- | An experimental new UI for cabal for working with multiple packages
-----------------------------------------------------------------------------
module Distribution.Client.MultiPkg (
    -- * High level things
    configure,
    build,
  ) where

import           Distribution.Client.PackageHash
import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanning

import           Distribution.Client.Types hiding (BuildResult, BuildSuccess(..), BuildFailure(..), DocsResult(..), TestsResult(..))
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan )
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.Dependency
import qualified Distribution.Client.ComponentDeps as CD
import           Distribution.Client.ComponentDeps (ComponentDeps)
import           Distribution.Client.Targets
import           Distribution.Client.DistDirLayout
import           Distribution.Client.FileStatusCache
import           Distribution.Client.Glob
import           Distribution.Client.SetupWrapper
import           Distribution.Client.JobControl
import           Distribution.Client.HttpUtils
import           Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar
import           Distribution.Client.Config ({-SavedConfig(..),-} defaultCabalDir)
import           Distribution.Client.Setup hiding (packageName, cabalVersion)

import           Distribution.Package
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription as PD
import           Distribution.PackageDescription (FlagAssignment)
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Client.PackageIndex as SourcePackageIndex
import           Distribution.Simple.Program
import qualified Distribution.Simple.Setup as Cabal
import           Distribution.Simple.Command (CommandUI)
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.InstallDirs as InstallDirs

import           Distribution.Simple.Utils hiding (matchFileGlob)
import           Distribution.Version
import           Distribution.Verbosity
import           Distribution.Text
import           Distribution.ParseUtils ( showPWarning )

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy       as LBS

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.List
import           Data.Monoid

import           System.FilePath hiding (combine)
import           System.IO
import           System.Directory
import           System.Exit

--import Debug.Trace


------------------------------------------------------------------------------
-- * Understanding this module
------------------------------------------------------------------------------

-- This module deals with building and incrementally rebuilding a collection
-- of packages. It is what backs the "cabal build" and "configure" commands,
-- as well as being a core part of "run", "test", "bench" and others. 
--
-- The primary thing is in fact rebuilding (and trying to make that quick by
-- not redoing unnecessary work), so building from scratch is just a special
-- case.
--
-- The build process and the code can be understood by breaking it down into
-- three major parts:
--
-- * The 'ElaboratedInstallPlan' type
--
-- * The "what to do" phase, where we look at the all input configuration
--   (project files, .cabal files, command line etc) and produce a detailed
--   plan of what to do -- the 'ElaboratedInstallPlan'.
--
-- * The "do it" phase, where we take the 'ElaboratedInstallPlan' and we
-- re-execute it.
--
-- As far as possible, the "what to do" phase embodies all the policy, leaving
-- the "do it" phase policy free. The first phase contains more of the
-- complicated logic, but it is contained in code that is either pure or just
-- has read effects (except cache updates). Then the second phase does all the
-- actions to build packages, but as far as possible it just follows the
-- instructions and avoids any logic for deciding what to do (apart from
-- recompilation avoidance in executing the plan).
--
-- This division helps us keep the code under control, making it easier to
-- understand, test and debug. So when you are extending this module, please
-- think about which parts of your change belong in which part. It is
-- perfectly ok to extend the description of what to do (i.e. the 
-- 'ElaboratedInstallPlan') if that helps keep the policy decisions in the
-- first phase. Also, the second phase does not have direct access to any of
-- the input configuration anyway; all the information has to flow via the
-- 'ElaboratedInstallPlan'.


------------------------------------------------------------------------------
-- * Top level commands: build and configure
------------------------------------------------------------------------------

build :: Verbosity
      -> GlobalFlags
      -> ConfigFlags
      -> ConfigExFlags
      -> InstallFlags
      -> Cabal.HaddockFlags
      -> [String]
      -> IO ()
build verbosity
      globalFlags
      configFlags configExFlags
      installFlags haddockFlags
      targetStrings = do

    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- findProjectRoot
    let distDirLayout = defaultDistDirLayout projectRootDir

    let (cliConfig,
         cliBuildSettings) = convertCommandLineFlags
                               globalFlags
                               configFlags configExFlags
                               installFlags haddockFlags

    userTargets <- readUserTargets verbosity targetStrings

    -- The (re)build is split into two major parts:
    --  * decide what to do
    --  * do it

    -- Phase 1: decide what to do.
    --
    -- 1.a) Take the project configuration and make a plan for how to build
    -- everything in the project. This is independent of any specific targets
    -- the user has asked for.
    --
    (elaboratedInstallPlan, sharedPackageConfig, projectConfig) <-
      rebuildInstallPlan verbosity
                         projectRootDir distDirLayout cabalDirLayout
                         cliConfig

    --TODO: [nice to have] some debug or status feature to write out the full
    -- elaboratedInstallPlan details
    -- For now can use this:
    --liftIO $ writeFile (distProjectCacheFile distDirLayout "plan.txt") $
    --           unlines $ show sharedPackageConfig
    --                   : map show (InstallPlan.toList elaboratedInstallPlan)

    let buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          (projectConfigBuildOnly projectConfig)
                          cliBuildSettings

    -- The plan for what to do is represented by an 'ElaboratedInstallPlan'

    -- 1.b) Now given the specific targets the user has asked for, decide
    -- which bits of the plan we will want to execute.
    --
    elaboratedInstallPlan' <-
      selectTargets verbosity
                    cabalDirLayout
                    elaboratedInstallPlan
                    buildSettings
                    userTargets

    -- Phase 2: now do it.
    --
    -- Execute all or parts of the description of what to do to build or
    -- rebuild the various packages needed.
    --
    unless (buildSettingDryRun buildSettings) $
      rebuildTargets verbosity
                     distDirLayout
                     elaboratedInstallPlan'
                     sharedPackageConfig
                     buildSettings

    -- Note that it is a deliberate design choice that the 'userTargets' is
    -- not passed to phase 1, and the various bits of input config is not
    -- passed to phase 2.
    --
    -- We make the install plan without looking at the particular targets the
    -- user asks us to build. The set of available things we can build is
    -- discovered from the env and config and is used to make the install plan.
    -- The targets just tell us which parts of the install plan to execute.
    --
    -- Conversely, executing the plan does not directly depend on any of the
    -- input config. The bits that are needed (or better, the decisions based
    -- on it) all go into the install plan.

    -- Notionally, the 'BuildFlags' should be things that do not affect what
    -- we build, just how we do it. These ones of course do 

-- To a first approximation, configure just runs the first phase of 'build'
-- where we bring the install plan up to date (thus checking that it's
-- possible).
--
-- The only difference is that 'configure' also allows the user to specify
-- some extra local temporary config flags.
--

configure :: Verbosity
          -> GlobalFlags
          -> ConfigFlags
          -> ConfigExFlags
          -> InstallFlags
          -> Cabal.HaddockFlags
          -> [String]
          -> IO ()
configure verbosity
          globalFlags
          configFlags configExFlags
          installFlags haddockFlags
          _extraArgs = do

    -- In this prototype version we don't yet support extra local temporary
    -- config, so it's really just the first phase of build.

    projectRootDir <- findProjectRoot
    cabalDir       <- defaultCabalDir
    let distDirLayout  = defaultDistDirLayout projectRootDir
        cabalDirLayout = defaultCabalDirLayout cabalDir
        (cliConfig,
         _cliBuildSettings) = convertCommandLineFlags 
                                globalFlags
                                configFlags configExFlags
                                installFlags haddockFlags

    _ <- rebuildInstallPlan verbosity
                            projectRootDir distDirLayout cabalDirLayout
                            cliConfig

    return ()


-- | Find the root of this project.
--
-- Searches for an explicit @cabal.project@ file, in the current directory or
-- parent directories. If no project file is found then the current dir is the
-- project root (and the project will use an implicit config).
--
findProjectRoot :: IO FilePath
findProjectRoot = do

    curdir  <- getCurrentDirectory
    homedir <- getHomeDirectory

    -- search upwards if we get to the users home dir or the filesystem root, then use the current dir
    let probe dir | isDrive dir || dir == homedir
                  = return curdir -- implicit project root
        probe dir = do
          exists <- doesFileExist (dir </> "cabal.project")
          if exists
            then return dir       -- explicit project root
            else probe (takeDirectory dir)

    probe curdir
   --TODO: [nice to have] add compat support for old style sandboxes



------------------------------------------------------------------------------
-- * Taking user targets into account, selecting what to build
------------------------------------------------------------------------------


selectTargets :: Verbosity
              -> CabalDirLayout
              -> ElaboratedInstallPlan
              -> BuildTimeSettings
              -> [UserTarget]
              -> IO ElaboratedInstallPlan
selectTargets verbosity
              CabalDirLayout{cabalWorldFile}
              installPlan
              buildSettings
              userTargets = do

    -- First do some defaulting, if no targets are given, use the package
    -- in the current directory if any.
    --
    userTargets' <- case userTargets of
      [] -> do res <- matchFileGlob "." (GlobFile (Glob [WildCard, Literal ".cabal"]))
               case res of
                 [local] -> return [UserTargetLocalCabalFile local]
                 []      -> die $ "TODO: [required feature] decide what to do when no targets are specified and there's no local package, suggest 'all' target?"
                 files   -> die $ "Multiple cabal files found.\n"
                               ++ "Please use only one of: "
                               ++ intercalate ", " files
      _  -> return userTargets

    -- Expand the world target (not really necessary) and disambiguate case
    -- insensitive package names to actual package names.
    --
    packageTargets <- concat <$> mapM (expandUserTarget cabalWorldFile) userTargets'
    let packageSpecifiers :: [PackageSpecifier (PackageLocation ())]
        (problems, packageSpecifiers) =
           disambiguatePackageTargets available availableExtra packageTargets

        --TODO: [required eventually] we're using an empty set of known package names here, which
        -- means we cannot resolve names of packages other than those that are
        -- directly in the current plan. We ought to keep a set of the known
        -- hackage packages so we can resolve names to those. Though we don't
        -- really need that until we can do something sensible with packages
        -- outside of the project.
        available :: SourcePackageIndex.PackageIndex PackageId
        available      = mempty
        availableExtra = map packageName (InstallPlan.toList installPlan)
    reportPackageTargetProblems verbosity problems

    -- Now check if those targets belong to the current project or not.
    -- Ultimately we want to do something sensible for targets not in this
    -- project, but for now we just bail. This gives us back the ipkgid from
    -- the plan.
    --
    targetPkgids <- checkTargets installPlan packageSpecifiers

    -- Finally, prune the install plan to cover just those target packages
    -- and their deps.
    --
    let installPlan' :: ElaboratedInstallPlan
        Right installPlan' =
          InstallPlan.new False $
            PackageIndex.fromList $
              InstallPlan.dependencyClosure installPlan targetPkgids

    -- Tell the user what we're going to do
    checkPrintPlan verbosity
                   installPlan'
                   buildSettings

    return installPlan'


checkTargets :: ElaboratedInstallPlan
             -> [PackageSpecifier (PackageLocation ())]
             -> IO [InstalledPackageId]
checkTargets installPlan targets = do

    -- Our first task is to work out if a target refers to something inside
    -- the project or outside, since we will behave differently in these cases.
    --
    -- It's not quite as simple as checking if the target refers to a package
    -- name that is inside the project because one might refer to a local
    -- package dir that is not in the project but that has the same name (e.g.
    -- a different local fork of a package). So the logic looks like this:
    -- * for targets with a package location, check it is in the set of
    --   locations of packages within the project
    -- * for targets with just a name, just check it is in the set of names of
    --   packages in the project.

    let projLocalPkgLocs =
          Map.fromList
            [ (fmap (const ()) (pkgSourceLocation pkg), installedPackageId pkg)
            | InstallPlan.Configured pkg <- InstallPlan.toList installPlan ]

        projAllPkgs =
          Map.fromList
            [ (packageName pkg, installedPackageId pkg)
            | pkg <- InstallPlan.toList installPlan ]
        --TODO: [research required] what if the solution has multiple versions of this package?
        --      e.g. due to setup deps or due to multiple independent sets of
        --      packages being built (e.g. ghc + ghcjs in a project)

    mapM (checkTarget projLocalPkgLocs projAllPkgs) targets

  where
    checkTarget projLocalPkgLocs _projAllPkgs (SpecificSourcePackage loc) = do
      absloc <- canonicalizePackageLocation loc
      case Map.lookup absloc projLocalPkgLocs of
        Nothing    -> die $ show loc ++ " is not in the project"
        Just pkgid -> return pkgid

    checkTarget _projLocalPkgLocs projAllPkgs (NamedPackage pkgname _constraints) = do
      case Map.lookup pkgname projAllPkgs of
        Nothing     -> die $ display pkgname ++ " is not in the project"
        Just ipkgid -> return ipkgid

canonicalizePackageLocation :: PackageLocation a -> IO (PackageLocation a)
canonicalizePackageLocation loc =
  case loc of
    LocalUnpackedPackage path -> LocalUnpackedPackage <$> canonicalizePath path
    LocalTarballPackage  path -> LocalTarballPackage  <$> canonicalizePath path
    _                         -> return loc


-------------------------------
-- Displaying plan info
--

checkPrintPlan :: Verbosity
               -> ElaboratedInstallPlan
               -> BuildTimeSettings
               -> IO ()
checkPrintPlan verbosity installPlan buildSettings =
    printPlan verbosity dryRun (linearizeInstallPlan installPlan)
    --TODO: [required eventually] see Install.hs for other things checkPrintPlan ought to do too
  where
    dryRun = buildSettingDryRun buildSettings


linearizeInstallPlan :: ElaboratedInstallPlan -> [ElaboratedReadyPackage]
linearizeInstallPlan =
    unfoldr next
  where
    next plan = case InstallPlan.ready plan of
      []          -> Nothing
      ((pkg,_):_) -> Just (pkg, plan')
        where
          pkgid  = installedPackageId pkg
          ipkg   = Installed.emptyInstalledPackageInfo {
                     Installed.sourcePackageId    = packageId pkg,
                     Installed.installedPackageId = pkgid
                   }
          plan'  = InstallPlan.completed pkgid (Just ipkg)
                     (BuildOk True DocsNotTried TestsNotTried)
                     (InstallPlan.processing [pkg] plan)
    --TODO: [code cleanup] This is a bit of a hack, pretending that each package is installed
    -- could we use InstallPlan.topologicalOrder?

printPlan :: Verbosity
          -> Bool -- is dry run
          -> [ElaboratedReadyPackage]
          -> IO ()
printPlan _         _      []   = return ()
printPlan verbosity dryRun pkgs
  | verbosity >= verbose
  = notice verbosity $ unlines $
      ("In order, the following " ++ wouldWill ++ " be built:")
    : map showPkgAndReason pkgs

  | otherwise
  = notice verbosity $ unlines $
      ("In order, the following " ++ wouldWill
       ++ " be built (use -v for more details):")
    : map showPkg pkgs
  where
    wouldWill | dryRun    = "would"
              | otherwise = "will"

    showPkg pkg = display (packageId pkg)

    showPkgAndReason :: ElaboratedReadyPackage -> String
    showPkgAndReason (ReadyPackage pkg' _) =
      display (packageId pkg') ++
      showFlagAssignment (nonDefaultFlags pkg') ++
      showStanzas (pkgEnabledStanzas pkg')

    toFlagAssignment :: [PD.Flag] -> FlagAssignment
    toFlagAssignment = map (\ f -> (Cabal.flagName f, Cabal.flagDefault f))

    nonDefaultFlags :: ElaboratedConfiguredPackage -> FlagAssignment
    nonDefaultFlags ElaboratedConfiguredPackage {
                      pkgFlagAssignment, pkgDescription
                    } =
      let defaultAssignment =
            toFlagAssignment
             (Cabal.genPackageFlags pkgDescription)
      in  pkgFlagAssignment \\ defaultAssignment

    showStanzas :: [OptionalStanza] -> String
    showStanzas = concatMap ((' ' :) . showStanza)
    showStanza TestStanzas  = "*test"
    showStanza BenchStanzas = "*bench"

    -- TODO: [code cleanup] this should be a proper function in a proper place
    showFlagAssignment :: FlagAssignment -> String
    showFlagAssignment = concatMap ((' ' :) . showFlagValue)
    showFlagValue (f, True)   = '+' : showFlagName f
    showFlagValue (f, False)  = '-' : showFlagName f
    showFlagName (Cabal.FlagName f) = f


------------------------------------------------------------------------------
-- * Doing it: executing an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------


rebuildTargets :: Verbosity
               -> DistDirLayout
               -> ElaboratedInstallPlan
               -> ElaboratedSharedConfig
               -> BuildTimeSettings
               -> IO ()
rebuildTargets verbosity
               distDirLayout@DistDirLayout{..}
               installPlan
               sharedPackageConfig
               buildSettings@BuildTimeSettings{
                 buildSettingNumJobs,
                 buildSettingHttpTransport
               } = do

    -- Concurrency control: create the job controller and concurrency limits
    -- for downloading, building and installing.
    jobControl    <- if isParallelBuild then newParallelJobControl
                                        else newSerialJobControl
    buildLimit    <- newJobLimit buildSettingNumJobs
    installLock   <- newLock -- serialise installation
    cacheLock     <- newLock -- serialise access to setup exe cache
                             --TODO: [code cleanup] eliminate setup exe cache

    createDirectoryIfMissingVerbose verbosity False distBuildRootDirectory
    createDirectoryIfMissingVerbose verbosity False distTempDirectory

    -- Before traversing the install plan, pre-emptively find all packages that
    -- will need to be downloaded and start downloading them.
    pkgsToDownload <- packagesRequiringDownload
                        pkgSourceLocation
                        installPlan

    _residualPlan <- 
      asyncDownloadPackages verbosity mkTransport
                            pkgsToDownload $ \downloadMap ->

      -- For each package in the plan, in dependency order, but in parallel...
      executeInstallPlan verbosity jobControl installPlan $
        \rpkg@(ReadyPackage cpkg _) depResults ->
        handle (return . BuildFailure) $ do

        -- If necessary, wait for the download of the remote package to finish.
        srcloc <- waitAsyncPackageDownload verbosity downloadMap cpkg

        -- We're not typing up buildLimit-limited resources while downloading,
        -- but we are once we start unpacking and building.
        withJobLimit buildLimit $
          withPackageInLocalDirectory
            verbosity distDirLayout srcloc
            (packageId rpkg) (pkgBuildStyle cpkg)
            (pkgDescriptionOverride cpkg)$ \srcdir builddir ->

            case pkgBuildStyle cpkg of
              BuildAndInstall ->
                buildAndInstallUnpackedPackage
                  verbosity distDirLayout
                  buildSettings installLock cacheLock
                  sharedPackageConfig
                  rpkg
                  srcdir builddir'
                where
                  builddir' = makeRelative srcdir builddir
                  --TODO: [nice to have] ^^ do this relative stuff better

              BuildInplaceOnly ->
                --TODO: [nice to have] use a relative build dir rather than absolute
                buildInplaceUnpackedPackage
                  verbosity distDirLayout
                  buildSettings cacheLock
                  sharedPackageConfig
                  rpkg depResults
                  srcdir builddir

    return ()

  where
    isParallelBuild = buildSettingNumJobs >= 2
    mkTransport     = configureTransport verbosity buildSettingHttpTransport


--TODO: [nice to have] do we need to use a with-style for the temp files for downloading http
-- packages, or are we going to cache them persistently?

-- | Given the current 'InstallPlan', work out which packages will require
-- a download step of some form, and return the locations of those packages,
-- in the dependency order (so that the one's we'll need first are the ones
-- we will start downloading first).
--
packagesRequiringDownload :: (srcpkg -> PackageLocation (Maybe FilePath))
                          -> GenericInstallPlan ipkg srcpkg iresult ifailure
                          -> IO [PackageLocation (Maybe FilePath)]
packagesRequiringDownload packageSourceLocation installPlan =
    filterM (fmap not . isFetched)
      [ packageSourceLocation pkg
      | InstallPlan.Configured pkg
         <- InstallPlan.reverseTopologicalOrder installPlan
      ]

type AsyncDownloadMap = Map (PackageLocation (Maybe FilePath))
                            (MVar (PackageLocation FilePath))

-- | Fork off an async action to download all the given packages. The body
-- action is passed a map from those packages (identified by their location)
-- to a completion var for that package. So the body action should lookup the
-- location and use 'waitAsyncPackageDownload' to get the result.
--
asyncDownloadPackages :: Verbosity
                      -> IO HttpTransport
                      -> [PackageLocation (Maybe FilePath)]
                      -> (AsyncDownloadMap -> IO a)
                      -> IO a
asyncDownloadPackages _ _ [] body = body Map.empty
asyncDownloadPackages verbosity mkTransport pkglocs0 body = do
    --TODO: [research required] use parallel downloads? if so, use the fetchLimit

    asyncDownloadVars <- mapM (\loc -> (,) loc <$> newEmptyMVar) pkglocs0
    transport         <- mkTransport

    let downloadAction = 
          mapM $ \(pkgloc, var) ->
            fetchPackage transport verbosity pkgloc >>= putMVar var

    withAsync (downloadAction asyncDownloadVars) $ \_ ->
      let !asyncDownloadMap = Map.fromList asyncDownloadVars
      in body asyncDownloadMap


-- | Check if a package needs downloading, and if so expect to find a download
-- in progress in the given 'AsyncDownloadMap' and wait on it to finish.
--
waitAsyncPackageDownload :: Verbosity
                         -> AsyncDownloadMap
                         -> ElaboratedConfiguredPackage
                         -> IO (PackageLocation FilePath)
waitAsyncPackageDownload verbosity downloadMap pkg = do
    mloc <- checkFetched (pkgSourceLocation pkg)
    case mloc of
      Nothing  -> do let Just hnd = Map.lookup (pkgSourceLocation pkg) downloadMap
                     debug verbosity $ "Waiting for download of "
                                    ++ display (packageId pkg)
                                    ++ " to finish"
                     takeMVar hnd
      Just loc -> return loc
--TODO: [required eventually] do the exception handling on download stuff


-- for the build cache we use one of two methods:
--  for build type Simple we assume we do actually know the files in the package
--  for other build types we look at all files, only excluding known boring


executeInstallPlan
  :: forall ipkg srcpkg iresult.
     (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
      HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
  => Verbosity
  -> JobControl IO ( GenericReadyPackage srcpkg ipkg
                   , GenericBuildResult ipkg iresult BuildFailure )
  -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
  -> (    GenericReadyPackage srcpkg ipkg
       -> ComponentDeps [iresult]
       -> IO (GenericBuildResult ipkg iresult BuildFailure))
  -> IO (GenericInstallPlan ipkg srcpkg iresult BuildFailure)
executeInstallPlan verbosity jobCtl plan0 installPkg =
    tryNewTasks 0 plan0
  where
    tryNewTasks taskCount plan = do
      case InstallPlan.ready plan of
        [] | taskCount == 0 -> return plan
           | otherwise      -> waitForTasks taskCount plan
        pkgs                -> do
          sequence_
            [ do debug verbosity $ "Ready to install " ++ display pkgid
                 spawnJob jobCtl $ do
                   buildResult <- installPkg pkg depResults
                   return (pkg, buildResult)
            | (pkg, depResults) <- pkgs
            , let pkgid = packageId pkg
            ]

          let taskCount' = taskCount + length pkgs
              plan'      = InstallPlan.processing (map fst pkgs) plan
          waitForTasks taskCount' plan'

    waitForTasks taskCount plan = do
      debug verbosity $ "Waiting for install task to finish..."
      (pkg, buildResult) <- collectJob jobCtl
      let taskCount' = taskCount-1
          plan'      = updatePlan pkg buildResult plan
      tryNewTasks taskCount' plan'

    updatePlan :: GenericReadyPackage srcpkg ipkg
               -> GenericBuildResult ipkg iresult BuildFailure
               -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
               -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
    updatePlan pkg (BuildSuccess mipkg buildSuccess) =
        InstallPlan.completed (installedPackageId pkg) mipkg buildSuccess

    updatePlan pkg (BuildFailure buildFailure) =
        InstallPlan.failed (installedPackageId pkg) buildFailure depsFailure
      where
        depsFailure = DependentFailed (packageId pkg)
        -- So this first pkgid failed for whatever reason (buildFailure).
        -- All the other packages that depended on this pkgid, which we
        -- now cannot build, we mark as failing due to 'DependentFailed'
        -- which kind of means it was not their fault.


-- | Ensure that the package is unpacked in an appropriate directory, either
-- a temporary one or a persistent one under the shared dist directory. 
--
withPackageInLocalDirectory
  :: Verbosity
  -> DistDirLayout
  -> PackageLocation FilePath
  -> PackageId
  -> BuildStyle
  -> Maybe CabalFileText
  -> (FilePath -> FilePath -> IO a)
  -> IO a
withPackageInLocalDirectory verbosity distDirLayout@DistDirLayout{..}
                            location pkgid buildstyle pkgTextOverride
                            buildPkg =

    case location of

      -- For the case of a user-managed local dir, irrespective of the build
      -- style, we build from that directory and put build artifacts under the
      -- shared dist directory.
      LocalUnpackedPackage srcdir ->
        buildPkg srcdir (distBuildDirectory pkgid)

      -- The three tarball cases are handled the same as each other,
      -- though depending on the build style.
      LocalTarballPackage    tarball -> withTarballLocalDirectory tarball
      RemoteTarballPackage _ tarball -> withTarballLocalDirectory tarball
      RepoTarballPackage _ _ tarball -> withTarballLocalDirectory tarball

  where
    withTarballLocalDirectory tarball =
      case buildstyle of
        -- In this case we make a temp dir, unpack the tarball to there and
        -- build and install it from that temp dir.
        BuildAndInstall ->
          withTempDirectory verbosity distTempDirectory
                            (display (packageName pkgid)) $ \tmpdir -> do
            unpackPackageTarball verbosity tarball tmpdir
                                 pkgid pkgTextOverride
            let srcdir   = tmpdir </> display pkgid
                builddir = srcdir </> "dist"
            buildPkg srcdir builddir

        -- In this case we make sure the tarball has been unpacked to the
        -- appropriate location under the shared dist dir, and then build it
        -- inplace there
        BuildInplaceOnly -> do
          let srcrootdir = distUnpackedSrcRootDirectory
              srcdir     = distUnpackedSrcDirectory pkgid
              builddir   = distBuildDirectory pkgid
          -- TODO: [nice to have] use a proper file monitor rather than this dir exists test
          exists <- doesDirectoryExist srcdir
          unless exists $ do
            createDirectoryIfMissingVerbose verbosity False srcrootdir
            unpackPackageTarball verbosity tarball srcrootdir
                                 pkgid pkgTextOverride
            moveTarballShippedDistDirectory verbosity distDirLayout
                                            srcrootdir pkgid
          buildPkg srcdir builddir


unpackPackageTarball :: Verbosity -> FilePath -> FilePath
                     -> PackageId -> Maybe CabalFileText
                     -> IO ()
unpackPackageTarball verbosity tarball parentdir pkgid pkgTextOverride =
    --TODO: [nice to have] switch to tar package and catch tar exceptions
    annotateFailure UnpackFailed $ do

      -- Unpack the tarball
      --
      info verbosity $ "Extracting " ++ tarball ++ " to " ++ parentdir ++ "..."
      Tar.extractTarGzFile parentdir pkgsubdir tarball

      -- Sanity check
      --
      exists <- doesFileExist cabalFile
      when (not exists) $
        die $ "Package .cabal file not found in the tarball: " ++ cabalFile

      -- Overwrite the .cabal with the one from the index, when appropriate
      --
      case pkgTextOverride of
        Nothing     -> return ()
        Just pkgtxt -> do
          info verbosity $ "Updating " ++ display pkgname <.> "cabal"
                        ++ " with the latest revision from the index."
          writeFileAtomic cabalFile pkgtxt

  where
    cabalFile = parentdir </> pkgsubdir
                          </> display pkgname <.> "cabal"
    pkgsubdir = display pkgid
    pkgname   = packageName pkgid


-- | This is a bit of a hacky workaround. A number of packages ship
-- pre-processed .hs files in a dist directory inside the tarball. We don't
-- use the standard 'dist' location so unless we move this dist dir to the
-- right place then we'll miss the shipped pre-procssed files. This hacky
-- approach to shipped pre-procssed files ought to be replaced by a proper
-- system, though we'll still need to keep this hack for older packages.
--
moveTarballShippedDistDirectory :: Verbosity -> DistDirLayout
                                -> FilePath -> PackageId -> IO ()
moveTarballShippedDistDirectory verbosity DistDirLayout{distBuildDirectory}
                                parentdir pkgid = do
    distDirExists <- doesDirectoryExist tarballDistDir
    when distDirExists $ do
      debug verbosity $ "Moving '" ++ tarballDistDir ++ "' to '"
                                   ++ targetDistDir ++ "'"
      --TODO: [nice to have] or perhaps better to copy, and use a file monitor
      renameDirectory tarballDistDir targetDistDir
  where
    tarballDistDir = parentdir </> display pkgid </> "dist"
    targetDistDir  = distBuildDirectory pkgid


buildAndInstallUnpackedPackage :: Verbosity
                               -> DistDirLayout
                               -> BuildTimeSettings -> Lock -> Lock
                               -> ElaboratedSharedConfig
                               -> ElaboratedReadyPackage
                               -> FilePath -> FilePath
                               -> IO BuildResult
buildAndInstallUnpackedPackage verbosity
                               DistDirLayout{distTempDirectory}
                               BuildTimeSettings {
                                 buildSettingNumJobs,
                                 buildSettingLogFile
                               }
                               installLock cacheLock
                               pkgshared@ElaboratedSharedConfig {
                                 pkgConfigCompiler  = compiler,
                                 pkgConfigPlatform  = platform,
                                 pkgConfigProgramDb = progdb
                               }
                               rpkg@(ReadyPackage pkg _deps)
                               srcdir builddir = do

    createDirectoryIfMissingVerbose verbosity False builddir
    initLogFile

    --TODO: [code cleanup] deal consistently with talking to older Setup.hs versions, much like
    --      we do for ghc, with a proper options type and rendering step
    --      which will also let us call directly into the lib, rather than always
    --      going via the lib's command line interface, which would also allow
    --      passing data like installed packages, compiler, and program db for a
    --      quicker configure.

    --TODO: [required feature] docs and tests
    --TODO: [required feature] sudo re-exec

    -- Configure phase
    when isParallelBuild $
      notice verbosity $ "Configuring " ++ display pkgid ++ "..."
    annotateFailure ConfigureFailed $
      setup configureCommand' configureFlags

    -- Build phase
    when isParallelBuild $
      notice verbosity $ "Building " ++ display pkgid ++ "..."
    annotateFailure BuildFailed $
      setup buildCommand' buildFlags

    -- Install phase
    mipkg <-
      criticalSection installLock $ 
      annotateFailure InstallFailed $ do
      --TODO: [research required] do we need the installLock for copying? can we not do that in
      -- parallel? Isn't it just registering that we have to lock for?

      --TODO: [required eventually] need to lock installing this ipkig so other processes don't
      -- stomp on our files, since we don't have ABI compat, not safe to replace

      -- TODO: [required eventually] note that for nix-style installations it is not necessary to do
      -- the 'withWin32SelfUpgrade' dance, but it would be necessary for a
      -- shared bin dir.

      -- Actual installation
      setup Cabal.copyCommand copyFlags
      
      LBS.writeFile
        (InstallDirs.prefix (pkgInstallDirs pkg) </> "cabal-hash.txt") $
        (renderPackageHashInputs (packageHashInputs pkgshared pkg))

      -- here's where we could keep track of the installed files ourselves if
      -- we wanted by calling copy to an image dir and then we would make a
      -- manifest and move it to its final location

      --TODO: [nice to have] we should actually have it make an image in store/incomming and
      -- then when it's done, move it to its final location, to reduce problems
      -- with installs failing half-way. Could also register and then move.

      -- For libraries, grab the package configuration file
      -- and register it ourselves
      if pkgRequiresRegistration pkg
        then do
          ipkg <- generateInstalledPackageInfo
          -- We register ourselves rather than via Setup.hs. We need to
          -- grab and modify the InstalledPackageInfo. We decide what
          -- the installed package id is, not the build system.
          let ipkg' = ipkg { Installed.installedPackageId = ipkgid }
          Cabal.registerPackage verbosity compiler progdb
                                True -- multi-instance, nix style
                                (pkgRegisterPackageDBStack pkg) ipkg'
          return (Just ipkg')
        else return Nothing

    --TODO: [required feature] docs and test phases
    let docsResult  = DocsNotTried
        testsResult = TestsNotTried

    return (BuildSuccess mipkg (BuildOk True docsResult testsResult))

  where
    pkgid  = packageId rpkg
    ipkgid = installedPackageId rpkg

    isParallelBuild = buildSettingNumJobs >= 2

    configureCommand'= Cabal.configureCommand defaultProgramConfiguration
    configureFlags v = flip filterConfigureFlags v $
                       setupHsConfigureFlags rpkg pkgshared
                                             verbosity builddir

    buildCommand'    = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = setupHsBuildFlags pkg pkgshared verbosity builddir

    generateInstalledPackageInfo :: IO InstalledPackageInfo
    generateInstalledPackageInfo =
      withTempInstalledPackageInfoFile
        verbosity distTempDirectory $ \pkgConfFile -> do
        -- make absolute since setup changes dir
        pkgConfFile' <- canonicalizePath pkgConfFile
        let registerFlags _ = setupHsRegisterFlags
                                pkg pkgshared
                                verbosity builddir
                                pkgConfFile'
        setup Cabal.registerCommand registerFlags

    copyFlags _ = setupHsCopyFlags pkg pkgshared verbosity builddir

    scriptOptions = setupHsScriptOptions rpkg pkgshared srcdir builddir
                                         isParallelBuild cacheLock

    setup :: CommandUI flags -> (Version -> flags) -> IO ()
    setup cmd flags =
      withLogging $ \mLogFileHandle -> 
        setupWrapper
          verbosity
          scriptOptions { useLoggingHandle = mLogFileHandle }
          (Just (Cabal.packageDescription (pkgDescription pkg)))
          cmd flags []

    mlogFile =
      case buildSettingLogFile of
        Nothing        -> Nothing
        Just mkLogFile -> Just (mkLogFile compiler platform pkgid libname)
          where
            --TODO: [code cleanup] please, can we not get rid of package keys in templates?
            libname = readyLibraryName compiler rpkg 

    initLogFile =
      case mlogFile of
        Nothing      -> return ()
        Just logFile -> do
          createDirectoryIfMissing True (takeDirectory logFile)
          exists <- doesFileExist logFile
          when exists $ removeFile logFile

    withLogging action =
      case mlogFile of
        Nothing      -> action Nothing
        Just logFile -> withFile logFile AppendMode (action . Just)


buildInplaceUnpackedPackage :: Verbosity
                            -> DistDirLayout
                            -> BuildTimeSettings -> Lock
                            -> ElaboratedSharedConfig
                            -> GenericReadyPackage ElaboratedConfiguredPackage
                                                   InstalledPackageInfo
                            -> ComponentDeps [BuildSuccess]
                            -> FilePath -> FilePath
                            -> IO BuildResult
buildInplaceUnpackedPackage verbosity
                            DistDirLayout {
                              distTempDirectory,
                              distPackageCacheFile,
                              distPackageCacheDirectory
                            }
                            BuildTimeSettings{buildSettingNumJobs}
                            cacheLock
                            pkgshared@ElaboratedSharedConfig {
                              pkgConfigCompiler  = compiler,
                              pkgConfigProgramDb = progdb
                            }
                            rpkg@(ReadyPackage pkg _deps)
                            depResults
                            srcdir builddir = do

    -- The configChanged here includes the identity of the dependencies, so
    -- depsChanged below is just needed for the changes in the content of deps.
    --
    configChanged <- checkFileMonitorChanged configFileMonitor srcdir rpkg
    buildChanged  <- checkFileMonitorChanged buildFileMonitor  srcdir ()
    let depsChanged = not $ null [ () | BuildOk True _ _ <- CD.flatDeps depResults ]
    --TODO: [nice to have] some debug-level message about file changes, like rerunIfChanged

    case (configChanged, buildChanged, depsChanged) of
      (Unchanged (mipkg, _), Unchanged (buildSuccess, _), False) ->
          return (BuildSuccess mipkg (markUnchanged buildSuccess)) --TODO: [code cleanup] make this cleaner
        where
          markUnchanged :: BuildSuccess -> BuildSuccess
          markUnchanged (BuildOk _ b c) = BuildOk False b c

      _ -> do
        --TODO: [code cleanup] there is duplication between the distdirlayout and the builddir here
        --      builddir is not enough, we also need the per-package cachedir
        createDirectoryIfMissingVerbose verbosity False builddir
        createDirectoryIfMissingVerbose verbosity False (distPackageCacheDirectory pkgid)
        createPackageDBIfMissing verbosity compiler progdb (pkgBuildPackageDBStack pkg)

        -- Configure phase
        whenChanged configChanged $ do
          when isParallelBuild $
            notice verbosity $ "Configuring " ++ display pkgid ++ "..."
          setup configureCommand' configureFlags

        -- Build phase
        when isParallelBuild $
          notice verbosity $ "Building " ++ display pkgid ++ "..."
        setup buildCommand' buildFlags

        -- Register locally
        mipkg <- case configChanged of
          Unchanged (mipkg, _) -> return mipkg
          Changed   _
            | pkgRequiresRegistration pkg -> do
                ipkg <- generateInstalledPackageInfo
                -- We register ourselves rather than via Setup.hs. We need to
                -- grab and modify the InstalledPackageInfo. We decide what
                -- the installed package id is, not the build system.
                let ipkg' = ipkg { Installed.installedPackageId = ipkgid }
                Cabal.registerPackage verbosity compiler progdb False
                                      (pkgRegisterPackageDBStack pkg)
                                      ipkg'
                return (Just ipkg')

           | otherwise -> return Nothing

        let docsResult  = DocsNotTried
            testsResult = TestsNotTried
            
            buildSuccess :: BuildSuccess
            buildSuccess = BuildOk True docsResult testsResult

        whenChanged configChanged $
          updateFileMonitor configFileMonitor srcdir
                            []
                            rpkg mipkg

        --TODO: [required eventually] temporary hack. We need to look at the package description
        -- and work out the exact file monitors to use
        allSrcFiles <- filter (not . ("dist-newstyle" `isPrefixOf`))
                   <$> getDirectoryContentsRecursive srcdir

        updateFileMonitor buildFileMonitor srcdir
                          (map MonitorFileHashed allSrcFiles)
                          () buildSuccess

        return (BuildSuccess mipkg buildSuccess)

  where
    pkgid  = packageId rpkg
    ipkgid = installedPackageId rpkg

    isParallelBuild = buildSettingNumJobs >= 2

    configFileMonitor :: FileMonitorCacheFile
                           (GenericReadyPackage ElaboratedConfiguredPackage
                                                InstalledPackageInfo)
                           (Maybe InstalledPackageInfo)
    configFileMonitor = FileMonitorCacheFile (distPackageCacheFile pkgid "config")

    buildFileMonitor  :: FileMonitorCacheFile () BuildSuccess
    buildFileMonitor  = FileMonitorCacheFile (distPackageCacheFile pkgid "build")

    whenChanged (Changed   _) action = action
    whenChanged (Unchanged _) _      = return ()

    configureCommand'= Cabal.configureCommand defaultProgramConfiguration
    configureFlags v = flip filterConfigureFlags v $
                       setupHsConfigureFlags rpkg pkgshared
                                             verbosity builddir

    buildCommand'    = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = setupHsBuildFlags pkg pkgshared
                                         verbosity builddir

    scriptOptions    = setupHsScriptOptions rpkg pkgshared
                                            srcdir builddir
                                            isParallelBuild cacheLock

    setup :: CommandUI flags -> (Version -> flags) -> IO ()
    setup cmd flags =
      setupWrapper verbosity
                   scriptOptions
                   (Just (Cabal.packageDescription (pkgDescription pkg)))
                   cmd flags []

    generateInstalledPackageInfo :: IO InstalledPackageInfo
    generateInstalledPackageInfo =
      withTempInstalledPackageInfoFile
        verbosity distTempDirectory $ \pkgConfFile -> do
        -- make absolute since setup changes dir
        pkgConfFile' <- canonicalizePath pkgConfFile
        let registerFlags _ = setupHsRegisterFlags
                                pkg pkgshared
                                verbosity builddir
                                pkgConfFile'
        setup Cabal.registerCommand registerFlags


-- helper
annotateFailure :: (String -> BuildFailure) -> IO a -> IO a
annotateFailure annotate action =
  action `catches`
    [ Handler $ \ioe  -> handler (ioe  :: IOException)
    , Handler $ \exit -> handler (exit :: ExitCode)
    ]
  where
    handler :: Exception e => e -> IO a
    handler = throwIO . annotate . show
                       --TODO: [nice to have] use displayException when available


withTempInstalledPackageInfoFile :: Verbosity -> FilePath
                                 -> (FilePath -> IO ())
                                 -> IO InstalledPackageInfo
withTempInstalledPackageInfoFile verbosity tempdir action =
    withTempFile tempdir "package-registration-" $ \pkgConfFile hnd -> do
      hClose hnd
      action pkgConfFile

      (warns, ipkg) <- withUTF8FileContents pkgConfFile $ \pkgConfStr ->
        case Installed.parseInstalledPackageInfo pkgConfStr of
          Installed.ParseFailed perror -> pkgConfParseFailed perror
          Installed.ParseOk warns ipkg -> return (warns, ipkg)

      unless (null warns) $
        warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)

      return ipkg
  where
    pkgConfParseFailed :: Installed.PError -> IO a
    pkgConfParseFailed perror =
      die $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
            ++ show perror

