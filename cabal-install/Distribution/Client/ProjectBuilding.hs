{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

-- | 
--
module Distribution.Client.ProjectBuilding (

    rebuildTargets
  ) where

import           Distribution.Client.PackageHash (renderPackageHashInputs)
import           Distribution.Client.RebuildMonad
import           Distribution.Client.ProjectConfig (BuildTimeSettings(..))
import           Distribution.Client.ProjectPlanning

import           Distribution.Client.Types (PackageLocation(..), GenericReadyPackage(..), PackageFixedDeps)
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan )
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.ComponentDeps as CD
import           Distribution.Client.ComponentDeps (ComponentDeps)
import           Distribution.Client.DistDirLayout
import           Distribution.Client.FileStatusCache (Changed(..), checkFileMonitorChanged, updateFileMonitor)
import           Distribution.Client.SetupWrapper
import           Distribution.Client.JobControl
import           Distribution.Client.HttpUtils
import           Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar
import           Distribution.Client.Setup (filterConfigureFlags)

import           Distribution.Package
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
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
import qualified Data.ByteString.Lazy as LBS

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.List

import           System.FilePath
import           System.IO
import           System.Directory
import           System.Exit (ExitCode)


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
     (HasComponentId ipkg,   PackageFixedDeps ipkg,
      HasComponentId srcpkg, PackageFixedDeps srcpkg)
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
          let ipkg' = ipkg { Installed.installedComponentId = ipkgid }
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
          (Just (pkgDescription pkg))
          cmd flags []

    mlogFile =
      case buildSettingLogFile of
        Nothing        -> Nothing
        Just mkLogFile -> Just (mkLogFile compiler platform pkgid ipkgid)

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
    configChanged <- checkFileMonitorChanged configFileMonitor srcdir pkgconfig
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
          setup configureCommand' configureFlags []

        -- Build phase
        when isParallelBuild $
          notice verbosity $ "Building " ++ display pkgid ++ "..."
        setup buildCommand' buildFlags buildArgs

        -- Register locally
        mipkg <- case configChanged of
          Unchanged (mipkg, _) -> return mipkg
          Changed   _
            | pkgRequiresRegistration pkg -> do
                ipkg <- generateInstalledPackageInfo
                -- We register ourselves rather than via Setup.hs. We need to
                -- grab and modify the InstalledPackageInfo. We decide what
                -- the installed package id is, not the build system.
                let ipkg' = ipkg { Installed.installedComponentId = ipkgid }
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
                            pkgconfig mipkg

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

    pkgconfig = ignoreBuildTargets rpkg
      where
        ignoreBuildTargets (ReadyPackage p d) =
          ReadyPackage (p { pkgBuildTargets = [] }) d

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
    buildArgs        = setupHsBuildArgs  pkg

    scriptOptions    = setupHsScriptOptions rpkg pkgshared
                                            srcdir builddir
                                            isParallelBuild cacheLock

    setup :: CommandUI flags -> (Version -> flags) -> [String] -> IO ()
    setup cmd flags args =
      setupWrapper verbosity
                   scriptOptions
                   (Just (pkgDescription pkg))
                   cmd flags args

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
        setup Cabal.registerCommand registerFlags []


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

