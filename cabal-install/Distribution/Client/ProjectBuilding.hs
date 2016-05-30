{-# LANGUAGE CPP, BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

-- | 
--
module Distribution.Client.ProjectBuilding (
    BuildStatus(..),
    BuildStatusMap,
    BuildStatusRebuild(..),
    BuildReason(..),
    MonitorChangedReason(..),
    rebuildTargetsDryRun,
    rebuildTargets
  ) where

import           Distribution.Client.PackageHash (renderPackageHashInputs)
import           Distribution.Client.RebuildMonad
import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanning

import           Distribution.Client.Types
                   ( PackageLocation(..), GenericReadyPackage(..)
                   , InstalledPackageId, installedPackageId )
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan, GenericPlanPackage )
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.DistDirLayout
import           Distribution.Client.FileMonitor
import           Distribution.Client.SetupWrapper
import           Distribution.Client.JobControl
import           Distribution.Client.FetchUtils
import           Distribution.Client.GlobalFlags (RepoContext)
import qualified Distribution.Client.Tar as Tar
import           Distribution.Client.Setup (filterConfigureFlags)
import           Distribution.Client.SrcDist (allPackageSourceFiles)
import           Distribution.Client.Utils (removeExistingFile)

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import           Distribution.Solver.Types.PackageFixedDeps

import           Distribution.Package hiding (InstalledPackageId, installedPackageId)
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import           Distribution.Simple.Program
import qualified Distribution.Simple.Setup as Cabal
import           Distribution.Simple.Command (CommandUI)
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.InstallDirs as InstallDirs
import           Distribution.Simple.LocalBuildInfo (ComponentName)
import qualified Distribution.Simple.Program.HcPkg as HcPkg

import           Distribution.Simple.Utils hiding (matchFileGlob)
import           Distribution.Version
import           Distribution.Verbosity
import           Distribution.Text
import           Distribution.ParseUtils ( showPWarning )

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LBS

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad
import           Control.Exception
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.List
import           Data.Maybe

import           System.FilePath
import           System.IO
import           System.Directory
import           System.Exit (ExitCode)


------------------------------------------------------------------------------
-- * Overall building strategy.
------------------------------------------------------------------------------
--
-- We start with an 'ElaboratedInstallPlan' that has already been improved by
-- reusing packages from the store. So the remaining packages in the
-- 'InstallPlan.Configured' state are ones we either need to build or rebuild.
--
-- First, we do a preliminary dry run phase where we work out which packages
-- we really need to (re)build, and for the ones we do need to build which
-- build phase to start at.


------------------------------------------------------------------------------
-- * Dry run: what bits of the 'ElaboratedInstallPlan' will we execute?
------------------------------------------------------------------------------

-- We split things like this for a couple reasons. Firstly we need to be able
-- to do dry runs, and these need to be reasonably accurate in terms of
-- letting users know what (and why) things are going to be (re)built.
--
-- Given that we need to be able to do dry runs, it would not be great if
-- we had to repeat all the same work when we do it for real. Not only is
-- it duplicate work, but it's duplicate code which is likely to get out of
-- sync. So we do things only once. We preserve info we discover in the dry
-- run phase and rely on it later when we build things for real. This also
-- somewhat simplifies the build phase. So this way the dry run can't so
-- easily drift out of sync with the real thing since we're relying on the
-- info it produces.
--
-- An additional advantage is that it makes it easier to debug rebuild
-- errors (ie rebuilding too much or too little), since all the rebuild
-- decisions are made without making any state changes at the same time
-- (that would make it harder to reproduce the problem sitation).


-- | The 'BuildStatus' of every package in the 'ElaboratedInstallPlan'
--
type BuildStatusMap = Map InstalledPackageId BuildStatus

-- | The build status for an individual package. That is, the state that the
-- package is in prior to initiating a (re)build.
--
-- It serves two purposes:
--
--  * For dry-run output, it lets us explain to the user if and why a package
--    is going to be (re)built.
--
--  * It tell us what step to start or resume building from, and carries
--    enough information for us to be able to do so.
--
data BuildStatus =

     -- | The package is in the 'InstallPlan.PreExisting' state, so does not
     --   need building.
     BuildStatusPreExisting

     -- | The package has not been downloaded yet, so it will have to be
     --   downloaded, unpacked and built.
   | BuildStatusDownload

     -- | The package has not been unpacked yet, so it will have to be
     --   unpacked and built.
   | BuildStatusUnpack FilePath

     -- | The package exists in a local dir already, and just needs building
     --   or rebuilding. So this can only happen for 'BuildInplaceOnly' style
     --   packages.
   | BuildStatusRebuild FilePath BuildStatusRebuild

     -- | The package exists in a local dir already, and is fully up to date.
     --   So this package can be put into the 'InstallPlan.Installed' state
     --   and it does not need to be built.
   | BuildStatusUpToDate [InstalledPackageInfo] BuildSuccess

-- | For a package that is going to be built or rebuilt, the state it's in now.
--
-- So again, this tells us why a package needs to be rebuilt and what build
-- phases need to be run. The 'MonitorChangedReason' gives us details like
-- which file changed, which is mainly for high verbosity debug output.
--
data BuildStatusRebuild =

     -- | The package configuration changed, so the configure and build phases
     --   needs to be (re)run.
     BuildStatusConfigure (MonitorChangedReason ())

     -- | The configuration has not changed but the build phase needs to be
     -- rerun. We record the reason the (re)build is needed.
     --
     -- The optional registration info here tells us if we've registered the
     -- package already, or if we stil need to do that after building.
     --
   | BuildStatusBuild (Maybe [InstalledPackageInfo]) BuildReason

data BuildReason =
     -- | The depencencies of this package have been (re)built so the build
     -- phase needs to be rerun.
     --
     -- The optional registration info here tells us if we've registered the
     -- package already, or if we stil need to do that after building.
     --
     BuildReasonDepsRebuilt

     -- | Changes in files within the package (or first run or corrupt cache) 
   | BuildReasonFilesChanged (MonitorChangedReason ())

     -- | An important special case is that no files have changed but the
     -- set of components the /user asked to build/ has changed. We track the
     -- set of components /we have built/, which of course only grows (until
     -- some other change resets it).
     --
     -- The @Set 'ComponentName'@ is the set of components we have built
     -- previously. When we update the monitor we take the union of the ones
     -- we have built previously with the ones the user has asked for this
     -- time and save those. See 'updatePackageBuildFileMonitor'.
     --
   | BuildReasonExtraTargets (Set ComponentName)

     -- | Although we're not going to build any additional targets as a whole,
     -- we're going to build some part of a component or run a repl or any
     -- other action that does not result in additional persistent artifacts.
     -- 
   | BuildReasonEphemeralTargets

-- | Which 'BuildStatus' values indicate we'll have to do some build work of
-- some sort. In particular we use this as part of checking if any of a
-- package's deps have changed.
--
buildStatusRequiresBuild :: BuildStatus -> Bool
buildStatusRequiresBuild BuildStatusPreExisting = False
buildStatusRequiresBuild BuildStatusUpToDate {} = False
buildStatusRequiresBuild _                      = True

-- | Do the dry run pass. This is a prerequisite of 'rebuildTargets'.
--
-- It gives us the 'BuildStatusMap' and also gives us an improved version of
-- the 'ElaboratedInstallPlan' with packages switched to the
-- 'InstallPlan.Installed' state when we find that they're already up to date.
--
rebuildTargetsDryRun :: DistDirLayout
                     -> ElaboratedInstallPlan
                     -> IO (ElaboratedInstallPlan, BuildStatusMap)
rebuildTargetsDryRun distDirLayout@DistDirLayout{..} = \installPlan -> do

    -- Do the various checks to work out the 'BuildStatus' of each package
    pkgsBuildStatus <- foldMInstallPlanDepOrder installPlan dryRunPkg

    -- For 'BuildStatusUpToDate' packages, improve the plan by marking them as
    -- 'InstallPlan.Installed'.
    let installPlan' = improveInstallPlanWithUpToDatePackages
                         installPlan pkgsBuildStatus

    return (installPlan', pkgsBuildStatus)
  where
    dryRunPkg :: ElaboratedPlanPackage
              -> ComponentDeps [BuildStatus]
              -> IO BuildStatus
    dryRunPkg (InstallPlan.PreExisting _pkg) _depsBuildStatus =
      return BuildStatusPreExisting

    dryRunPkg (InstallPlan.Configured pkg) depsBuildStatus = do
      mloc <- checkFetched (pkgSourceLocation pkg)
      case mloc of
        Nothing -> return BuildStatusDownload

        Just (LocalUnpackedPackage srcdir) ->
          -- For the case of a user-managed local dir, irrespective of the
          -- build style, we build from that directory and put build
          -- artifacts under the shared dist directory.
          dryRunLocalPkg pkg depsBuildStatus srcdir

        -- The three tarball cases are handled the same as each other,
        -- though depending on the build style.
        Just (LocalTarballPackage    tarball) ->
          dryRunTarballPkg pkg depsBuildStatus tarball

        Just (RemoteTarballPackage _ tarball) ->
          dryRunTarballPkg pkg depsBuildStatus tarball

        Just (RepoTarballPackage _ _ tarball) ->
          dryRunTarballPkg pkg depsBuildStatus tarball

    dryRunPkg (InstallPlan.Processing {}) _ = unexpectedState
    dryRunPkg (InstallPlan.Installed  {}) _ = unexpectedState
    dryRunPkg (InstallPlan.Failed     {}) _ = unexpectedState

    unexpectedState = error "rebuildTargetsDryRun: unexpected package state"

    dryRunTarballPkg :: ElaboratedConfiguredPackage
                     -> ComponentDeps [BuildStatus]
                     -> FilePath
                     -> IO BuildStatus
    dryRunTarballPkg pkg depsBuildStatus tarball =
      case pkgBuildStyle pkg of
        BuildAndInstall  -> return (BuildStatusUnpack tarball)
        BuildInplaceOnly -> do
          -- TODO: [nice to have] use a proper file monitor rather than this dir exists test
          exists <- doesDirectoryExist srcdir
          if exists
            then dryRunLocalPkg pkg depsBuildStatus srcdir
            else return (BuildStatusUnpack tarball)
      where
        srcdir = distUnpackedSrcDirectory (packageId pkg)

    dryRunLocalPkg :: ElaboratedConfiguredPackage
                   -> ComponentDeps [BuildStatus]
                   -> FilePath
                   -> IO BuildStatus
    dryRunLocalPkg pkg depsBuildStatus srcdir = do
        -- Go and do lots of I/O, reading caches and probing files to work out
        -- if anything has changed
        change <- checkPackageFileMonitorChanged
                    packageFileMonitor pkg srcdir depsBuildStatus
        case change of
          -- It did change, giving us 'BuildStatusRebuild' info on why
          Left rebuild ->
            return (BuildStatusRebuild srcdir rebuild)

          -- No changes, the package is up to date. Use the saved build results.
          Right (ipkgs, buildSuccess) ->
            return (BuildStatusUpToDate ipkgs buildSuccess)
      where
        packageFileMonitor =
          newPackageFileMonitor distDirLayout (packageId pkg)


-- | A specialised traversal over the packages in an install plan.
--
-- The packages are visited in dependency order, starting with packages with no
-- depencencies. The result for each package is accumulated into a 'Map' and
-- returned as the final result. In addition, when visting a package, the
-- visiting function is passed the results for all the immediate package
-- depencencies. This can be used to propagate information from depencencies.
--
foldMInstallPlanDepOrder
  :: forall m ipkg srcpkg iresult ifailure b.
     (Monad m,
      HasUnitId ipkg,   PackageFixedDeps ipkg,
      HasUnitId srcpkg, PackageFixedDeps srcpkg)
  => GenericInstallPlan ipkg srcpkg iresult ifailure
  -> (GenericPlanPackage ipkg srcpkg iresult ifailure ->
      ComponentDeps [b] -> m b)
  -> m (Map InstalledPackageId b)
foldMInstallPlanDepOrder plan0 visit =
    go Map.empty (InstallPlan.reverseTopologicalOrder plan0)
  where
    go :: Map InstalledPackageId b
       -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
       -> m (Map InstalledPackageId b)
    go !results [] = return results

    go !results (pkg : pkgs) = do
      -- we go in the right order so the results map has entries for all deps
      let depresults :: ComponentDeps [b]
          depresults =
            fmap (map (\ipkgid -> let Just result = Map.lookup ipkgid results
                                   in result))
                 (depends pkg)
      result <- visit pkg depresults
      let results' = Map.insert (installedPackageId pkg) result results
      go results' pkgs

improveInstallPlanWithUpToDatePackages :: ElaboratedInstallPlan
                                       -> BuildStatusMap
                                       -> ElaboratedInstallPlan
improveInstallPlanWithUpToDatePackages installPlan pkgsBuildStatus =
    replaceWithPreInstalled installPlan
      [ (installedPackageId pkg, ipkgs, buildSuccess)
      | InstallPlan.Configured pkg
          <- InstallPlan.reverseTopologicalOrder installPlan
      , let ipkgid = installedPackageId pkg
            Just pkgBuildStatus = Map.lookup ipkgid pkgsBuildStatus
      , BuildStatusUpToDate ipkgs buildSuccess <- [pkgBuildStatus]
      ]
  where
    replaceWithPreInstalled =
      foldl' (\plan (ipkgid, ipkgs, buildSuccess) ->
                InstallPlan.preinstalled ipkgid
                    (find (\ipkg -> installedPackageId ipkg == ipkgid) ipkgs)
                    buildSuccess plan)


-----------------------------
-- Package change detection
--

-- | As part of the dry run for local unpacked packages we have to check if the
-- package config or files have changed. That is the purpose of
-- 'PackageFileMonitor' and 'checkPackageFileMonitorChanged'.
--
-- When a package is (re)built, the monitor must be updated to reflect the new
-- state of the package. Because we sometimes build without reconfiguring the
-- state updates are split into two, one for package config changes and one
-- for other changes. This is the purpose of 'updatePackageConfigFileMonitor'
-- and 'updatePackageBuildFileMonitor'.
--
data PackageFileMonitor = PackageFileMonitor {
       pkgFileMonitorConfig :: FileMonitor ElaboratedConfiguredPackage (),
       pkgFileMonitorBuild  :: FileMonitor (Set ComponentName) BuildSuccess,
       pkgFileMonitorReg    :: FileMonitor () [InstalledPackageInfo]
     }

newPackageFileMonitor :: DistDirLayout -> PackageId -> PackageFileMonitor
newPackageFileMonitor DistDirLayout{distPackageCacheFile} pkgid =
    PackageFileMonitor {
      pkgFileMonitorConfig =
        newFileMonitor (distPackageCacheFile pkgid "config"),

      pkgFileMonitorBuild =
        FileMonitor {
          fileMonitorCacheFile = distPackageCacheFile pkgid "build",
          fileMonitorKeyValid  = \componentsToBuild componentsAlreadyBuilt ->
            componentsToBuild `Set.isSubsetOf` componentsAlreadyBuilt,
          fileMonitorCheckIfOnlyValueChanged = True
        },

      pkgFileMonitorReg =
        newFileMonitor (distPackageCacheFile pkgid "registration")
    }

-- | Helper function for 'checkPackageFileMonitorChanged',
-- 'updatePackageConfigFileMonitor' and 'updatePackageBuildFileMonitor'.
--
-- It selects the info from a 'ElaboratedConfiguredPackage' that are used by
-- the 'FileMonitor's (in the 'PackageFileMonitor') to detect value changes.
--
packageFileMonitorKeyValues :: ElaboratedConfiguredPackage
                            -> (ElaboratedConfiguredPackage, Set ComponentName)
packageFileMonitorKeyValues pkg =
    (pkgconfig, buildComponents)
  where
    -- The first part is the value used to guard (re)configuring the package.
    -- That is, if this value changes then we will reconfigure.
    -- The ElaboratedConfiguredPackage consists mostly (but not entirely) of
    -- information that affects the (re)configure step. But those parts that
    -- do not affect the configure step need to be nulled out. Those parts are
    -- the specific targets that we're going to build.
    --
    pkgconfig = pkg {
      pkgBuildTargets  = [],
      pkgReplTarget    = Nothing,
      pkgBuildHaddocks = False
    }

    -- The second part is the value used to guard the build step. So this is
    -- more or less the opposite of the first part, as it's just the info about
    -- what targets we're going to build.
    --
    buildComponents = pkgBuildTargetWholeComponents pkg

-- | Do all the checks on whether a package has changed and thus needs either
-- rebuilding or reconfiguring and rebuilding.
--
checkPackageFileMonitorChanged :: PackageFileMonitor
                               -> ElaboratedConfiguredPackage
                               -> FilePath
                               -> ComponentDeps [BuildStatus]
                               -> IO (Either BuildStatusRebuild
                                            ([InstalledPackageInfo],
                                             BuildSuccess))
checkPackageFileMonitorChanged PackageFileMonitor{..}
                               pkg srcdir depsBuildStatus = do
    --TODO: [nice to have] some debug-level message about file changes, like rerunIfChanged
    configChanged <- checkFileMonitorChanged
                       pkgFileMonitorConfig srcdir pkgconfig
    case configChanged of
      MonitorChanged monitorReason ->
          return (Left (BuildStatusConfigure monitorReason'))
        where
          monitorReason' = fmap (const ()) monitorReason

      MonitorUnchanged () _
          -- The configChanged here includes the identity of the dependencies,
          -- so depsBuildStatus is just needed for the changes in the content
          -- of depencencies.
        | any buildStatusRequiresBuild (CD.flatDeps depsBuildStatus) -> do
            regChanged <- checkFileMonitorChanged pkgFileMonitorReg srcdir ()
            let mreg = changedToMaybe regChanged
            return (Left (BuildStatusBuild mreg BuildReasonDepsRebuilt))

        | otherwise -> do
            buildChanged  <- checkFileMonitorChanged
                               pkgFileMonitorBuild srcdir buildComponents
            regChanged    <- checkFileMonitorChanged
                               pkgFileMonitorReg srcdir ()
            let mreg = changedToMaybe regChanged
            case (buildChanged, regChanged) of
              (MonitorChanged (MonitoredValueChanged prevBuildComponents), _) ->
                  return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason = BuildReasonExtraTargets prevBuildComponents

              (MonitorChanged monitorReason, _) ->
                  return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason    = BuildReasonFilesChanged monitorReason'
                  monitorReason' = fmap (const ()) monitorReason

              (MonitorUnchanged _ _, MonitorChanged monitorReason) ->
                -- this should only happen if the file is corrupt or been
                -- manually deleted. We don't want to bother with another
                -- phase just for this, so we'll reregister by doing a build.
                  return (Left (BuildStatusBuild Nothing buildReason))
                where
                  buildReason    = BuildReasonFilesChanged monitorReason'
                  monitorReason' = fmap (const ()) monitorReason

              (MonitorUnchanged _ _, MonitorUnchanged _ _)
                | pkgHasEphemeralBuildTargets pkg ->
                  return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason = BuildReasonEphemeralTargets

              (MonitorUnchanged buildSuccess _, MonitorUnchanged ipkgs _) ->
                return (Right (ipkgs, buildSuccess))
  where
    (pkgconfig, buildComponents) = packageFileMonitorKeyValues pkg
    changedToMaybe (MonitorChanged     _) = Nothing
    changedToMaybe (MonitorUnchanged x _) = Just x


updatePackageConfigFileMonitor :: PackageFileMonitor
                               -> FilePath
                               -> ElaboratedConfiguredPackage
                               -> IO ()
updatePackageConfigFileMonitor PackageFileMonitor{pkgFileMonitorConfig}
                               srcdir pkg =
    updateFileMonitor pkgFileMonitorConfig srcdir Nothing
                      [] pkgconfig ()
  where
    (pkgconfig, _buildComponents) = packageFileMonitorKeyValues pkg

updatePackageBuildFileMonitor :: PackageFileMonitor
                              -> FilePath
                              -> MonitorTimestamp
                              -> ElaboratedConfiguredPackage
                              -> BuildStatusRebuild
                              -> [FilePath]
                              -> BuildSuccess
                              -> IO ()
updatePackageBuildFileMonitor PackageFileMonitor{pkgFileMonitorBuild}
                              srcdir timestamp pkg pkgBuildStatus
                              allSrcFiles buildSuccess =
    updateFileMonitor pkgFileMonitorBuild srcdir (Just timestamp)
                      (map monitorFileHashed allSrcFiles)
                      buildComponents' buildSuccess
  where
    (_pkgconfig, buildComponents) = packageFileMonitorKeyValues pkg

    -- If the only thing that's changed is that we're now building extra
    -- components, then we can avoid later unnecessary rebuilds by saving the
    -- total set of components that have been built, namely the union of the
    -- existing ones plus the new ones. If files also changed this would be
    -- the wrong thing to do. Note that we rely on the
    -- fileMonitorCheckIfOnlyValueChanged = True mode to get this guarantee
    -- that it's /only/ the value that changed not any files that changed.
    buildComponents' =
      case pkgBuildStatus of
        BuildStatusBuild _ (BuildReasonExtraTargets prevBuildComponents)
          -> buildComponents `Set.union` prevBuildComponents
        _ -> buildComponents

updatePackageRegFileMonitor :: PackageFileMonitor
                            -> FilePath
                            -> [InstalledPackageInfo]
                            -> IO ()
updatePackageRegFileMonitor PackageFileMonitor{pkgFileMonitorReg}
                            srcdir ipkgs =
    updateFileMonitor pkgFileMonitorReg srcdir Nothing
                      [] () ipkgs

invalidatePackageRegFileMonitor :: PackageFileMonitor -> IO ()
invalidatePackageRegFileMonitor PackageFileMonitor{pkgFileMonitorReg} =
    removeExistingFile (fileMonitorCacheFile pkgFileMonitorReg)


------------------------------------------------------------------------------
-- * Doing it: executing an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------


-- | Build things for real.
--
-- It requires the 'BuildStatusMap' gathered by 'rebuildTargetsDryRun'.
--
rebuildTargets :: Verbosity
               -> DistDirLayout
               -> ElaboratedInstallPlan
               -> ElaboratedSharedConfig
               -> BuildStatusMap
               -> BuildTimeSettings
               -> IO ElaboratedInstallPlan
rebuildTargets verbosity
               distDirLayout@DistDirLayout{..}
               installPlan
               sharedPackageConfig
               pkgsBuildStatus
               buildSettings@BuildTimeSettings{buildSettingNumJobs} = do

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
    asyncDownloadPackages verbosity withRepoCtx
                          installPlan pkgsBuildStatus $ \downloadMap ->

      -- For each package in the plan, in dependency order, but in parallel...
      executeInstallPlan verbosity jobControl installPlan $ \pkg ->
        handle (return . BuildFailure) $ --TODO: review exception handling

        let ipkgid = installedPackageId pkg
            Just pkgBuildStatus = Map.lookup ipkgid pkgsBuildStatus in

        rebuildTarget
          verbosity
          distDirLayout
          buildSettings downloadMap
          buildLimit installLock cacheLock 
          sharedPackageConfig
          pkg
          pkgBuildStatus
  where
    isParallelBuild = buildSettingNumJobs >= 2
    withRepoCtx     = projectConfigWithBuilderRepoContext verbosity 
                        buildSettings

-- | Given all the context and resources, (re)build an individual package.
--
rebuildTarget :: Verbosity
              -> DistDirLayout
              -> BuildTimeSettings
              -> AsyncDownloadMap
              -> JobLimit -> Lock -> Lock
              -> ElaboratedSharedConfig
              -> ElaboratedReadyPackage
              -> BuildStatus
              -> IO BuildResult
rebuildTarget verbosity
              distDirLayout@DistDirLayout{distBuildDirectory}
              buildSettings downloadMap
              buildLimit installLock cacheLock
              sharedPackageConfig
              rpkg@(ReadyPackage pkg)
              pkgBuildStatus =

    -- We rely on the 'BuildStatus' to decide which phase to start from:
    case pkgBuildStatus of
      BuildStatusDownload              -> downloadPhase
      BuildStatusUnpack tarball        -> unpackTarballPhase tarball
      BuildStatusRebuild srcdir status -> rebuildPhase status srcdir

      -- TODO: perhaps re-nest the types to make these impossible
      BuildStatusPreExisting {} -> unexpectedState
      BuildStatusUpToDate    {} -> unexpectedState
  where
    unexpectedState = error "rebuildTarget: unexpected package status"

    downloadPhase = do
        downsrcloc <- waitAsyncPackageDownload verbosity downloadMap pkg
        case downsrcloc of
          DownloadedTarball tarball -> unpackTarballPhase tarball
          --TODO: [nice to have] git/darcs repos etc


    unpackTarballPhase tarball =
      withJobLimit buildLimit $
        withTarballLocalDirectory
          verbosity distDirLayout tarball
          (packageId pkg) (pkgBuildStyle pkg)
          (pkgDescriptionOverride pkg) $

          case pkgBuildStyle pkg of
            BuildAndInstall  -> buildAndInstall
            BuildInplaceOnly -> buildInplace buildStatus
              where
                buildStatus = BuildStatusConfigure MonitorFirstRun

    -- Note that this really is rebuild, not build. It can only happen for
    -- 'BuildInplaceOnly' style packages. 'BuildAndInstall' style packages
    -- would only start from download or unpack phases.
    --
    rebuildPhase buildStatus srcdir =
        assert (pkgBuildStyle pkg == BuildInplaceOnly) $

        withJobLimit buildLimit $
          buildInplace buildStatus srcdir builddir
      where
        builddir = distBuildDirectory (packageId pkg)

    buildAndInstall srcdir builddir =
        buildAndInstallUnpackedPackage
          verbosity distDirLayout
          buildSettings installLock cacheLock
          sharedPackageConfig
          rpkg
          srcdir builddir'
      where
        builddir' = makeRelative srcdir builddir
        --TODO: [nice to have] ^^ do this relative stuff better

    buildInplace buildStatus srcdir builddir =
        --TODO: [nice to have] use a relative build dir rather than absolute
        buildInplaceUnpackedPackage
          verbosity distDirLayout
          buildSettings cacheLock
          sharedPackageConfig
          rpkg
          buildStatus
          srcdir builddir

--TODO: [nice to have] do we need to use a with-style for the temp files for downloading http
-- packages, or are we going to cache them persistently?

type AsyncDownloadMap = Map (PackageLocation (Maybe FilePath))
                            (MVar DownloadedSourceLocation)

data DownloadedSourceLocation = DownloadedTarball FilePath
                              --TODO: [nice to have] git/darcs repos etc

downloadedSourceLocation :: PackageLocation FilePath
                         -> Maybe DownloadedSourceLocation
downloadedSourceLocation pkgloc =
    case pkgloc of
      RemoteTarballPackage _ tarball -> Just (DownloadedTarball tarball)
      RepoTarballPackage _ _ tarball -> Just (DownloadedTarball tarball)
      _                              -> Nothing

-- | Given the current 'InstallPlan' and 'BuildStatusMap', select all the
-- packages we have to download and fork off an async action to download them.
-- We download them in dependency order so that the one's we'll need
-- first are the ones we will start downloading first.
--
-- The body action is passed a map from those packages (identified by their
-- location) to a completion var for that package. So the body action should
-- lookup the location and use 'waitAsyncPackageDownload' to get the result.
--
asyncDownloadPackages :: Verbosity
                      -> ((RepoContext -> IO ()) -> IO ())
                      -> ElaboratedInstallPlan
                      -> BuildStatusMap
                      -> (AsyncDownloadMap -> IO a)
                      -> IO a
asyncDownloadPackages verbosity withRepoCtx installPlan pkgsBuildStatus body
  | null pkgsToDownload = body Map.empty
  | otherwise = do
    --TODO: [research required] use parallel downloads? if so, use the fetchLimit

    asyncDownloadVars <- mapM (\loc -> (,) loc <$> newEmptyMVar) pkgsToDownload

    let downloadAction :: IO ()
        downloadAction =
          withRepoCtx $ \repoctx ->
            forM_ asyncDownloadVars $ \(pkgloc, var) -> do
              Just scrloc <- downloadedSourceLocation <$>
                             fetchPackage verbosity repoctx pkgloc
              putMVar var scrloc

    withAsync downloadAction $ \_ ->
      body (Map.fromList asyncDownloadVars)
  where
    pkgsToDownload = 
      [ pkgSourceLocation pkg
      | InstallPlan.Configured pkg
         <- InstallPlan.reverseTopologicalOrder installPlan
      , let ipkgid = installedPackageId pkg
            Just pkgBuildStatus = Map.lookup ipkgid pkgsBuildStatus
      , BuildStatusDownload <- [pkgBuildStatus]
      ]


-- | Check if a package needs downloading, and if so expect to find a download
-- in progress in the given 'AsyncDownloadMap' and wait on it to finish.
--
waitAsyncPackageDownload :: Verbosity
                         -> AsyncDownloadMap
                         -> ElaboratedConfiguredPackage
                         -> IO DownloadedSourceLocation
waitAsyncPackageDownload verbosity downloadMap pkg =
    case Map.lookup (pkgSourceLocation pkg) downloadMap of
      Just hnd -> do
        debug verbosity $
          "Waiting for download of " ++ display (packageId pkg) ++ " to finish"
        --TODO: [required eventually] do the exception handling on download stuff
        takeMVar hnd
      Nothing ->
        fail "waitAsyncPackageDownload: package not being download"


executeInstallPlan
  :: forall ipkg srcpkg iresult.
     (HasUnitId ipkg,   PackageFixedDeps ipkg,
      HasUnitId srcpkg, PackageFixedDeps srcpkg)
  => Verbosity
  -> JobControl IO ( GenericReadyPackage srcpkg
                   , GenericBuildResult ipkg iresult BuildFailure )
  -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
  -> (    GenericReadyPackage srcpkg
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
                   buildResult <- installPkg pkg
                   return (pkg, buildResult)
            | pkg <- pkgs
            , let pkgid = packageId pkg
            ]

          let taskCount' = taskCount + length pkgs
              plan'      = InstallPlan.processing pkgs plan
          waitForTasks taskCount' plan'

    waitForTasks taskCount plan = do
      debug verbosity $ "Waiting for install task to finish..."
      (pkg, buildResult) <- collectJob jobCtl
      let taskCount' = taskCount-1
          plan'      = updatePlan pkg buildResult plan
      tryNewTasks taskCount' plan'

    updatePlan :: GenericReadyPackage srcpkg
               -> GenericBuildResult ipkg iresult BuildFailure
               -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
               -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
    updatePlan pkg (BuildSuccess ipkgs buildSuccess) =
        InstallPlan.completed (installedPackageId pkg)
            mipkg
            buildSuccess
      where
        mipkg = case (ipkgs, find (\ipkg -> installedPackageId ipkg
                                         == installedPackageId pkg) ipkgs) of
          ([],    _)         -> Nothing
          ((_:_), Just ipkg) -> Just ipkg
          ((_:_), Nothing)   ->
            error $ "executeInstallPlan: package " ++ display (packageId pkg)
                 ++ " was expected to register the unit "
                 ++ display (installedPackageId pkg)
                 ++ " but is actually registering the unit(s) "
                 ++ intercalate ", " (map (display . installedPackageId) ipkgs)

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
withTarballLocalDirectory
  :: Verbosity
  -> DistDirLayout
  -> FilePath
  -> PackageId
  -> BuildStyle
  -> Maybe CabalFileText
  -> (FilePath -> FilePath -> IO a)
  -> IO a
withTarballLocalDirectory verbosity distDirLayout@DistDirLayout{..}
                          tarball pkgid buildstyle pkgTextOverride
                          buildPkg  =
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
                                 pkgConfigPlatform      = platform,
                                 pkgConfigCompiler      = compiler,
                                 pkgConfigCompilerProgs = progdb
                               }
                               rpkg@(ReadyPackage pkg)
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
      setup configureCommand configureFlags

    -- Build phase
    when isParallelBuild $
      notice verbosity $ "Building " ++ display pkgid ++ "..."
    annotateFailure BuildFailed $
      setup buildCommand buildFlags

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

      if pkgRequiresRegistration pkg
        then do
          ipkgs <- generateInstalledPackageInfos
          -- We register ourselves rather than via Setup.hs. We need to
          -- grab and modify the InstalledPackageInfo. We decide what
          -- the installed package id is, not the build system.

          -- See Note [Updating installedUnitId]
          let ipkgs' = case ipkgs of
                          -- Case A and B
                          [ipkg] -> [ipkg { Installed.installedUnitId = ipkgid }]
                          -- Case C
                          _      -> ipkgs
          unless (any ((== ipkgid) . Installed.installedUnitId) ipkgs') $
            die $ "the package " ++ display (packageId pkg) ++ " was expected "
               ++ " to produce registeration info for the unit Id "
               ++ display ipkgid ++ " but it actually produced info for "
               ++ intercalate ", "
                    (map (display . Installed.installedUnitId) ipkgs')
          forM_ ipkgs' $ \ipkg' ->
              Cabal.registerPackage verbosity compiler progdb
                                    HcPkg.MultiInstance
                                    (pkgRegisterPackageDBStack pkg) ipkg'
          return ipkgs'
        else return []

    --TODO: [required feature] docs and test phases
    let docsResult  = DocsNotTried
        testsResult = TestsNotTried

    return (BuildSuccess mipkg (BuildOk docsResult testsResult))

  where
    pkgid  = packageId rpkg
    ipkgid = installedPackageId rpkg

    isParallelBuild = buildSettingNumJobs >= 2

    configureCommand = Cabal.configureCommand defaultProgramConfiguration
    configureFlags v = flip filterConfigureFlags v $
                       setupHsConfigureFlags rpkg pkgshared
                                             verbosity builddir

    buildCommand     = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = setupHsBuildFlags pkg pkgshared verbosity builddir

    generateInstalledPackageInfos :: IO [InstalledPackageInfo]
    generateInstalledPackageInfos =
      withTempInstalledPackageInfoFiles
        verbosity distTempDirectory $ \pkgConfDest -> do
        let registerFlags _ = setupHsRegisterFlags
                                pkg pkgshared
                                verbosity builddir
                                pkgConfDest
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
                            -> ElaboratedReadyPackage
                            -> BuildStatusRebuild
                            -> FilePath -> FilePath
                            -> IO BuildResult
buildInplaceUnpackedPackage verbosity
                            distDirLayout@DistDirLayout {
                              distTempDirectory,
                              distPackageCacheDirectory
                            }
                            BuildTimeSettings{buildSettingNumJobs}
                            cacheLock
                            pkgshared@ElaboratedSharedConfig {
                              pkgConfigCompiler      = compiler,
                              pkgConfigCompilerProgs = progdb
                            }
                            rpkg@(ReadyPackage pkg)
                            buildStatus
                            srcdir builddir = do

        --TODO: [code cleanup] there is duplication between the distdirlayout and the builddir here
        --      builddir is not enough, we also need the per-package cachedir
        createDirectoryIfMissingVerbose verbosity False builddir
        createDirectoryIfMissingVerbose verbosity False (distPackageCacheDirectory pkgid)
        createPackageDBIfMissing verbosity compiler progdb (pkgBuildPackageDBStack pkg)

        -- Configure phase
        --
        whenReConfigure $ do
          annotateFailure ConfigureFailed $
            setup configureCommand configureFlags []
          invalidatePackageRegFileMonitor packageFileMonitor
          updatePackageConfigFileMonitor packageFileMonitor srcdir pkg

        -- Build phase
        --
        let docsResult  = DocsNotTried
            testsResult = TestsNotTried

            buildSuccess :: BuildSuccess
            buildSuccess = BuildOk docsResult testsResult

        whenRebuild $ do
          timestamp <- beginUpdateFileMonitor
          annotateFailure BuildFailed $
            setup buildCommand buildFlags buildArgs

          --TODO: [required eventually] this doesn't track file
          --non-existence, so we could fail to rebuild if someone
          --adds a new file which changes behavior.
          allSrcFiles <- allPackageSourceFiles verbosity srcdir

          updatePackageBuildFileMonitor packageFileMonitor srcdir timestamp
                                        pkg buildStatus
                                        allSrcFiles buildSuccess

        ipkgs <- whenReRegister $ annotateFailure InstallFailed $ do
          -- Register locally
          ipkgs <- if pkgRequiresRegistration pkg
            then do
                ipkgs <- generateInstalledPackageInfos
                -- We register ourselves rather than via Setup.hs. We need to
                -- grab and modify the InstalledPackageInfo. We decide what
                -- the installed package id is, not the build system.

                -- Note [Updating installedUnitId]
                -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                -- This is a bit tricky.  There are three variables we
                -- care about:
                --
                --      1. Does the Setup script we're interfacing with
                --         support --ipid?  (Only if version >= 1.23)
                --         If not, we have to explicitly update the
                --         the UID that was recorded.
                --
                --      2. Does the Setup script we're interfacing with
                --         support internal libraries?  (Only if
                --         version >= 1.25).  If so, there may be
                --         multiple IPIs... and it would be wrong to
                --         update them all to the same UID (you need
                --         to generate derived UIDs for each
                --         subcomponent.)
                --
                --      3. Does GHC require that the IPID be input at
                --         configure time?  (Only if GHC >= 8.0, which
                --         also implies Cabal version >= 1.23, as earlier
                --         Cabal's don't know how to do this properly).
                --         If so, it is IMPERMISSIBLE to update the
                --         UID that was recorded.
                --
                -- This means that there are three situations:
                --
                --   A. Cabal  < 1.23
                --   B. Cabal >= 1.23 && < 1.25
                --   C. Cabal >= 1.25
                --
                -- We consider each in turn:
                --
                --      A. There is only ever one IPI, and we must
                --         update it.
                --
                --      B. There is only ever one IPI, but because
                --         --ipid is supported, the installedUnitId of
                --         this IPI is ipkgid (so it's harmless to
                --         overwrite).
                --
                --      C. There may be multiple IPIs, but because
                --         --ipid is supported they always have the
                --         right installedUnitIds.
                --
                let ipkgs' = case ipkgs of
                                -- Case A and B
                                [ipkg] -> [ipkg { Installed.installedUnitId = ipkgid }]
                                -- Case C
                                _      -> assert (any ((== ipkgid) . Installed.installedUnitId)
                                                      ipkgs) ipkgs
                forM_ ipkgs' $ \ipkg' ->
                    Cabal.registerPackage verbosity compiler progdb HcPkg.NoMultiInstance
                                          (pkgRegisterPackageDBStack pkg)
                                          ipkg'
                return ipkgs'

           else return []

          updatePackageRegFileMonitor packageFileMonitor srcdir ipkgs
          return ipkgs

        -- Repl phase
        --
        whenRepl $
          annotateFailure BuildFailed $
          setup replCommand replFlags replArgs

        -- Haddock phase
        whenHaddock $
          annotateFailure BuildFailed $
          setup haddockCommand haddockFlags []

        return (BuildSuccess ipkgs buildSuccess)

  where
    pkgid  = packageId rpkg
    ipkgid = installedPackageId rpkg

    isParallelBuild = buildSettingNumJobs >= 2

    packageFileMonitor = newPackageFileMonitor distDirLayout pkgid

    whenReConfigure action = case buildStatus of
      BuildStatusConfigure _ -> action
      _                      -> return ()

    whenRebuild action
      | null (pkgBuildTargets pkg) = return ()
      | otherwise                  = action

    whenRepl action
      | isNothing (pkgReplTarget pkg) = return ()
      | otherwise                     = action

    whenHaddock action
      | pkgBuildHaddocks pkg = action
      | otherwise            = return ()

    whenReRegister  action = case buildStatus of
      BuildStatusConfigure          _ -> action
      BuildStatusBuild Nothing      _ -> action
      BuildStatusBuild (Just ipkgs) _ -> return ipkgs

    configureCommand = Cabal.configureCommand defaultProgramConfiguration
    configureFlags v = flip filterConfigureFlags v $
                       setupHsConfigureFlags rpkg pkgshared
                                             verbosity builddir

    buildCommand     = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = setupHsBuildFlags pkg pkgshared
                                         verbosity builddir
    buildArgs        = setupHsBuildArgs  pkg

    replCommand      = Cabal.replCommand defaultProgramConfiguration
    replFlags _      = setupHsReplFlags pkg pkgshared
                                        verbosity builddir
    replArgs         = setupHsReplArgs  pkg

    haddockCommand   = Cabal.haddockCommand
    haddockFlags _   = setupHsHaddockFlags pkg pkgshared
                                           verbosity builddir

    scriptOptions    = setupHsScriptOptions rpkg pkgshared
                                            srcdir builddir
                                            isParallelBuild cacheLock

    setup :: CommandUI flags -> (Version -> flags) -> [String] -> IO ()
    setup cmd flags args =
      setupWrapper verbosity
                   scriptOptions
                   (Just (pkgDescription pkg))
                   cmd flags args

    generateInstalledPackageInfos :: IO [InstalledPackageInfo]
    generateInstalledPackageInfos =
      withTempInstalledPackageInfoFiles
        verbosity distTempDirectory $ \pkgConfDest -> do
        let registerFlags _ = setupHsRegisterFlags
                                pkg pkgshared
                                verbosity builddir
                                pkgConfDest
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
    handler = throwIO . annotate
#if MIN_VERSION_base(4,8,0)
            . displayException
#else
            . show
#endif


withTempInstalledPackageInfoFiles :: Verbosity -> FilePath
                                  -> (FilePath -> IO ())
                                  -> IO [InstalledPackageInfo]
withTempInstalledPackageInfoFiles verbosity tempdir action =
    withTempDirectory verbosity tempdir "package-registration-" $ \dir -> do
      -- make absolute since @action@ will often change directory
      abs_dir <- canonicalizePath dir

      let pkgConfDest = abs_dir </> "pkgConf"
      action pkgConfDest

      is_dir <- doesDirectoryExist pkgConfDest

      let notHidden = not . isHidden
          isHidden name = "." `isPrefixOf` name
      if is_dir
        then mapM (readPkgConf pkgConfDest) . sort . filter notHidden
                =<< getDirectoryContents pkgConfDest
        else fmap (:[]) $ readPkgConf "." pkgConfDest
  where
    pkgConfParseFailed :: Installed.PError -> IO a
    pkgConfParseFailed perror =
      die $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
            ++ show perror

    readPkgConf pkgConfDir pkgConfFile = do
      (warns, ipkg) <- withUTF8FileContents (pkgConfDir </> pkgConfFile) $ \pkgConfStr ->
        case Installed.parseInstalledPackageInfo pkgConfStr of
          Installed.ParseFailed perror -> pkgConfParseFailed perror
          Installed.ParseOk warns ipkg -> return (warns, ipkg)

      unless (null warns) $
        warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)

      return ipkg

