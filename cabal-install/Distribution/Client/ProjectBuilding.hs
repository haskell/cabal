{-# LANGUAGE CPP, BangPatterns, RecordWildCards, NamedFieldPuns,
             ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
--
module Distribution.Client.ProjectBuilding (
    -- * Dry run phase
    -- | What bits of the plan will we execute? The dry run does not change
    -- anything but tells us what will need to be built.
    rebuildTargetsDryRun,
    improveInstallPlanWithUpToDatePackages,

    -- ** Build status
    -- | This is the detailed status information we get from the dry run.
    BuildStatusMap,
    BuildStatus(..),
    BuildStatusRebuild(..),
    BuildReason(..),
    MonitorChangedReason(..),
    buildStatusToString,

    -- * Build phase
    -- | Now we actually execute the plan.
    rebuildTargets,
    -- ** Build outcomes
    -- | This is the outcome for each package of executing the plan.
    -- For each package, did the build succeed or fail?
    BuildOutcomes,
    BuildOutcome,
    BuildResult(..),
    BuildFailure(..),
    BuildFailureReason(..),
  ) where

import           Distribution.Client.PackageHash (renderPackageHashInputs)
import           Distribution.Client.RebuildMonad
import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanning
import           Distribution.Client.ProjectPlanning.Types
import           Distribution.Client.ProjectBuilding.Types

import           Distribution.Client.Types
                   hiding (BuildOutcomes, BuildOutcome,
                           BuildResult(..), BuildFailure(..))
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan, GenericPlanPackage, IsUnit )
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
import           Distribution.Compat.Graph (IsNode(..))

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LBS

import           Control.Monad
import           Control.Exception
import           Data.List
import           Data.Maybe

import           System.FilePath
import           System.IO
import           System.Directory


------------------------------------------------------------------------------
-- * Overall building strategy.
------------------------------------------------------------------------------
--
-- We start with an 'ElaboratedInstallPlan' that has already been improved by
-- reusing packages from the store, and pruned to include only the targets of
-- interest and their dependencies. So the remaining packages in the
-- 'InstallPlan.Configured' state are ones we either need to build or rebuild.
--
-- First, we do a preliminary dry run phase where we work out which packages
-- we really need to (re)build, and for the ones we do need to build which
-- build phase to start at.
--
-- We use this to improve the 'ElaboratedInstallPlan' again by changing
-- up-to-date 'InstallPlan.Configured' packages to 'InstallPlan.Installed'
-- so that the build phase will skip them.
--
-- Then we execute the plan, that is actually build packages. The outcomes of
-- trying to build all the packages are collected and returned.
--
-- We split things like this (dry run and execute) for a couple reasons.
-- Firstly we need to be able to do dry runs anyway, and these need to be
-- reasonably accurate in terms of letting users know what (and why) things
-- are going to be (re)built.
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
-- (that would make it harder to reproduce the problem situation).
--
-- Finally, we can use the dry run build status and the build outcomes to
-- give us some information on the overall status of packages in the project.
-- This includes limited information about the status of things that were
-- not actually in the subset of the plan that was used for the dry run or
-- execution phases. In particular we may know that some packages are now
-- definitely out of date. See "Distribution.Client.ProjectPlanOutput" for
-- details.


------------------------------------------------------------------------------
-- * Dry run: what bits of the 'ElaboratedInstallPlan' will we execute?
------------------------------------------------------------------------------

-- Refer to ProjectBuilding.Types for details of these important types:

-- type BuildStatusMap     = ...
-- data BuildStatus        = ...
-- data BuildStatusRebuild = ...
-- data BuildReason        = ...

-- | Do the dry run pass. This is a prerequisite of 'rebuildTargets'.
--
-- It gives us the 'BuildStatusMap'. This should be used with
-- 'improveInstallPlanWithUpToDatePackages' to give an improved version of
-- the 'ElaboratedInstallPlan' with packages switched to the
-- 'InstallPlan.Installed' state when we find that they're already up to date.
--
rebuildTargetsDryRun :: DistDirLayout
                     -> ElaboratedSharedConfig
                     -> ElaboratedInstallPlan
                     -> IO BuildStatusMap
rebuildTargetsDryRun distDirLayout@DistDirLayout{..} shared =
    -- Do the various checks to work out the 'BuildStatus' of each package
    foldMInstallPlanDepOrder dryRunPkg
  where
    dryRunPkg :: ElaboratedPlanPackage
              -> [BuildStatus]
              -> IO BuildStatus
    dryRunPkg (InstallPlan.PreExisting _pkg) _depsBuildStatus =
      return BuildStatusPreExisting

    dryRunPkg (InstallPlan.Installed _pkg) _depsBuildStatus =
      return BuildStatusInstalled

    dryRunPkg (InstallPlan.Configured pkg) depsBuildStatus = do
      mloc <- checkFetched (elabPkgSourceLocation pkg)
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

    dryRunTarballPkg :: ElaboratedConfiguredPackage
                     -> [BuildStatus]
                     -> FilePath
                     -> IO BuildStatus
    dryRunTarballPkg pkg depsBuildStatus tarball =
      case elabBuildStyle pkg of
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
                   -> [BuildStatus]
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
          Right buildResult ->
            return (BuildStatusUpToDate buildResult)
      where
        packageFileMonitor =
          newPackageFileMonitor distDirLayout (elabDistDirParams shared pkg)


-- | A specialised traversal over the packages in an install plan.
--
-- The packages are visited in dependency order, starting with packages with no
-- dependencies. The result for each package is accumulated into a 'Map' and
-- returned as the final result. In addition, when visting a package, the
-- visiting function is passed the results for all the immediate package
-- dependencies. This can be used to propagate information from dependencies.
--
foldMInstallPlanDepOrder
  :: forall m ipkg srcpkg b.
     (Monad m, IsUnit ipkg, IsUnit srcpkg)
  => (GenericPlanPackage ipkg srcpkg ->
      [b] -> m b)
  -> GenericInstallPlan ipkg srcpkg
  -> m (Map UnitId b)
foldMInstallPlanDepOrder visit =
    go Map.empty . InstallPlan.reverseTopologicalOrder
  where
    go :: Map UnitId b
       -> [GenericPlanPackage ipkg srcpkg]
       -> m (Map UnitId b)
    go !results [] = return results

    go !results (pkg : pkgs) = do
      -- we go in the right order so the results map has entries for all deps
      let depresults :: [b]
          depresults =
            map (\ipkgid -> let Just result = Map.lookup ipkgid results
                              in result)
                (InstallPlan.depends pkg)
      result <- visit pkg depresults
      let results' = Map.insert (nodeKey pkg) result results
      go results' pkgs

improveInstallPlanWithUpToDatePackages :: BuildStatusMap
                                       -> ElaboratedInstallPlan
                                       -> ElaboratedInstallPlan
improveInstallPlanWithUpToDatePackages pkgsBuildStatus =
    InstallPlan.installed canPackageBeImproved
  where
    canPackageBeImproved pkg =
      case Map.lookup (installedUnitId pkg) pkgsBuildStatus of
        Just BuildStatusUpToDate {} -> True
        Just _                      -> False
        Nothing -> error $ "improveInstallPlanWithUpToDatePackages: "
                        ++ display (packageId pkg) ++ " not in status map"


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
       pkgFileMonitorBuild  :: FileMonitor (Set ComponentName) BuildResultMisc,
       pkgFileMonitorReg    :: FileMonitor () (Maybe InstalledPackageInfo)
     }

-- | This is all the components of the 'BuildResult' other than the
-- @['InstalledPackageInfo']@.
--
-- We have to split up the 'BuildResult' components since they get produced
-- at different times (or rather, when different things change).
--
type BuildResultMisc = (DocsResult, TestsResult)

newPackageFileMonitor :: DistDirLayout -> DistDirParams -> PackageFileMonitor
newPackageFileMonitor DistDirLayout{distPackageCacheFile} dparams =
    PackageFileMonitor {
      pkgFileMonitorConfig =
        newFileMonitor (distPackageCacheFile dparams "config"),

      pkgFileMonitorBuild =
        FileMonitor {
          fileMonitorCacheFile = distPackageCacheFile dparams "build",
          fileMonitorKeyValid  = \componentsToBuild componentsAlreadyBuilt ->
            componentsToBuild `Set.isSubsetOf` componentsAlreadyBuilt,
          fileMonitorCheckIfOnlyValueChanged = True
        },

      pkgFileMonitorReg =
        newFileMonitor (distPackageCacheFile dparams "registration")
    }

-- | Helper function for 'checkPackageFileMonitorChanged',
-- 'updatePackageConfigFileMonitor' and 'updatePackageBuildFileMonitor'.
--
-- It selects the info from a 'ElaboratedConfiguredPackage' that are used by
-- the 'FileMonitor's (in the 'PackageFileMonitor') to detect value changes.
--
packageFileMonitorKeyValues :: ElaboratedConfiguredPackage
                            -> (ElaboratedConfiguredPackage, Set ComponentName)
packageFileMonitorKeyValues elab =
    (elab_config, buildComponents)
  where
    -- The first part is the value used to guard (re)configuring the package.
    -- That is, if this value changes then we will reconfigure.
    -- The ElaboratedConfiguredPackage consists mostly (but not entirely) of
    -- information that affects the (re)configure step. But those parts that
    -- do not affect the configure step need to be nulled out. Those parts are
    -- the specific targets that we're going to build.
    --
    elab_config =
        elab {
            elabBuildTargets = [],
            elabReplTarget    = Nothing,
            elabBuildHaddocks = False
        }

    -- The second part is the value used to guard the build step. So this is
    -- more or less the opposite of the first part, as it's just the info about
    -- what targets we're going to build.
    --
    buildComponents = elabBuildTargetWholeComponents elab

-- | Do all the checks on whether a package has changed and thus needs either
-- rebuilding or reconfiguring and rebuilding.
--
checkPackageFileMonitorChanged :: PackageFileMonitor
                               -> ElaboratedConfiguredPackage
                               -> FilePath
                               -> [BuildStatus]
                               -> IO (Either BuildStatusRebuild BuildResult)
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
          -- of dependencies.
        | any buildStatusRequiresBuild depsBuildStatus -> do
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

              (MonitorUnchanged buildResult _, MonitorUnchanged _ _) ->
                  return $ Right BuildResult {
                    buildResultDocs    = docsResult,
                    buildResultTests   = testsResult,
                    buildResultLogFile = Nothing
                  }
                where
                  (docsResult, testsResult) = buildResult
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
                              -> BuildResultMisc
                              -> IO ()
updatePackageBuildFileMonitor PackageFileMonitor{pkgFileMonitorBuild}
                              srcdir timestamp pkg pkgBuildStatus
                              allSrcFiles buildResult =
    updateFileMonitor pkgFileMonitorBuild srcdir (Just timestamp)
                      (map monitorFileHashed allSrcFiles)
                      buildComponents' buildResult
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
                            -> Maybe InstalledPackageInfo
                            -> IO ()
updatePackageRegFileMonitor PackageFileMonitor{pkgFileMonitorReg}
                            srcdir mipkg =
    updateFileMonitor pkgFileMonitorReg srcdir Nothing
                      [] () mipkg

invalidatePackageRegFileMonitor :: PackageFileMonitor -> IO ()
invalidatePackageRegFileMonitor PackageFileMonitor{pkgFileMonitorReg} =
    removeExistingFile (fileMonitorCacheFile pkgFileMonitorReg)


------------------------------------------------------------------------------
-- * Doing it: executing an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------

-- Refer to ProjectBuilding.Types for details of these important types:

-- type BuildOutcomes = ...
-- type BuildOutcome  = ...
-- data BuildResult   = ...
-- data BuildFailure  = ...
-- data BuildFailureReason = ...

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
               -> IO BuildOutcomes
rebuildTargets verbosity
               distDirLayout@DistDirLayout{..}
               installPlan
               sharedPackageConfig@ElaboratedSharedConfig {
                 pkgConfigCompiler      = compiler,
                 pkgConfigCompilerProgs = progdb
               }
               pkgsBuildStatus
               buildSettings@BuildTimeSettings{
                 buildSettingNumJobs,
                 buildSettingKeepGoing
               } = do

    -- Concurrency control: create the job controller and concurrency limits
    -- for downloading, building and installing.
    jobControl    <- if isParallelBuild
                       then newParallelJobControl buildSettingNumJobs
                       else newSerialJobControl
    registerLock  <- newLock -- serialise registration
    cacheLock     <- newLock -- serialise access to setup exe cache
                             --TODO: [code cleanup] eliminate setup exe cache

    createDirectoryIfMissingVerbose verbosity True distBuildRootDirectory
    createDirectoryIfMissingVerbose verbosity True distTempDirectory
    mapM_ (createPackageDBIfMissing verbosity compiler progdb) packageDBsToUse

    -- Before traversing the install plan, pre-emptively find all packages that
    -- will need to be downloaded and start downloading them.
    asyncDownloadPackages verbosity withRepoCtx
                          installPlan pkgsBuildStatus $ \downloadMap ->

      -- For each package in the plan, in dependency order, but in parallel...
      InstallPlan.execute jobControl keepGoing
                          (BuildFailure Nothing . DependentFailed . packageId)
                          installPlan $ \pkg ->
        --TODO: review exception handling
        handle (\(e :: BuildFailure) -> return (Left e)) $ fmap Right $

        let uid = installedUnitId pkg
            Just pkgBuildStatus = Map.lookup uid pkgsBuildStatus in

        rebuildTarget
          verbosity
          distDirLayout
          buildSettings downloadMap
          registerLock cacheLock
          sharedPackageConfig
          pkg
          pkgBuildStatus
  where
    isParallelBuild = buildSettingNumJobs >= 2
    keepGoing       = buildSettingKeepGoing
    withRepoCtx     = projectConfigWithBuilderRepoContext verbosity
                        buildSettings
    packageDBsToUse = -- all the package dbs we may need to create
      (Set.toList . Set.fromList)
        [ pkgdb
        | InstallPlan.Configured elab <- InstallPlan.toList installPlan
        , (pkgdb:_) <- map reverse [ elabBuildPackageDBStack elab,
                                     elabRegisterPackageDBStack elab,
                                     elabSetupPackageDBStack elab ]
        ]

-- | Given all the context and resources, (re)build an individual package.
--
rebuildTarget :: Verbosity
              -> DistDirLayout
              -> BuildTimeSettings
              -> AsyncFetchMap
              -> Lock -> Lock
              -> ElaboratedSharedConfig
              -> ElaboratedReadyPackage
              -> BuildStatus
              -> IO BuildResult
rebuildTarget verbosity
              distDirLayout@DistDirLayout{distBuildDirectory}
              buildSettings downloadMap
              registerLock cacheLock
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
      BuildStatusInstalled   {} -> unexpectedState
      BuildStatusUpToDate    {} -> unexpectedState
  where
    unexpectedState = error "rebuildTarget: unexpected package status"

    downloadPhase = do
        downsrcloc <- annotateFailureNoLog DownloadFailed $
                        waitAsyncPackageDownload verbosity downloadMap pkg
        case downsrcloc of
          DownloadedTarball tarball -> unpackTarballPhase tarball
          --TODO: [nice to have] git/darcs repos etc


    unpackTarballPhase tarball =
        withTarballLocalDirectory
          verbosity distDirLayout tarball
          (packageId pkg) (elabDistDirParams sharedPackageConfig pkg) (elabBuildStyle pkg)
          (elabPkgDescriptionOverride pkg) $

          case elabBuildStyle pkg of
            BuildAndInstall  -> buildAndInstall
            BuildInplaceOnly -> buildInplace buildStatus
              where
                buildStatus = BuildStatusConfigure MonitorFirstRun

    -- Note that this really is rebuild, not build. It can only happen for
    -- 'BuildInplaceOnly' style packages. 'BuildAndInstall' style packages
    -- would only start from download or unpack phases.
    --
    rebuildPhase buildStatus srcdir =
        assert (elabBuildStyle pkg == BuildInplaceOnly) $

          buildInplace buildStatus srcdir builddir
      where
        builddir = distBuildDirectory (elabDistDirParams sharedPackageConfig pkg)

    buildAndInstall srcdir builddir =
        buildAndInstallUnpackedPackage
          verbosity distDirLayout
          buildSettings registerLock cacheLock
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
          buildSettings registerLock cacheLock
          sharedPackageConfig
          rpkg
          buildStatus
          srcdir builddir

--TODO: [nice to have] do we need to use a with-style for the temp files for downloading http
-- packages, or are we going to cache them persistently?

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
                      -> ((RepoContext -> IO a) -> IO a)
                      -> ElaboratedInstallPlan
                      -> BuildStatusMap
                      -> (AsyncFetchMap -> IO a)
                      -> IO a
asyncDownloadPackages verbosity withRepoCtx installPlan pkgsBuildStatus body
  | null pkgsToDownload = body Map.empty
  | otherwise           = withRepoCtx $ \repoctx ->
                            asyncFetchPackages verbosity repoctx
                                               pkgsToDownload body
  where
    pkgsToDownload =
      ordNub $
      [ elabPkgSourceLocation elab
      | InstallPlan.Configured elab
         <- InstallPlan.reverseTopologicalOrder installPlan
      , let uid = installedUnitId elab
            Just pkgBuildStatus = Map.lookup uid pkgsBuildStatus
      , BuildStatusDownload <- [pkgBuildStatus]
      ]


-- | Check if a package needs downloading, and if so expect to find a download
-- in progress in the given 'AsyncFetchMap' and wait on it to finish.
--
waitAsyncPackageDownload :: Verbosity
                         -> AsyncFetchMap
                         -> ElaboratedConfiguredPackage
                         -> IO DownloadedSourceLocation
waitAsyncPackageDownload verbosity downloadMap elab = do
    pkgloc <- waitAsyncFetchPackage verbosity downloadMap
                                    (elabPkgSourceLocation elab)
    case downloadedSourceLocation pkgloc of
      Just loc -> return loc
      Nothing  -> fail "waitAsyncPackageDownload: unexpected source location"

data DownloadedSourceLocation = DownloadedTarball FilePath
                              --TODO: [nice to have] git/darcs repos etc

downloadedSourceLocation :: PackageLocation FilePath
                         -> Maybe DownloadedSourceLocation
downloadedSourceLocation pkgloc =
    case pkgloc of
      RemoteTarballPackage _ tarball -> Just (DownloadedTarball tarball)
      RepoTarballPackage _ _ tarball -> Just (DownloadedTarball tarball)
      _                              -> Nothing




-- | Ensure that the package is unpacked in an appropriate directory, either
-- a temporary one or a persistent one under the shared dist directory.
--
withTarballLocalDirectory
  :: Verbosity
  -> DistDirLayout
  -> FilePath
  -> PackageId
  -> DistDirParams
  -> BuildStyle
  -> Maybe CabalFileText
  -> (FilePath -> -- Source directory
      FilePath -> -- Build directory
      IO a)
  -> IO a
withTarballLocalDirectory verbosity distDirLayout@DistDirLayout{..}
                          tarball pkgid dparams buildstyle pkgTextOverride
                          buildPkg  =
      case buildstyle of
        -- In this case we make a couple temp dirs, unpack the tarball to one
        -- and build and install it from the other. We avoid nesting the
        -- builddir under the tarball src dir to keep path name lengths down.
        BuildAndInstall ->
          let tmpdir = distTempDirectory in
          withTempDirectory verbosity tmpdir "src"   $ \unpackdir ->
          withTempDirectory verbosity tmpdir "build" $ \builddir -> do
            unpackPackageTarball verbosity tarball unpackdir
                                 pkgid pkgTextOverride
            let srcdir   = unpackdir </> display pkgid
            buildPkg srcdir builddir

        -- In this case we make sure the tarball has been unpacked to the
        -- appropriate location under the shared dist dir, and then build it
        -- inplace there
        BuildInplaceOnly -> do
          let srcrootdir = distUnpackedSrcRootDirectory
              srcdir     = distUnpackedSrcDirectory pkgid
              builddir   = distBuildDirectory dparams
          -- TODO: [nice to have] use a proper file monitor rather than this dir exists test
          exists <- doesDirectoryExist srcdir
          unless exists $ do
            createDirectoryIfMissingVerbose verbosity True srcrootdir
            unpackPackageTarball verbosity tarball srcrootdir
                                 pkgid pkgTextOverride
            moveTarballShippedDistDirectory verbosity distDirLayout
                                            srcrootdir pkgid dparams
          buildPkg srcdir builddir


unpackPackageTarball :: Verbosity -> FilePath -> FilePath
                     -> PackageId -> Maybe CabalFileText
                     -> IO ()
unpackPackageTarball verbosity tarball parentdir pkgid pkgTextOverride =
    --TODO: [nice to have] switch to tar package and catch tar exceptions
    annotateFailureNoLog UnpackFailed $ do

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
                                -> FilePath -> PackageId -> DistDirParams -> IO ()
moveTarballShippedDistDirectory verbosity DistDirLayout{distBuildDirectory}
                                parentdir pkgid dparams = do
    distDirExists <- doesDirectoryExist tarballDistDir
    when distDirExists $ do
      debug verbosity $ "Moving '" ++ tarballDistDir ++ "' to '"
                                   ++ targetDistDir ++ "'"
      --TODO: [nice to have] or perhaps better to copy, and use a file monitor
      renameDirectory tarballDistDir targetDistDir
  where
    tarballDistDir = parentdir </> display pkgid </> "dist"
    targetDistDir  = distBuildDirectory dparams


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
                               registerLock cacheLock
                               pkgshared@ElaboratedSharedConfig {
                                 pkgConfigPlatform      = platform,
                                 pkgConfigCompiler      = compiler,
                                 pkgConfigCompilerProgs = progdb
                               }
                               rpkg@(ReadyPackage pkg)
                               srcdir builddir = do

    createDirectoryIfMissingVerbose verbosity True builddir
    initLogFile

    --TODO: [code cleanup] deal consistently with talking to older Setup.hs versions, much like
    --      we do for ghc, with a proper options type and rendering step
    --      which will also let us call directly into the lib, rather than always
    --      going via the lib's command line interface, which would also allow
    --      passing data like installed packages, compiler, and program db for a
    --      quicker configure.

    --TODO: [required feature] docs and tests
    --TODO: [required feature] sudo re-exec

    let dispname = case elabPkgOrComp pkg of
            ElabPackage _ -> display pkgid
                ++ " (all, due to Custom setup)"
            ElabComponent comp -> display pkgid
                ++ " (" ++ maybe "custom" display (compComponentName comp) ++ ")"

    -- Configure phase
    when isParallelBuild $
      notice verbosity $ "Configuring " ++ dispname ++ "..."
    annotateFailure mlogFile ConfigureFailed $
      setup' configureCommand configureFlags configureArgs

    -- Build phase
    when isParallelBuild $
      notice verbosity $ "Building " ++ dispname ++ "..."
    annotateFailure mlogFile BuildFailed $
      setup buildCommand buildFlags

    -- Install phase
    annotateFailure mlogFile InstallFailed $ do
      --TODO: [required eventually] need to lock installing this ipkig so other processes don't
      -- stomp on our files, since we don't have ABI compat, not safe to replace

      -- TODO: [required eventually] note that for nix-style installations it is not necessary to do
      -- the 'withWin32SelfUpgrade' dance, but it would be necessary for a
      -- shared bin dir.

      -- Actual installation
      setup Cabal.copyCommand copyFlags

      LBS.writeFile
        (InstallDirs.prefix (elabInstallDirs pkg) </> "cabal-hash.txt") $
        (renderPackageHashInputs (packageHashInputs pkgshared pkg))

      -- here's where we could keep track of the installed files ourselves if
      -- we wanted by calling copy to an image dir and then we would make a
      -- manifest and move it to its final location

      --TODO: [nice to have] we should actually have it make an image in store/incomming and
      -- then when it's done, move it to its final location, to reduce problems
      -- with installs failing half-way. Could also register and then move.

      if elabRequiresRegistration pkg
        then do
          -- We register ourselves rather than via Setup.hs. We need to
          -- grab and modify the InstalledPackageInfo. We decide what
          -- the installed package id is, not the build system.
          ipkg0 <- generateInstalledPackageInfo
          let ipkg = ipkg0 { Installed.installedUnitId = uid }

          criticalSection registerLock $
              Cabal.registerPackage verbosity compiler progdb
                                    HcPkg.MultiInstance
                                    (elabRegisterPackageDBStack pkg) ipkg
        else return ()

    --TODO: [required feature] docs and test phases
    let docsResult  = DocsNotTried
        testsResult = TestsNotTried

    return BuildResult {
       buildResultDocs    = docsResult,
       buildResultTests   = testsResult,
       buildResultLogFile = mlogFile
    }

  where
    pkgid  = packageId rpkg
    uid = installedUnitId rpkg

    isParallelBuild = buildSettingNumJobs >= 2

    configureCommand = Cabal.configureCommand defaultProgramDb
    configureFlags v = flip filterConfigureFlags v $
                       setupHsConfigureFlags rpkg pkgshared
                                             verbosity builddir
    configureArgs    = setupHsConfigureArgs pkg

    buildCommand     = Cabal.buildCommand defaultProgramDb
    buildFlags   _   = setupHsBuildFlags pkg pkgshared verbosity builddir

    generateInstalledPackageInfo :: IO InstalledPackageInfo
    generateInstalledPackageInfo =
      withTempInstalledPackageInfoFile
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
    setup cmd flags = setup' cmd flags []

    setup' :: CommandUI flags -> (Version -> flags) -> [String] -> IO ()
    setup' cmd flags args =
      withLogging $ \mLogFileHandle ->
        setupWrapper
          verbosity
          scriptOptions { useLoggingHandle = mLogFileHandle }
          (Just (elabPkgDescription pkg))
          cmd flags args

    mlogFile :: Maybe FilePath
    mlogFile =
      case buildSettingLogFile of
        Nothing        -> Nothing
        Just mkLogFile -> Just (mkLogFile compiler platform pkgid uid)

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
                            -> BuildTimeSettings -> Lock -> Lock
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
                            registerLock cacheLock
                            pkgshared@ElaboratedSharedConfig {
                              pkgConfigCompiler      = compiler,
                              pkgConfigCompilerProgs = progdb
                            }
                            rpkg@(ReadyPackage pkg)
                            buildStatus
                            srcdir builddir = do

        --TODO: [code cleanup] there is duplication between the distdirlayout and the builddir here
        --      builddir is not enough, we also need the per-package cachedir
        createDirectoryIfMissingVerbose verbosity True builddir
        createDirectoryIfMissingVerbose verbosity True (distPackageCacheDirectory dparams)

        -- Configure phase
        --
        whenReConfigure $ do
          annotateFailureNoLog ConfigureFailed $
            setup configureCommand configureFlags configureArgs
          invalidatePackageRegFileMonitor packageFileMonitor
          updatePackageConfigFileMonitor packageFileMonitor srcdir pkg

        -- Build phase
        --
        let docsResult  = DocsNotTried
            testsResult = TestsNotTried

            buildResult :: BuildResultMisc
            buildResult = (docsResult, testsResult)

        whenRebuild $ do
          timestamp <- beginUpdateFileMonitor
          annotateFailureNoLog BuildFailed $
            setup buildCommand buildFlags buildArgs

          --TODO: [required eventually] this doesn't track file
          --non-existence, so we could fail to rebuild if someone
          --adds a new file which changes behavior.
          allSrcFiles <-
            let trySdist    = allPackageSourceFiles verbosity scriptOptions srcdir
                -- This is just a hack, to get semi-reasonable file
                -- listings for the monitor
                tryFallback = do
                    warn verbosity $
                        "Couldn't use sdist to compute source files; falling " ++
                        "back on recursive file scan."
                    filter (not . ("dist" `isPrefixOf`))
                        `fmap` getDirectoryContentsRecursive srcdir
            in if elabSetupScriptCliVersion pkg >= mkVersion [1,17]
                  then do r <- trySdist
                          if null r
                            then tryFallback
                            else return r
                  else tryFallback

          updatePackageBuildFileMonitor packageFileMonitor srcdir timestamp
                                        pkg buildStatus
                                        allSrcFiles buildResult

        -- PURPOSELY omitted: no copy!

        whenReRegister $ annotateFailureNoLog InstallFailed $ do
          -- Register locally
          mipkg <- if elabRequiresRegistration pkg
            then do
                ipkg0 <- generateInstalledPackageInfo
                -- We register ourselves rather than via Setup.hs. We need to
                -- grab and modify the InstalledPackageInfo. We decide what
                -- the installed package id is, not the build system.
                let ipkg = ipkg0 { Installed.installedUnitId = ipkgid }
                criticalSection registerLock $
                    Cabal.registerPackage verbosity compiler progdb HcPkg.NoMultiInstance
                                          (elabRegisterPackageDBStack pkg)
                                          ipkg
                return (Just ipkg)

           else return Nothing

          updatePackageRegFileMonitor packageFileMonitor srcdir mipkg

        -- Repl phase
        --
        whenRepl $
          annotateFailureNoLog ReplFailed $
          setupInteractive replCommand replFlags replArgs

        -- Haddock phase
        whenHaddock $
          annotateFailureNoLog HaddocksFailed $
          setup haddockCommand haddockFlags []

        return BuildResult {
          buildResultDocs    = docsResult,
          buildResultTests   = testsResult,
          buildResultLogFile = Nothing
        }

  where
    ipkgid  = installedUnitId pkg
    dparams = elabDistDirParams pkgshared pkg

    isParallelBuild = buildSettingNumJobs >= 2

    packageFileMonitor = newPackageFileMonitor distDirLayout dparams

    whenReConfigure action = case buildStatus of
      BuildStatusConfigure _ -> action
      _                      -> return ()

    whenRebuild action
      | null (elabBuildTargets pkg) = return ()
      | otherwise                  = action

    whenRepl action
      | isNothing (elabReplTarget pkg) = return ()
      | otherwise                     = action

    whenHaddock action
      | elabBuildHaddocks pkg = action
      | otherwise            = return ()

    whenReRegister  action
      = case buildStatus of
          -- We registered the package already
          BuildStatusBuild (Just _) _     -> return ()
          -- There is nothing to register
          _ | null (elabBuildTargets pkg) -> return ()
            | otherwise                   -> action

    configureCommand = Cabal.configureCommand defaultProgramDb
    configureFlags v = flip filterConfigureFlags v $
                       setupHsConfigureFlags rpkg pkgshared
                                             verbosity builddir
    configureArgs    = setupHsConfigureArgs pkg

    buildCommand     = Cabal.buildCommand defaultProgramDb
    buildFlags   _   = setupHsBuildFlags pkg pkgshared
                                         verbosity builddir
    buildArgs        = setupHsBuildArgs  pkg

    replCommand      = Cabal.replCommand defaultProgramDb
    replFlags _      = setupHsReplFlags pkg pkgshared
                                        verbosity builddir
    replArgs         = setupHsReplArgs  pkg

    haddockCommand   = Cabal.haddockCommand
    haddockFlags _   = setupHsHaddockFlags pkg pkgshared
                                           verbosity builddir

    scriptOptions    = setupHsScriptOptions rpkg pkgshared
                                            srcdir builddir
                                            isParallelBuild cacheLock

    setupInteractive :: CommandUI flags
                     -> (Version -> flags) -> [String] -> IO ()
    setupInteractive cmd flags args =
      setupWrapper verbosity
                   scriptOptions { isInteractive = True }
                   (Just (elabPkgDescription pkg))
                   cmd flags args

    setup :: CommandUI flags -> (Version -> flags) -> [String] -> IO ()
    setup cmd flags args =
      setupWrapper verbosity
                   scriptOptions
                   (Just (elabPkgDescription pkg))
                   cmd flags args

    generateInstalledPackageInfo :: IO InstalledPackageInfo
    generateInstalledPackageInfo =
      withTempInstalledPackageInfoFile
        verbosity distTempDirectory $ \pkgConfDest -> do
        let registerFlags _ = setupHsRegisterFlags
                                pkg pkgshared
                                verbosity builddir
                                pkgConfDest
        setup Cabal.registerCommand registerFlags []

withTempInstalledPackageInfoFile :: Verbosity -> FilePath
                                  -> (FilePath -> IO ())
                                  -> IO InstalledPackageInfo
withTempInstalledPackageInfoFile verbosity tempdir action =
    withTempDirectory verbosity tempdir "package-registration-" $ \dir -> do
      -- make absolute since @action@ will often change directory
      abs_dir <- canonicalizePath dir

      let pkgConfDest = abs_dir </> "pkgConf"
      action pkgConfDest

      readPkgConf "." pkgConfDest
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


------------------------------------------------------------------------------
-- * Utilities
------------------------------------------------------------------------------

annotateFailureNoLog :: (SomeException -> BuildFailureReason)
                     -> IO a -> IO a
annotateFailureNoLog annotate action =
  annotateFailure Nothing annotate action

annotateFailure :: Maybe FilePath
                -> (SomeException -> BuildFailureReason)
                -> IO a -> IO a
annotateFailure mlogFile annotate action =
  action `catches`
    -- It's not just IOException and ExitCode we have to deal with, there's
    -- lots, including exceptions from the hackage-security and tar packages.
    -- So we take the strategy of catching everything except async exceptions.
    [
#if MIN_VERSION_base(4,7,0)
      Handler $ \async -> throwIO (async :: SomeAsyncException)
#else
      Handler $ \async -> throwIO (async :: AsyncException)
#endif
    , Handler $ \other -> handler (other :: SomeException)
    ]
  where
    handler :: Exception e => e -> IO a
    handler = throwIO . BuildFailure mlogFile . annotate . toException

