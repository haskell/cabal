{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Distribution.Client.ProjectBuilding
  ( -- * Dry run phase

    -- | What bits of the plan will we execute? The dry run does not change
    -- anything but tells us what will need to be built.
    rebuildTargetsDryRun
  , improveInstallPlanWithUpToDatePackages

    -- ** Build status

    -- | This is the detailed status information we get from the dry run.
  , BuildStatusMap
  , BuildStatus (..)
  , BuildStatusRebuild (..)
  , BuildReason (..)
  , MonitorChangedReason (..)
  , buildStatusToString

    -- * Build phase

    -- | Now we actually execute the plan.
  , rebuildTargets

    -- ** Build outcomes

    -- | This is the outcome for each package of executing the plan.
    -- For each package, did the build succeed or fail?
  , BuildOutcomes
  , BuildOutcome
  , BuildResult (..)
  , BuildFailure (..)
  , BuildFailureReason (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ProjectBuilding.Types
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Store

import Distribution.Client.DistDirLayout
import Distribution.Client.FetchUtils
import Distribution.Client.GlobalFlags (RepoContext)
import Distribution.Client.InstallPlan
  ( GenericInstallPlan
  , GenericPlanPackage
  , IsUnit
  )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.JobControl
import qualified Distribution.Client.Tar as Tar
import Distribution.Client.Types hiding
  ( BuildFailure (..)
  , BuildOutcome
  , BuildOutcomes
  , BuildResult (..)
  )

import Distribution.Package
import Distribution.Simple.Compiler
  ( Compiler
  , PackageDB (..)
  , jsemSupported
  )
import Distribution.Simple.Program
import qualified Distribution.Simple.Register as Cabal

import Distribution.Compat.Graph (IsNode (..))
import Distribution.Simple.Utils
import Distribution.Utils.Path hiding
  ( (<.>)
  , (</>)
  )
import Distribution.Version

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Text.PrettyPrint as Disp

import Control.Exception (assert, bracket, handle)
import System.Directory (doesDirectoryExist, doesFileExist, renameDirectory)
import System.FilePath (makeRelative, normalise, takeDirectory, (<.>), (</>))
import System.Semaphore (SemaphoreName (..))

import Distribution.Client.Errors
import Distribution.Simple.Flag (fromFlagOrDefault)

import Distribution.Client.ProjectBuilding.PackageFileMonitor
import Distribution.Client.ProjectBuilding.UnpackedPackage (annotateFailureNoLog, buildAndInstallUnpackedPackage, buildInplaceUnpackedPackage)
import Distribution.Client.Utils (numberOfProcessors)

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
rebuildTargetsDryRun
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> IO BuildStatusMap
rebuildTargetsDryRun distDirLayout@DistDirLayout{..} shared =
  -- Do the various checks to work out the 'BuildStatus' of each package
  foldMInstallPlanDepOrder dryRunPkg
  where
    dryRunPkg
      :: ElaboratedPlanPackage
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
        -- The rest cases are all tarball cases are,
        -- and handled the same as each other though depending on the build style.
        Just (LocalTarballPackage tarball) ->
          dryRunTarballPkg pkg depsBuildStatus tarball
        Just (RemoteTarballPackage _ tarball) ->
          dryRunTarballPkg pkg depsBuildStatus tarball
        Just (RepoTarballPackage _ _ tarball) ->
          dryRunTarballPkg pkg depsBuildStatus tarball
        Just (RemoteSourceRepoPackage _repo tarball) ->
          dryRunTarballPkg pkg depsBuildStatus tarball

    dryRunTarballPkg
      :: ElaboratedConfiguredPackage
      -> [BuildStatus]
      -> FilePath
      -> IO BuildStatus
    dryRunTarballPkg pkg depsBuildStatus tarball =
      case elabBuildStyle pkg of
        BuildAndInstall -> return (BuildStatusUnpack tarball)
        BuildInplaceOnly{} -> do
          -- TODO: [nice to have] use a proper file monitor rather
          -- than this dir exists test
          exists <- doesDirectoryExist srcdir
          if exists
            then dryRunLocalPkg pkg depsBuildStatus srcdir
            else return (BuildStatusUnpack tarball)
      where
        srcdir :: FilePath
        srcdir = distUnpackedSrcDirectory (packageId pkg)

    dryRunLocalPkg
      :: ElaboratedConfiguredPackage
      -> [BuildStatus]
      -> FilePath
      -> IO BuildStatus
    dryRunLocalPkg pkg depsBuildStatus srcdir = do
      -- Go and do lots of I/O, reading caches and probing files to work out
      -- if anything has changed
      change <-
        checkPackageFileMonitorChanged
          packageFileMonitor
          pkg
          srcdir
          depsBuildStatus
      case change of
        -- It did change, giving us 'BuildStatusRebuild' info on why
        Left rebuild ->
          return (BuildStatusRebuild srcdir rebuild)
        -- No changes, the package is up to date. Use the saved build results.
        Right buildResult ->
          return (BuildStatusUpToDate buildResult)
      where
        packageFileMonitor :: PackageFileMonitor
        packageFileMonitor =
          newPackageFileMonitor
            shared
            distDirLayout
            (elabDistDirParams shared pkg)

-- | A specialised traversal over the packages in an install plan.
--
-- The packages are visited in dependency order, starting with packages with no
-- dependencies. The result for each package is accumulated into a 'Map' and
-- returned as the final result. In addition, when visiting a package, the
-- visiting function is passed the results for all the immediate package
-- dependencies. This can be used to propagate information from dependencies.
foldMInstallPlanDepOrder
  :: forall m ipkg srcpkg b
   . (Monad m, IsUnit ipkg, IsUnit srcpkg)
  => ( GenericPlanPackage ipkg srcpkg
       -> [b]
       -> m b
     )
  -> GenericInstallPlan ipkg srcpkg
  -> m (Map UnitId b)
foldMInstallPlanDepOrder visit =
  go Map.empty . InstallPlan.reverseTopologicalOrder
  where
    go
      :: Map UnitId b
      -> [GenericPlanPackage ipkg srcpkg]
      -> m (Map UnitId b)
    go !results [] = return results
    go !results (pkg : pkgs) = do
      -- we go in the right order so the results map has entries for all deps
      let depresults :: [b]
          depresults =
            map
              ( \ipkgid ->
                  let result = Map.findWithDefault (error "foldMInstallPlanDepOrder") ipkgid results
                   in result
              )
              (InstallPlan.depends pkg)
      result <- visit pkg depresults
      let results' = Map.insert (nodeKey pkg) result results
      go results' pkgs

improveInstallPlanWithUpToDatePackages
  :: BuildStatusMap
  -> ElaboratedInstallPlan
  -> ElaboratedInstallPlan
improveInstallPlanWithUpToDatePackages pkgsBuildStatus =
  InstallPlan.installed canPackageBeImproved
  where
    canPackageBeImproved :: ElaboratedConfiguredPackage -> Bool
    canPackageBeImproved pkg =
      case Map.lookup (installedUnitId pkg) pkgsBuildStatus of
        Just BuildStatusUpToDate{} -> True
        Just _ -> False
        Nothing ->
          error $
            "improveInstallPlanWithUpToDatePackages: "
              ++ prettyShow (packageId pkg)
              ++ " not in status map"

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
rebuildTargets
  :: Verbosity
  -> ProjectConfig
  -> DistDirLayout
  -> StoreDirLayout
  -> ElaboratedInstallPlan
  -> ElaboratedSharedConfig
  -> BuildStatusMap
  -> BuildTimeSettings
  -> IO BuildOutcomes
rebuildTargets
  verbosity
  ProjectConfig
    { projectConfigBuildOnly = config
    }
  distDirLayout@DistDirLayout{..}
  storeDirLayout
  installPlan
  sharedPackageConfig@ElaboratedSharedConfig
    { pkgConfigCompiler = compiler
    , pkgConfigCompilerProgs = progdb
    }
  pkgsBuildStatus
  buildSettings@BuildTimeSettings
    { buildSettingNumJobs
    , buildSettingKeepGoing
    }
    | fromFlagOrDefault False (projectConfigOfflineMode config) && not (null packagesToDownload) = return offlineError
    | otherwise = do
        -- Concurrency control: create the job controller and concurrency limits
        -- for downloading, building and installing.
        mkJobControl <- case buildSettingNumJobs of
          Serial -> newSerialJobControl
          NumJobs n -> newParallelJobControl (fromMaybe numberOfProcessors n)
          UseSem n ->
            if jsemSupported compiler
              then newSemaphoreJobControl n
              else do
                warn verbosity "-jsem is not supported by the selected compiler, falling back to normal parallelism control."
                newParallelJobControl n
        registerLock <- newLock -- serialise registration
        cacheLock <- newLock -- serialise access to setup exe cache
        -- TODO: [code cleanup] eliminate setup exe cache
        info verbosity $
          "Executing install plan "
            ++ case buildSettingNumJobs of
              NumJobs n -> "in parallel using " ++ show n ++ " threads."
              UseSem n -> "in parallel using a semaphore with " ++ show n ++ " slots."
              Serial -> "serially."

        createDirectoryIfMissingVerbose verbosity True distBuildRootDirectory
        createDirectoryIfMissingVerbose verbosity True distTempDirectory
        traverse_ (createPackageDBIfMissing verbosity compiler progdb) packageDBsToUse

        bracket (pure mkJobControl) cleanupJobControl $ \jobControl -> do
          -- Before traversing the install plan, preemptively find all packages that
          -- will need to be downloaded and start downloading them.
          asyncDownloadPackages
            verbosity
            withRepoCtx
            installPlan
            pkgsBuildStatus
            $ \downloadMap ->
              -- For each package in the plan, in dependency order, but in parallel...
              InstallPlan.execute
                mkJobControl
                keepGoing
                (BuildFailure Nothing . DependentFailed . packageId)
                installPlan
                $ \pkg ->
                  -- TODO: review exception handling
                  handle (\(e :: BuildFailure) -> return (Left e)) $ fmap Right $ do
                    let uid = installedUnitId pkg
                        pkgBuildStatus = Map.findWithDefault (error "rebuildTargets") uid pkgsBuildStatus

                    rebuildTarget
                      verbosity
                      distDirLayout
                      storeDirLayout
                      (jobControlSemaphore jobControl)
                      buildSettings
                      downloadMap
                      registerLock
                      cacheLock
                      sharedPackageConfig
                      installPlan
                      pkg
                      pkgBuildStatus
    where
      keepGoing = buildSettingKeepGoing
      withRepoCtx =
        projectConfigWithBuilderRepoContext
          verbosity
          buildSettings
      packageDBsToUse =
        -- all the package dbs we may need to create
        (Set.toList . Set.fromList)
          [ pkgdb
          | InstallPlan.Configured elab <- InstallPlan.toList installPlan
          , pkgdb <-
              concat
                [ elabBuildPackageDBStack elab
                , elabRegisterPackageDBStack elab
                , elabSetupPackageDBStack elab
                ]
          ]

      offlineError :: BuildOutcomes
      offlineError = Map.fromList . map makeBuildOutcome $ packagesToDownload
        where
          makeBuildOutcome :: ElaboratedConfiguredPackage -> (UnitId, BuildOutcome)
          makeBuildOutcome
            ElaboratedConfiguredPackage
              { elabUnitId
              , elabPkgSourceId = PackageIdentifier{pkgName, pkgVersion}
              } =
              ( elabUnitId
              , Left
                  ( BuildFailure
                      { buildFailureLogFile = Nothing
                      , buildFailureReason = GracefulFailure $ makeError pkgName pkgVersion
                      }
                  )
              )
          makeError :: PackageName -> Version -> String
          makeError n v =
            "--offline was specified, hence refusing to download the package: "
              ++ unPackageName n
              ++ " version "
              ++ Disp.render (pretty v)

      packagesToDownload :: [ElaboratedConfiguredPackage]
      packagesToDownload =
        [ elab | InstallPlan.Configured elab <- InstallPlan.reverseTopologicalOrder installPlan, isRemote $ elabPkgSourceLocation elab
        ]
        where
          isRemote :: PackageLocation a -> Bool
          isRemote (RemoteTarballPackage _ _) = True
          isRemote (RepoTarballPackage{}) = True
          isRemote (RemoteSourceRepoPackage _ _) = True
          isRemote _ = False

-- | Create a package DB if it does not currently exist. Note that this action
-- is /not/ safe to run concurrently.
createPackageDBIfMissing
  :: Verbosity
  -> Compiler
  -> ProgramDb
  -> PackageDB
  -> IO ()
createPackageDBIfMissing
  verbosity
  compiler
  progdb
  (SpecificPackageDB dbPath) = do
    exists <- Cabal.doesPackageDBExist dbPath
    unless exists $ do
      createDirectoryIfMissingVerbose verbosity True (takeDirectory dbPath)
      Cabal.createPackageDB verbosity compiler progdb False dbPath
createPackageDBIfMissing _ _ _ _ = return ()

-- | Given all the context and resources, (re)build an individual package.
rebuildTarget
  :: Verbosity
  -> DistDirLayout
  -> StoreDirLayout
  -> Maybe SemaphoreName
  -> BuildTimeSettings
  -> AsyncFetchMap
  -> Lock
  -> Lock
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> ElaboratedReadyPackage
  -> BuildStatus
  -> IO BuildResult
rebuildTarget
  verbosity
  distDirLayout@DistDirLayout{distBuildDirectory}
  storeDirLayout
  semaphoreName
  buildSettings
  downloadMap
  registerLock
  cacheLock
  sharedPackageConfig
  plan
  rpkg@(ReadyPackage pkg)
  pkgBuildStatus
    -- Technically, doing the --only-download filtering only in this function is
    -- not perfect. We could also prune the plan at an earlier stage, like it's
    -- done with --only-dependencies. But...
    --   * the benefit would be minimal (practically just avoiding to print the
    --     "requires build" parts of the plan)
    --   * we currently don't have easy access to the BuildStatus of packages
    --     in the pruning phase
    --   * we still have to check it here to avoid performing successive phases
    | buildSettingOnlyDownload buildSettings = do
        case pkgBuildStatus of
          BuildStatusDownload ->
            void $ waitAsyncPackageDownload verbosity downloadMap pkg
          _ -> return ()
        return $ BuildResult DocsNotTried TestsNotTried Nothing
    | otherwise =
        -- We rely on the 'BuildStatus' to decide which phase to start from:
        case pkgBuildStatus of
          BuildStatusDownload -> downloadPhase
          BuildStatusUnpack tarball -> unpackTarballPhase tarball
          BuildStatusRebuild srcdir status -> rebuildPhase status (makeSymbolicPath srcdir)
          -- TODO: perhaps re-nest the types to make these impossible
          BuildStatusPreExisting{} -> unexpectedState
          BuildStatusInstalled{} -> unexpectedState
          BuildStatusUpToDate{} -> unexpectedState
    where
      unexpectedState = error "rebuildTarget: unexpected package status"

      downloadPhase :: IO BuildResult
      downloadPhase = do
        downsrcloc <-
          annotateFailureNoLog DownloadFailed $
            waitAsyncPackageDownload verbosity downloadMap pkg
        case downsrcloc of
          DownloadedTarball tarball -> unpackTarballPhase tarball
      -- TODO: [nice to have] git/darcs repos etc

      unpackTarballPhase :: FilePath -> IO BuildResult
      unpackTarballPhase tarball =
        withTarballLocalDirectory
          verbosity
          distDirLayout
          tarball
          (packageId pkg)
          (elabDistDirParams sharedPackageConfig pkg)
          (elabBuildStyle pkg)
          (elabPkgDescriptionOverride pkg)
          $ case elabBuildStyle pkg of
            BuildAndInstall -> buildAndInstall
            BuildInplaceOnly{} -> buildInplace buildStatus
              where
                buildStatus = BuildStatusConfigure MonitorFirstRun

      -- Note that this really is rebuild, not build. It can only happen for
      -- 'BuildInplaceOnly' style packages. 'BuildAndInstall' style packages
      -- would only start from download or unpack phases.
      --
      rebuildPhase :: BuildStatusRebuild -> SymbolicPath CWD (Dir Pkg) -> IO BuildResult
      rebuildPhase buildStatus srcdir =
        assert
          (isInplaceBuildStyle $ elabBuildStyle pkg)
          buildInplace
          buildStatus
          srcdir
          builddir
        where
          distdir = distBuildDirectory (elabDistDirParams sharedPackageConfig pkg)
          builddir =
            makeSymbolicPath $
              makeRelative (normalise $ getSymbolicPath srcdir) distdir
      -- TODO: [nice to have] ^^ do this relative stuff better

      buildAndInstall :: SymbolicPath CWD (Dir Pkg) -> SymbolicPath Pkg (Dir Dist) -> IO BuildResult
      buildAndInstall srcdir builddir =
        buildAndInstallUnpackedPackage
          verbosity
          distDirLayout
          storeDirLayout
          semaphoreName
          buildSettings
          registerLock
          cacheLock
          sharedPackageConfig
          plan
          rpkg
          srcdir
          builddir

      buildInplace :: BuildStatusRebuild -> SymbolicPath CWD (Dir Pkg) -> SymbolicPath Pkg (Dir Dist) -> IO BuildResult
      buildInplace buildStatus srcdir builddir =
        -- TODO: [nice to have] use a relative build dir rather than absolute
        buildInplaceUnpackedPackage
          verbosity
          distDirLayout
          semaphoreName
          buildSettings
          registerLock
          cacheLock
          sharedPackageConfig
          plan
          rpkg
          buildStatus
          srcdir
          builddir

-- TODO: [nice to have] do we need to use a with-style for the temp
-- files for downloading http packages, or are we going to cache them
-- persistently?

-- | Given the current 'InstallPlan' and 'BuildStatusMap', select all the
-- packages we have to download and fork off an async action to download them.
-- We download them in dependency order so that the one's we'll need
-- first are the ones we will start downloading first.
--
-- The body action is passed a map from those packages (identified by their
-- location) to a completion var for that package. So the body action should
-- lookup the location and use 'waitAsyncPackageDownload' to get the result.
asyncDownloadPackages
  :: Verbosity
  -> ((RepoContext -> IO a) -> IO a)
  -> ElaboratedInstallPlan
  -> BuildStatusMap
  -> (AsyncFetchMap -> IO a)
  -> IO a
asyncDownloadPackages verbosity withRepoCtx installPlan pkgsBuildStatus body
  | null pkgsToDownload = body Map.empty
  | otherwise = withRepoCtx $ \repoctx ->
      asyncFetchPackages
        verbosity
        repoctx
        pkgsToDownload
        body
  where
    pkgsToDownload :: [PackageLocation (Maybe FilePath)]
    pkgsToDownload =
      ordNub $
        [ elabPkgSourceLocation elab
        | InstallPlan.Configured elab <-
            InstallPlan.reverseTopologicalOrder installPlan
        , let uid = installedUnitId elab
              pkgBuildStatus = Map.findWithDefault (error "asyncDownloadPackages") uid pkgsBuildStatus
        , BuildStatusDownload <- [pkgBuildStatus]
        ]

-- | Check if a package needs downloading, and if so expect to find a download
-- in progress in the given 'AsyncFetchMap' and wait on it to finish.
waitAsyncPackageDownload
  :: Verbosity
  -> AsyncFetchMap
  -> ElaboratedConfiguredPackage
  -> IO DownloadedSourceLocation
waitAsyncPackageDownload verbosity downloadMap elab = do
  pkgloc <-
    waitAsyncFetchPackage
      verbosity
      downloadMap
      (elabPkgSourceLocation elab)
  case downloadedSourceLocation pkgloc of
    Just loc -> return loc
    Nothing -> fail "waitAsyncPackageDownload: unexpected source location"

data DownloadedSourceLocation = DownloadedTarball FilePath

-- TODO: [nice to have] git/darcs repos etc

downloadedSourceLocation
  :: PackageLocation FilePath
  -> Maybe DownloadedSourceLocation
downloadedSourceLocation pkgloc =
  case pkgloc of
    RemoteTarballPackage _ tarball -> Just (DownloadedTarball tarball)
    RepoTarballPackage _ _ tarball -> Just (DownloadedTarball tarball)
    _ -> Nothing

-- | Ensure that the package is unpacked in an appropriate directory, either
-- a temporary one or a persistent one under the shared dist directory.
withTarballLocalDirectory
  :: Verbosity
  -> DistDirLayout
  -> FilePath
  -> PackageId
  -> DistDirParams
  -> BuildStyle
  -> Maybe CabalFileText
  -> ( SymbolicPath CWD (Dir Pkg) -- Source directory
       -> SymbolicPath Pkg (Dir Dist) -- Build directory
       -> IO a
     )
  -> IO a
withTarballLocalDirectory
  verbosity
  distDirLayout@DistDirLayout{..}
  tarball
  pkgid
  dparams
  buildstyle
  pkgTextOverride
  buildPkg =
    case buildstyle of
      -- In this case we make a temp dir (e.g. tmp/src2345/), unpack
      -- the tarball to it (e.g. tmp/src2345/foo-1.0/), and for
      -- compatibility we put the dist dir within it
      -- (i.e. tmp/src2345/foo-1.0/dist/).
      --
      -- Unfortunately, a few custom Setup.hs scripts do not respect
      -- the --builddir flag and always look for it at ./dist/ so
      -- this way we avoid breaking those packages
      BuildAndInstall ->
        let tmpdir = distTempDirectory
            builddir = relativeSymbolicPath $ makeRelativePathEx "dist"
         in withTempDirectory verbosity tmpdir "src" $ \unpackdir -> do
              let srcdir = makeSymbolicPath $ unpackdir </> prettyShow pkgid
              unpackPackageTarball
                verbosity
                tarball
                unpackdir
                pkgid
                pkgTextOverride
              buildPkg srcdir builddir

      -- In this case we make sure the tarball has been unpacked to the
      -- appropriate location under the shared dist dir, and then build it
      -- inplace there
      BuildInplaceOnly{} -> do
        let srcrootdir = distUnpackedSrcRootDirectory
            srcdir = distUnpackedSrcDirectory pkgid
            builddir =
              makeSymbolicPath $
                makeRelative (normalise srcdir) $
                  distBuildDirectory dparams
        -- TODO: [nice to have] ^^ do this relative stuff better
        exists <- doesDirectoryExist srcdir
        -- TODO: [nice to have] use a proper file monitor rather
        -- than this dir exists test
        unless exists $ do
          createDirectoryIfMissingVerbose verbosity True srcrootdir
          unpackPackageTarball
            verbosity
            tarball
            srcrootdir
            pkgid
            pkgTextOverride
          moveTarballShippedDistDirectory
            verbosity
            distDirLayout
            srcrootdir
            pkgid
            dparams
        buildPkg (makeSymbolicPath srcdir) builddir

unpackPackageTarball
  :: Verbosity
  -> FilePath
  -> FilePath
  -> PackageId
  -> Maybe CabalFileText
  -> IO ()
unpackPackageTarball verbosity tarball parentdir pkgid pkgTextOverride =
  -- TODO: [nice to have] switch to tar package and catch tar exceptions
  annotateFailureNoLog UnpackFailed $ do
    -- Unpack the tarball
    --
    info verbosity $ "Extracting " ++ tarball ++ " to " ++ parentdir ++ "..."
    Tar.extractTarGzFile parentdir pkgsubdir tarball

    -- Sanity check
    --
    exists <- doesFileExist cabalFile
    unless exists $
      dieWithException verbosity $
        CabalFileNotFound cabalFile

    -- Overwrite the .cabal with the one from the index, when appropriate
    --
    case pkgTextOverride of
      Nothing -> return ()
      Just pkgtxt -> do
        info verbosity $
          "Updating "
            ++ prettyShow pkgname <.> "cabal"
            ++ " with the latest revision from the index."
        writeFileAtomic cabalFile pkgtxt
  where
    cabalFile :: FilePath
    cabalFile =
      parentdir
        </> pkgsubdir
        </> prettyShow pkgname
        <.> "cabal"
    pkgsubdir = prettyShow pkgid
    pkgname = packageName pkgid

-- | This is a bit of a hacky workaround. A number of packages ship
-- pre-processed .hs files in a dist directory inside the tarball. We don't
-- use the standard 'dist' location so unless we move this dist dir to the
-- right place then we'll miss the shipped pre-processed files. This hacky
-- approach to shipped pre-processed files ought to be replaced by a proper
-- system, though we'll still need to keep this hack for older packages.
moveTarballShippedDistDirectory
  :: Verbosity
  -> DistDirLayout
  -> FilePath
  -> PackageId
  -> DistDirParams
  -> IO ()
moveTarballShippedDistDirectory
  verbosity
  DistDirLayout{distBuildDirectory}
  parentdir
  pkgid
  dparams = do
    distDirExists <- doesDirectoryExist tarballDistDir
    when distDirExists $ do
      debug verbosity $
        "Moving '"
          ++ tarballDistDir
          ++ "' to '"
          ++ targetDistDir
          ++ "'"
      -- TODO: [nice to have] or perhaps better to copy, and use a file monitor
      renameDirectory tarballDistDir targetDistDir
    where
      tarballDistDir = parentdir </> prettyShow pkgid </> "dist"
      targetDistDir = distBuildDirectory dparams
