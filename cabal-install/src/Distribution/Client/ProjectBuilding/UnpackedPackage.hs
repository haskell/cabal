{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module exposes functions to build and register unpacked packages.
--
-- Mainly, unpacked packages are either:
--  * Built and registered in-place
--  * Built and installed
--
-- The two cases differ significantly for there to be a distinction.
-- For instance, we only care about file monitoring and re-building when dealing
-- with "inplace" registered packages, whereas for installed packages we don't.
module Distribution.Client.ProjectBuilding.UnpackedPackage
  ( buildInplaceUnpackedPackage
  , buildAndInstallUnpackedPackage

    -- ** Auxiliary definitions
  , buildAndRegisterUnpackedPackage
  , PackageBuildingPhase

    -- ** Utilities
  , annotateFailure
  , annotateFailureNoLog
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.PackageHash (renderPackageHashInputs)
import Distribution.Client.ProjectBuilding.Types
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad
import Distribution.Client.Store

import Distribution.Client.DistDirLayout
import Distribution.Client.FileMonitor
import Distribution.Client.JobControl
import Distribution.Client.Setup
  ( CommonSetupFlags
  , filterCommonFlags
  , filterConfigureFlags
  , filterHaddockArgs
  , filterHaddockFlags
  , filterTestFlags
  )
import Distribution.Client.SetupWrapper
import Distribution.Client.SourceFiles
import Distribution.Client.SrcDist (allPackageSourceFiles)
import qualified Distribution.Client.Tar as Tar
import Distribution.Client.Types hiding
  ( BuildFailure (..)
  , BuildOutcome
  , BuildOutcomes
  , BuildResult (..)
  )
import Distribution.Client.Utils
  ( ProgressPhase (..)
  , progressMessage
  )

import Distribution.Compat.Lens
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.BuildPaths (haddockDirName)
import Distribution.Simple.Command (CommandUI)
import Distribution.Simple.Compiler
  ( PackageDBStack
  )
import qualified Distribution.Simple.Configure as Cabal
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.LocalBuildInfo
  ( ComponentName (..)
  , LibraryName (..)
  )
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import Distribution.Simple.Program
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Types.BuildType
import Distribution.Types.PackageDescription.Lens (componentModules)

import Distribution.Client.Errors
import Distribution.Compat.Directory (listDirectory)
import Distribution.Simple.Utils
import Distribution.System (Platform (..))
import Distribution.Utils.Path hiding
  ( (<.>)
  , (</>)
  )
import Distribution.Version

import Distribution.Client.ProjectBuilding.PackageFileMonitor

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import qualified Data.List.NonEmpty as NE

import Control.Exception (ErrorCall, Handler (..), SomeAsyncException, assert, catches)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath (dropDrive, normalise, takeDirectory, (<.>), (</>))
import System.IO (Handle, IOMode (AppendMode), withFile)
import System.Semaphore (SemaphoreName (..))

import GHC.Stack
import Web.Browser (openBrowser)

-- | Each unpacked package is processed in the following phases:
--
-- * Configure phase
-- * Build phase
-- * Haddock phase
-- * Install phase (copy + register)
-- * Register phase
-- * Test phase
-- * Bench phase
-- * Repl phase
--
-- Depending on whether we are installing the package or building it inplace,
-- the phases will be carried out differently. For example, when installing,
-- the test, benchmark, and repl phase are ignored.
data PackageBuildingPhase r where
  PBConfigurePhase :: {runConfigure :: IO InLibraryLBI} -> PackageBuildingPhase InLibraryLBI
  PBBuildPhase :: {runBuild :: IO [MonitorFilePath]} -> PackageBuildingPhase ()
  PBHaddockPhase :: {runHaddock :: IO [MonitorFilePath]} -> PackageBuildingPhase ()
  PBInstallPhase
    :: { runCopy :: FilePath -> IO ()
       , runRegister
          :: PackageDBStack
          -> Cabal.RegisterOptions
          -> IO InstalledPackageInfo
       }
    -> PackageBuildingPhase ()
  PBTestPhase :: {runTest :: IO ()} -> PackageBuildingPhase ()
  PBBenchPhase :: {runBench :: IO ()} -> PackageBuildingPhase ()
  PBReplPhase :: {runRepl :: IO ()} -> PackageBuildingPhase ()

-- | Structures the phases of building and registering a package amongst others
-- (see t'PackageBuildingPhase'). Delegates logic specific to a certain
-- building style (notably, inplace vs install) to the delegate function that
-- receives as an argument t'PackageBuildingPhase')
buildAndRegisterUnpackedPackage
  :: Verbosity
  -> DistDirLayout
  -> Maybe SemaphoreName
  -- ^ Whether to pass a semaphore to build process
  -- this is different to BuildTimeSettings because the
  -- name of the semaphore is created freshly each time.
  -> BuildTimeSettings
  -> Lock
  -> Lock
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> ElaboratedReadyPackage
  -> SymbolicPath CWD (Dir Pkg)
  -> SymbolicPath Pkg (Dir Dist)
  -> Maybe FilePath
  -- ^ The path to an /initialized/ log file
  -> (forall r. PackageBuildingPhase r -> IO r)
  -> IO ()
buildAndRegisterUnpackedPackage
  verbosity
  distDirLayout@DistDirLayout{distTempDirectory}
  maybe_semaphore
  buildTimeSettings
  registerLock
  cacheLock
  pkgshared@ElaboratedSharedConfig
    { pkgConfigCompiler = compiler
    , pkgConfigCompilerProgs = progdb
    }
  plan
  rpkg@(ReadyPackage pkg)
  srcdir
  builddir
  mlogFile
  delegate = do
    -- Configure phase
    mbLBI <-
      delegate $
        PBConfigurePhase $
          annotateFailure mlogFile ConfigureFailed $
            setup
              configureCommand
              Cabal.configCommonFlags
              configureFlags
              configureArgs
              (InLibraryArgs $ InLibraryConfigureArgs pkgshared rpkg)

    -- Build phase
    delegate $
      PBBuildPhase $
        annotateFailure mlogFile BuildFailed $
          setup
            buildCommand
            Cabal.buildCommonFlags
            buildFlags
            buildArgs
            (InLibraryArgs $ InLibraryPostConfigureArgs SBuildPhase mbLBI)

    -- Haddock phase
    whenHaddock $
      delegate $
        PBHaddockPhase $
          annotateFailure mlogFile HaddocksFailed $
            setup
              haddockCommand
              Cabal.haddockCommonFlags
              haddockFlags
              haddockArgs
              (InLibraryArgs $ InLibraryPostConfigureArgs SHaddockPhase mbLBI)

    -- Install phase
    delegate $
      PBInstallPhase
        { runCopy = \destdir ->
            annotateFailure mlogFile InstallFailed $
              setup
                Cabal.copyCommand
                Cabal.copyCommonFlags
                (copyFlags destdir)
                copyArgs
                (InLibraryArgs $ InLibraryPostConfigureArgs SCopyPhase mbLBI)
        , runRegister = \pkgDBStack registerOpts ->
            annotateFailure mlogFile InstallFailed $ do
              -- We register ourselves rather than via Setup.hs. We need to
              -- grab and modify the InstalledPackageInfo. We decide what
              -- the installed package id is, not the build system.
              ipkg0 <- generateInstalledPackageInfo mbLBI
              let ipkg = ipkg0{Installed.installedUnitId = uid}
              criticalSection registerLock $
                Cabal.registerPackage
                  verbosity
                  compiler
                  progdb
                  Nothing
                  pkgDBStack
                  ipkg
                  registerOpts
              return ipkg
        }

    -- Test phase
    whenTest $
      delegate $
        PBTestPhase $
          annotateFailure mlogFile TestsFailed $
            setup
              testCommand
              Cabal.testCommonFlags
              testFlags
              testArgs
              (InLibraryArgs $ InLibraryPostConfigureArgs STestPhase mbLBI)

    -- Bench phase
    whenBench $
      delegate $
        PBBenchPhase $
          annotateFailure mlogFile BenchFailed $
            setup
              benchCommand
              Cabal.benchmarkCommonFlags
              benchFlags
              benchArgs
              (InLibraryArgs $ InLibraryPostConfigureArgs SBenchPhase mbLBI)

    -- Repl phase
    whenRepl $
      delegate $
        PBReplPhase $
          annotateFailure mlogFile ReplFailed $
            setupInteractive
              replCommand
              Cabal.replCommonFlags
              replFlags
              replArgs
              (InLibraryArgs $ InLibraryPostConfigureArgs SReplPhase mbLBI)

    return ()
    where
      uid = installedUnitId rpkg

      comp_par_strat = case maybe_semaphore of
        Just sem_name -> Cabal.toFlag (getSemaphoreName sem_name)
        _ -> Cabal.NoFlag

      whenTest action
        | null (elabTestTargets pkg) = return ()
        | otherwise = action

      whenBench action
        | null (elabBenchTargets pkg) = return ()
        | otherwise = action

      whenRepl action
        | null (elabReplTarget pkg) = return ()
        | otherwise = action

      whenHaddock action
        | hasValidHaddockTargets pkg = action
        | otherwise = return ()

      mbWorkDir = useWorkingDir scriptOptions
      commonFlags v =
        flip filterCommonFlags v $
          setupHsCommonFlags verbosity mbWorkDir builddir

      configureCommand = Cabal.configureCommand defaultProgramDb
      configureFlags v =
        flip filterConfigureFlags v $
          setupHsConfigureFlags
            plan
            rpkg
            pkgshared
            (commonFlags v)
      configureArgs _ = setupHsConfigureArgs pkg

      buildCommand = Cabal.buildCommand defaultProgramDb
      buildFlags v = setupHsBuildFlags comp_par_strat pkg pkgshared $ commonFlags v
      buildArgs _ = setupHsBuildArgs pkg

      copyFlags destdir v =
        setupHsCopyFlags
          pkg
          pkgshared
          (commonFlags v)
          destdir
      -- In theory, we could want to copy less things than those that were
      -- built, but instead, we simply copy the targets that were built.
      copyArgs = buildArgs

      testCommand = Cabal.testCommand -- defaultProgramDb
      testFlags v =
        flip filterTestFlags v $
          setupHsTestFlags
            pkg
            (commonFlags v)
      testArgs _ = setupHsTestArgs pkg

      benchCommand = Cabal.benchmarkCommand
      benchFlags v =
        setupHsBenchFlags
          pkg
          pkgshared
          (commonFlags v)
      benchArgs _ = setupHsBenchArgs pkg

      replCommand = Cabal.replCommand defaultProgramDb
      replFlags v =
        setupHsReplFlags
          pkg
          pkgshared
          (commonFlags v)
      replArgs _ = setupHsReplArgs pkg

      haddockCommand = Cabal.haddockCommand
      haddockFlags v =
        flip filterHaddockFlags v $
          setupHsHaddockFlags
            pkg
            pkgshared
            buildTimeSettings
            (commonFlags v)
      haddockArgs v =
        flip filterHaddockArgs v $
          setupHsHaddockArgs pkg

      scriptOptions =
        setupHsScriptOptions
          rpkg
          plan
          pkgshared
          distDirLayout
          srcdir
          builddir
          cacheLock

      setup
        :: (HasCallStack, RightFlagsForPhase flags setupSpec)
        => CommandUI flags
        -> (flags -> CommonSetupFlags)
        -> (Version -> flags)
        -> (Version -> [String])
        -> SetupRunnerArgs setupSpec
        -> IO (SetupRunnerRes setupSpec)
      setup cmd getCommonFlags flags args wrapperArgs =
        withLogging $ \mLogFileHandle ->
          setupWrapper
            verbosity
            scriptOptions
              { useLoggingHandle = mLogFileHandle
              , useExtraEnvOverrides =
                  dataDirsEnvironmentForPlan
                    distDirLayout
                    plan
              }
            (Just (elabPkgDescription pkg))
            cmd
            getCommonFlags
            flags
            args
            wrapperArgs

      setupInteractive
        :: RightFlagsForPhase flags setupSpec
        => CommandUI flags
        -> (flags -> CommonSetupFlags)
        -> (Version -> flags)
        -> (Version -> [String])
        -> SetupRunnerArgs setupSpec
        -> IO (SetupRunnerRes setupSpec)
      setupInteractive =
        setupWrapper
          verbosity
          scriptOptions{isInteractive = True}
          (Just (elabPkgDescription pkg))

      generateInstalledPackageInfo :: InLibraryLBI -> IO InstalledPackageInfo
      generateInstalledPackageInfo mbLBI =
        withTempInstalledPackageInfoFile
          verbosity
          distTempDirectory
          $ \pkgConfDest -> do
            let registerFlags v =
                  setupHsRegisterFlags
                    pkg
                    pkgshared
                    (commonFlags v)
                    pkgConfDest
            setup
              (Cabal.registerCommand)
              Cabal.registerCommonFlags
              registerFlags
              (const [])
              (InLibraryArgs $ InLibraryPostConfigureArgs SRegisterPhase mbLBI)

      withLogging :: (Maybe Handle -> IO r) -> IO r
      withLogging action =
        case mlogFile of
          Nothing -> action Nothing
          Just logFile -> withFile logFile AppendMode (action . Just)

--------------------------------------------------------------------------------

-- * Build Inplace

--------------------------------------------------------------------------------

buildInplaceUnpackedPackage
  :: Verbosity
  -> DistDirLayout
  -> Maybe SemaphoreName
  -> BuildTimeSettings
  -> Lock
  -> Lock
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> ElaboratedReadyPackage
  -> BuildStatusRebuild
  -> SymbolicPath CWD (Dir Pkg)
  -> SymbolicPath Pkg (Dir Dist)
  -> IO BuildResult
buildInplaceUnpackedPackage
  verbosity
  distDirLayout@DistDirLayout
    { distPackageCacheDirectory
    , distDirectory
    , distHaddockOutputDir
    }
  maybe_semaphore
  buildSettings@BuildTimeSettings{buildSettingHaddockOpen}
  registerLock
  cacheLock
  pkgshared@ElaboratedSharedConfig{pkgConfigPlatform = Platform _ os}
  plan
  rpkg@(ReadyPackage pkg)
  buildStatus
  srcdir
  builddir = do
    -- TODO: [code cleanup] there is duplication between the
    --      distdirlayout and the builddir here builddir is not
    --      enough, we also need the per-package cachedir
    createDirectoryIfMissingVerbose verbosity True $ getSymbolicPath builddir
    createDirectoryIfMissingVerbose
      verbosity
      True
      (distPackageCacheDirectory dparams)

    let docsResult = DocsNotTried
        testsResult = TestsNotTried

        buildResult :: BuildResultMisc
        buildResult = (docsResult, testsResult)

    buildAndRegisterUnpackedPackage
      verbosity
      distDirLayout
      maybe_semaphore
      buildSettings
      registerLock
      cacheLock
      pkgshared
      plan
      rpkg
      srcdir
      builddir
      Nothing -- no log file for inplace builds!
      $ \case
        PBConfigurePhase{runConfigure} ->
          whenReconfigure $ do
            mbLBI <- runConfigure
            invalidatePackageRegFileMonitor packageFileMonitor
            updatePackageConfigFileMonitor packageFileMonitor (getSymbolicPath srcdir) pkg
            return mbLBI
        PBBuildPhase{runBuild} -> do
          whenRebuild $ do
            timestamp <- beginUpdateFileMonitor
            monitors' <- runBuild

            let listSimple =
                  execRebuild (getSymbolicPath srcdir) (needElaboratedConfiguredPackage pkg)
                listSdist =
                  fmap (map monitorFileHashed) $
                    allPackageSourceFiles verbosity (getSymbolicPath srcdir)
                ifNullThen m m' = do
                  xs <- m
                  if null xs then m' else return xs
            monitors <- case PD.buildType (elabPkgDescription pkg) of
              Simple -> listSimple
              Hooks -> listSdist `ifNullThen` listSimple
              -- If a Custom setup was used, AND the Cabal is recent
              -- enough to have sdist --list-sources, use that to
              -- determine the files that we need to track.  This can
              -- cause unnecessary rebuilding (for example, if README
              -- is edited, we will try to rebuild) but there isn't
              -- a more accurate Custom interface we can use to get
              -- this info.  We prefer not to use listSimple here
              -- as it can miss extra source files that are considered
              -- by the Custom setup.
              _
                | elabSetupScriptCliVersion pkg >= mkVersion [1, 17] ->
                    -- However, sometimes sdist --list-sources will fail
                    -- and return an empty list.  In that case, fall
                    -- back on the (inaccurate) simple tracking.
                    listSdist `ifNullThen` listSimple
                | otherwise ->
                    listSimple

            let dep_monitors =
                  map monitorFileHashed $
                    elabInplaceDependencyBuildCacheFiles
                      distDirLayout
                      pkgshared
                      plan
                      pkg
            updatePackageBuildFileMonitor
              packageFileMonitor
              (getSymbolicPath srcdir)
              timestamp
              pkg
              buildStatus
              (monitors ++ monitors' ++ dep_monitors)
              buildResult
        PBHaddockPhase{runHaddock} -> do
          _monitors <- runHaddock
          let haddockTarget = elabHaddockForHackage pkg
          when (haddockTarget == Cabal.ForHackage) $ do
            let dest = distDirectory </> name <.> "tar.gz"
                name = haddockDirName haddockTarget (elabPkgDescription pkg)
                docDir =
                  distBuildDirectory distDirLayout dparams
                    </> "doc"
                    </> "html"
            Tar.createTarGzFile dest docDir name
            notice verbosity $ "Documentation tarball created: " ++ dest

          when (buildSettingHaddockOpen && haddockTarget /= Cabal.ForHackage) $ do
            let dest = docDir </> "index.html"
                name = haddockDirName haddockTarget (elabPkgDescription pkg)
                docDir = case distHaddockOutputDir of
                  Nothing -> distBuildDirectory distDirLayout dparams </> "doc" </> "html" </> name
                  Just dir -> dir
            catch
              (void $ openBrowser dest)
              ( \(_ :: ErrorCall) ->
                  dieWithException verbosity $
                    FindOpenProgramLocationErr $
                      "Unsupported OS: " <> show os
              )
        PBInstallPhase{runCopy = _runCopy, runRegister} -> do
          -- PURPOSELY omitted: no copy!

          whenReRegister $ do
            -- Register locally
            mipkg <-
              if elabRequiresRegistration pkg
                then do
                  ipkg <-
                    runRegister
                      (elabRegisterPackageDBStack pkg)
                      Cabal.defaultRegisterOptions
                  return (Just ipkg)
                else return Nothing

            updatePackageRegFileMonitor packageFileMonitor (getSymbolicPath srcdir) mipkg
        PBTestPhase{runTest} -> runTest
        PBBenchPhase{runBench} -> runBench
        PBReplPhase{runRepl} -> runRepl

    return
      BuildResult
        { buildResultDocs = docsResult
        , buildResultTests = testsResult
        , buildResultLogFile = Nothing
        }
    where
      dparams = elabDistDirParams pkgshared pkg

      packageFileMonitor = newPackageFileMonitor pkgshared distDirLayout dparams

      whenReconfigure :: IO InLibraryLBI -> IO InLibraryLBI
      whenReconfigure action =
        case buildStatus of
          BuildStatusConfigure _ -> action
          _ -> do
            lbi_wo_programs <- Cabal.getPersistBuildConfig (Just srcdir) builddir
            -- Restore info about unconfigured programs, since it is not serialized
            -- TODO: copied from Distribution.Simple.getBuildConfig.
            let lbi =
                  lbi_wo_programs
                    { Cabal.withPrograms =
                        restoreProgramDb
                          builtinPrograms
                          (Cabal.withPrograms lbi_wo_programs)
                    }
            return $ InLibraryLBI lbi

      whenRebuild, whenReRegister :: IO () -> IO ()
      whenRebuild action
        | null (elabBuildTargets pkg)
        , -- NB: we have to build the test/bench suite!
          null (elabTestTargets pkg)
        , null (elabBenchTargets pkg) =
            return ()
        | otherwise = action

      whenReRegister action =
        case buildStatus of
          -- We registered the package already
          BuildStatusBuild (Just _) _ ->
            info verbosity "whenReRegister: previously registered"
          -- There is nothing to register
          _
            | null (elabBuildTargets pkg) ->
                info verbosity "whenReRegister: nothing to register"
            | otherwise -> action

--------------------------------------------------------------------------------

-- * Build and Install

--------------------------------------------------------------------------------

buildAndInstallUnpackedPackage
  :: Verbosity
  -> DistDirLayout
  -> StoreDirLayout
  -> Maybe SemaphoreName
  -- ^ Whether to pass a semaphore to build process
  -- this is different to BuildTimeSettings because the
  -- name of the semaphore is created freshly each time.
  -> BuildTimeSettings
  -> Lock
  -> Lock
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> ElaboratedReadyPackage
  -> SymbolicPath CWD (Dir Pkg)
  -> SymbolicPath Pkg (Dir Dist)
  -> IO BuildResult
buildAndInstallUnpackedPackage
  verbosity
  distDirLayout
  storeDirLayout@StoreDirLayout
    { storePackageDBStack
    }
  maybe_semaphore
  buildSettings@BuildTimeSettings{buildSettingNumJobs, buildSettingLogFile}
  registerLock
  cacheLock
  pkgshared@ElaboratedSharedConfig
    { pkgConfigCompiler = compiler
    , pkgConfigPlatform = platform
    }
  plan
  rpkg@(ReadyPackage pkg)
  srcdir
  builddir = do
    createDirectoryIfMissingVerbose verbosity True (interpretSymbolicPath (Just srcdir) builddir)

    -- TODO: [code cleanup] deal consistently with talking to older
    --      Setup.hs versions, much like we do for ghc, with a proper
    --      options type and rendering step which will also let us
    --      call directly into the lib, rather than always going via
    --      the lib's command line interface, which would also allow
    --      passing data like installed packages, compiler, and
    --      program db for a quicker configure.

    -- TODO: [required feature] docs and tests
    -- TODO: [required feature] sudo re-exec

    initLogFile

    buildAndRegisterUnpackedPackage
      verbosity
      distDirLayout
      maybe_semaphore
      buildSettings
      registerLock
      cacheLock
      pkgshared
      plan
      rpkg
      srcdir
      builddir
      mlogFile
      $ \case
        PBConfigurePhase{runConfigure} -> do
          noticeProgress ProgressStarting
          runConfigure
        PBBuildPhase{runBuild} -> do
          noticeProgress ProgressBuilding
          _monitors <- runBuild
          return ()
        PBHaddockPhase{runHaddock} -> do
          noticeProgress ProgressHaddock
          _monitors <- runHaddock
          return ()
        PBInstallPhase{runCopy, runRegister} -> do
          noticeProgress ProgressInstalling

          let registerPkg
                | not (elabRequiresRegistration pkg) =
                    debug verbosity $
                      "registerPkg: elab does NOT require registration for "
                        ++ prettyShow uid
                | otherwise = do
                    assert
                      ( elabRegisterPackageDBStack pkg
                          == storePackageDBStack compiler (elabPackageDbs pkg)
                      )
                      (return ())
                    _ <-
                      runRegister
                        (elabRegisterPackageDBStack pkg)
                        Cabal.defaultRegisterOptions
                          { Cabal.registerMultiInstance = True
                          , Cabal.registerSuppressFilesCheck = True
                          }
                    return ()

          -- Actual installation
          void $
            newStoreEntry
              verbosity
              storeDirLayout
              compiler
              uid
              (copyPkgFiles verbosity pkgshared pkg runCopy)
              registerPkg

        -- No tests on install
        PBTestPhase{} -> return ()
        -- No bench on install
        PBBenchPhase{} -> return ()
        -- No repl on install
        PBReplPhase{} -> return ()

    -- TODO: [nice to have] we currently rely on Setup.hs copy to do the right
    -- thing. Although we do copy into an image dir and do the move into the
    -- final location ourselves, perhaps we ought to do some sanity checks on
    -- the image dir first.

    -- TODO: [required eventually] note that for nix-style
    -- installations it is not necessary to do the
    -- 'withWin32SelfUpgrade' dance, but it would be necessary for a
    -- shared bin dir.

    -- TODO: [required feature] docs and test phases
    let docsResult = DocsNotTried
        testsResult = TestsNotTried

    noticeProgress ProgressCompleted

    return
      BuildResult
        { buildResultDocs = docsResult
        , buildResultTests = testsResult
        , buildResultLogFile = mlogFile
        }
    where
      uid = installedUnitId rpkg
      pkgid = packageId rpkg

      dispname :: String
      dispname = case elabPkgOrComp pkg of
        -- Packages built altogether, instead of per component
        ElabPackage ElaboratedPackage{pkgWhyNotPerComponent} ->
          prettyShow pkgid
            ++ " (all, legacy fallback: "
            ++ unwords (map whyNotPerComponent $ NE.toList pkgWhyNotPerComponent)
            ++ ")"
        -- Packages built per component
        ElabComponent comp ->
          prettyShow pkgid
            ++ " ("
            ++ maybe "custom" prettyShow (compComponentName comp)
            ++ ")"

      noticeProgress :: ProgressPhase -> IO ()
      noticeProgress phase =
        when (isParallelBuild buildSettingNumJobs) $
          progressMessage verbosity phase dispname

      mlogFile :: Maybe FilePath
      mlogFile =
        case buildSettingLogFile of
          Nothing -> Nothing
          Just mkLogFile -> Just (mkLogFile compiler platform pkgid uid)

      initLogFile :: IO ()
      initLogFile =
        case mlogFile of
          Nothing -> return ()
          Just logFile -> do
            createDirectoryIfMissing True (takeDirectory logFile)
            exists <- doesFileExist logFile
            when exists $ removeFile logFile

-- | The copy part of the installation phase when doing build-and-install
copyPkgFiles
  :: Verbosity
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> (FilePath -> IO ())
  -- ^ The 'runCopy' function which invokes ./Setup copy for the
  -- given filepath
  -> FilePath
  -- ^ The temporary dir file path
  -> IO (FilePath, [FilePath])
copyPkgFiles verbosity pkgshared pkg runCopy tmpDir = do
  let tmpDirNormalised = normalise tmpDir
  runCopy tmpDirNormalised
  -- Note that the copy command has put the files into
  -- @$tmpDir/$prefix@ so we need to return this dir so
  -- the store knows which dir will be the final store entry.
  let prefix =
        normalise $
          dropDrive (InstallDirs.prefix (elabInstallDirs pkg))
      entryDir = tmpDirNormalised </> prefix

  -- if there weren't anything to build, it might be that directory is not created
  -- the @setup Cabal.copyCommand@ above might do nothing.
  -- https://github.com/haskell/cabal/issues/4130
  createDirectoryIfMissingVerbose verbosity True entryDir

  let hashFileName = entryDir </> "cabal-hash.txt"
      outPkgHashInputs = renderPackageHashInputs (packageHashInputs pkgshared pkg)

  info verbosity $
    "creating file with the inputs used to compute the package hash: " ++ hashFileName

  LBS.writeFile hashFileName outPkgHashInputs

  debug verbosity "Package hash inputs:"
  traverse_
    (debug verbosity . ("> " ++))
    (lines $ LBS.Char8.unpack outPkgHashInputs)

  -- Ensure that there are no files in `tmpDir`, that are
  -- not in `entryDir`. While this breaks the
  -- prefix-relocatable property of the libraries, it is
  -- necessary on macOS to stay under the load command limit
  -- of the macOS mach-o linker. See also
  -- @PackageHash.hashedInstalledPackageIdVeryShort@.
  --
  -- We also normalise paths to ensure that there are no
  -- different representations for the same path. Like / and
  -- \\ on windows under msys.
  otherFiles <-
    filter (not . isPrefixOf entryDir)
      <$> listFilesRecursive tmpDirNormalised
  -- Here's where we could keep track of the installed files
  -- ourselves if we wanted to by making a manifest of the
  -- files in the tmp dir.
  return (entryDir, otherFiles)
  where
    listFilesRecursive :: FilePath -> IO [FilePath]
    listFilesRecursive path = do
      files <- fmap (path </>) <$> (listDirectory path)
      allFiles <- for files $ \file -> do
        isDir <- doesDirectoryExist file
        if isDir
          then listFilesRecursive file
          else return [file]
      return (concat allFiles)

--------------------------------------------------------------------------------

-- * Exported Utils

--------------------------------------------------------------------------------

{- FOURMOLU_DISABLE -}
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

--------------------------------------------------------------------------------
-- * Other Utils
--------------------------------------------------------------------------------

hasValidHaddockTargets :: ElaboratedConfiguredPackage -> Bool
hasValidHaddockTargets ElaboratedConfiguredPackage{..}
  | not elabBuildHaddocks = False
  | otherwise = any componentHasHaddocks components
  where
    components :: [ComponentTarget]
    components =
      elabBuildTargets
        ++ elabTestTargets
        ++ elabBenchTargets
        ++ elabReplTarget
        ++ elabHaddockTargets

    componentHasHaddocks :: ComponentTarget -> Bool
    componentHasHaddocks (ComponentTarget name _) =
      case name of
        CLibName LMainLibName -> hasHaddocks
        CLibName (LSubLibName _) -> elabHaddockInternal && hasHaddocks
        CFLibName _ -> elabHaddockForeignLibs && hasHaddocks
        CExeName _ -> elabHaddockExecutables && hasHaddocks
        CTestName _ -> elabHaddockTestSuites && hasHaddocks
        CBenchName _ -> elabHaddockBenchmarks && hasHaddocks
      where
        hasHaddocks = not (null (elabPkgDescription ^. componentModules name))

withTempInstalledPackageInfoFile
  :: Verbosity
  -> FilePath
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
    pkgConfParseFailed :: String -> IO a
    pkgConfParseFailed perror =
      dieWithException verbosity $ PkgConfParseFailed perror

    readPkgConf :: FilePath -> FilePath -> IO InstalledPackageInfo
    readPkgConf pkgConfDir pkgConfFile = do
      pkgConfStr <- BS.readFile (pkgConfDir </> pkgConfFile)
      (warns, ipkg) <- case Installed.parseInstalledPackageInfo pkgConfStr of
        Left perrors -> pkgConfParseFailed $ unlines $ NE.toList perrors
        Right (warns, ipkg) -> return (warns, ipkg)

      unless (null warns) $
        warn verbosity $
          unlines warns

      return ipkg

