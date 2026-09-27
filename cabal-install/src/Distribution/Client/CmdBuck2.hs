-- | cabal-install CLI command: buck2
--
-- Sets up (or refreshes) a buck2 build for the current project, using the
-- prelude and support scripts checked out at @buck2\/@ (a checkout of
-- <https://github.com/simonmar/haskell-buck2>, see @buck2\/README.md@):
--
--   1. Build every dependency (never the local packages themselves), the
--      same as @cabal build all --only-dependencies@ would - by resolving
--      and pruning the install plan exactly as 'CmdBuild.buildAction'
--      does (reusing its 'CmdBuild.selectPackageTargets'\/
--      'CmdBuild.selectComponentTarget'), so every flag @cabal build@\/
--      @cabal configure@ understands (@-f...@, @--enable-profiling@,
--      @--enable-tests@, ...) works here too. Doing this in-process,
--      rather than delegating to 'CmdBuild.buildAction' as an opaque
--      action, is what lets step 3 below see exactly the same
--      (test\/benchmark-flag-aware) dependency closure that just got
--      built - see "Distribution.Client.Buck2.Prebuilt".
--   2. Create @.buckconfig@\/@PACKAGE@ if they don't exist yet (copied
--      verbatim from @buck2\/example@).
--   3. Turn the resolved dependency closure into @third-party\/haskell@ -
--      see "Distribution.Client.Buck2.Prebuilt".
--   4. Generate a @BUCK.cabal.bzl@ (and, where missing, a @BUCK@) for
--      every local package - see "Distribution.Client.Buck2.Generate".
module Distribution.Client.CmdBuck2
  ( buck2Command
  , buck2Action
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.Concurrent.Async as Async
import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Concurrent.STM
  ( STM
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , retry
  , writeTVar
  )

import qualified Distribution.Client.CmdBuild as CmdBuild
import Distribution.Client.CmdErrorMessages (renderCannotPruneDependencies, reportTargetProblems)
import qualified Distribution.Client.InLibrary as InLibrary
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Client.DistDirLayout
  ( DistDirLayout (distBuildDirectory, distProjectRootDirectory, distUnpackedSrcDirectory)
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectOrchestration
-- 'pruneInstallPlanToTargets' is hidden: 'ProjectOrchestration' re-exports
-- its own wrapper of the same name (taking a 'TargetsMap' directly,
-- matching what 'resolveTargetsFromSolver' below returns), which would
-- otherwise be ambiguous with 'ProjectPlanning's lower-level original.
import Distribution.Client.ProjectPlanning hiding (pruneInstallPlanToTargets)
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedPackageOrComponent (ElabComponent, ElabPackage)
  , elabComponentName
  , elabDistDirParams
  , elabExeDependencyPaths
  , elabOrderLibDependencies
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( GlobalFlags
  , InstallFlags (installOnlyDeps)
  )
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Client.Types.ReadyPackage (GenericReadyPackage (ReadyPackage))
import Distribution.Client.Utils (numberOfProcessors)

import qualified Distribution.PackageDescription as PD
import Distribution.Package (HasUnitId (installedUnitId), PackageName, packageId, packageName)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.Compiler (PackageDBX (GlobalPackageDB))
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Command (CommandUI (..), usageAlternatives)
import Distribution.Simple.Flag (toFlag)
import Distribution.Simple.Program.Builtin (builtinPrograms)
import Distribution.Simple.Program.Db (prependProgramSearchPathNoLogging, restoreProgramDb)
import Distribution.Simple.Register (generateRegistrationInfo)
import Distribution.Simple.Utils (dieWithException, notice)
import Distribution.Types.Component (componentName)
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Types.LocalBuildInfo
  ( LocalBuildInfo
  , componentNameCLBIs
  , distPrefLBI
  , relocatable
  )
import Distribution.Types.UnitId (UnitId)
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity (defaultVerbosityHandles, normal)

import System.Directory (canonicalizePath)
import System.FilePath ((</>))

import Distribution.Client.Buck2.Generate (generateAllPackages)
import Distribution.Client.Buck2.Prebuilt (generatePrebuilt)
import Distribution.Client.Buck2.Setup
  ( checkBuck2Prelude
  , ensureBuckconfigAndPackage
  )
import Distribution.Client.Errors
  ( CabalInstallException (Buck2ActionExtraArgs, Buck2NonLocalPackageLocation, ReportCannotPruneDependencies)
  )

buck2Command :: CommandUI (NixStyleFlags ())
buck2Command =
  CommandUI
    { commandName = "buck2"
    , commandSynopsis = "Set up (or refresh) a buck2 build for this project."
    , commandUsage = usageAlternatives "buck2" ["[FLAGS]"]
    , commandDescription = Just $ \_ ->
        "Builds every dependency of the project (as `cabal build all "
          ++ "--only-dependencies` would), then generates the buck2 build "
          ++ "files (.buckconfig, PACKAGE, third-party/haskell, and a "
          ++ "BUCK.cabal.bzl for each local package) needed to build the "
          ++ "project with buck2 instead of cabal. Requires a checkout of "
          ++ "https://github.com/simonmar/haskell-buck2 at ./buck2. See "
          ++ "buck2/README.md for details.\n\n"
          ++ "Flags that would normally be passed to `cabal build`/`cabal "
          ++ "configure` (-f, --enable-profiling, --enable-tests, etc.) are "
          ++ "honoured here too, and apply to the dependency build."
    , commandNotes = Nothing
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = nixStyleOptions (const [])
    }

buck2Action :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
buck2Action flags extraArgs globalFlags = do
  unless (null extraArgs) $
    dieWithException verbosity (Buck2ActionExtraArgs extraArgs)

  withContextAndSelectors verbosity RejectNoTargets Nothing depsFlags ["all"] globalFlags BuildCommand $
    \targetCtx ctx targetSelectors -> do
      baseCtx <- case targetCtx of
        ProjectContext -> return ctx
        GlobalContext -> return ctx
        ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

      let projectRoot = distProjectRootDirectory (distDirLayout baseCtx)
      checkBuck2Prelude verbosity projectRoot

      -- The same target resolution + pruning 'CmdBuild.buildAction ["all"]
      -- --only-dependencies' does, inlined here (rather than delegated to
      -- it as an opaque action) so 'elaboratedPlanToExecute' below - the
      -- exact, test/benchmark-flag-aware dependency closure that's about
      -- to be built - stays in hand for 'generatePrebuilt'.
      buildCtx@ProjectBuildContext{elaboratedPlanOriginal, elaboratedPlanToExecute, elaboratedShared} <-
        runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
          targets <-
            either (reportTargetProblems verbosity "buck2") return $
              resolveTargetsFromSolver
                CmdBuild.selectPackageTargets
                CmdBuild.selectComponentTarget
                elaboratedPlan
                Nothing
                targetSelectors
          let elaboratedPlan' = pruneInstallPlanToTargets TargetActionBuild targets elaboratedPlan
          elaboratedPlan'' <-
            either (dieWithException verbosity . ReportCannotPruneDependencies . renderCannotPruneDependencies) return $
              pruneToDependenciesNeeded (Map.keysSet targets) elaboratedPlan'
          return (elaboratedPlan'', targets)

      notice verbosity "cabal buck2: building dependencies (cabal build all --only-dependencies)"
      printPlan verbosity baseCtx buildCtx
      buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
      runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

      ensureBuckconfigAndPackage verbosity projectRoot
      resolvedDeps <-
        generatePrebuilt
          verbosity
          projectRoot
          (cabalDirLayout baseCtx)
          (distDirLayout baseCtx)
          elaboratedShared
          elaboratedPlanToExecute

      -- Every genuinely local package, *plus* every non-local one
      -- whose own build was forced 'inplace' by depending on
      -- one. When a non-local package is forced inplace we must
      -- include it in the set of buck2-built packages, otherwise the
      -- build will contain multiple incompatible versions of the
      -- local dependency. A real-world example of this is
      -- hackage-security in the cabal project, which is not a local
      -- package but depends on the local Cabal-syntax.
      localPkgs <-
        sequenceA
          [ do
              dir <- packageSourceDir verbosity (distDirLayout baseCtx) elab
              return (dir, elabPkgDescription elab)
          | InstallPlan.Configured elab <- InstallPlan.toList elaboratedPlanOriginal
          , elabLocalToProject elab || elabBuildStyle elab /= BuildAndInstall
          ]

      -- A real, correctly-versioned 'InstalledPackageIndex' covering the
      -- whole resolved dependency closure - 'generatePrebuilt' already
      -- did the work of finding and parsing every real @.conf@ file, so
      -- this is free (no second walk of the store\/global\/inplace
      -- package dbs).
      let installedIndex :: InstalledPackageIndex
          installedIndex = PackageIndex.fromList resolvedDeps

      -- A real 'LocalBuildInfo' for every local (or quasi-local, per
      -- 'localPkgs's own comment above) *component*, computed
      -- concurrently - see 'configureComponentsConcurrently' for why
      -- this needs to be concurrent at all, and how it stays correct
      -- while being so.
      componentLBIs <- configureComponentsConcurrently verbosity (distDirLayout baseCtx) elaboratedPlanOriginal elaboratedShared installedIndex

      -- Per-component elaboration gives each local package one
      -- 'ElaboratedConfiguredPackage' per component (library, executable,
      -- ...), all sharing the same directory and the same (whole-package)
      -- 'PackageDescription' - so without this, a package with N
      -- buildable components would get regenerated N times over.
      generateAllPackages verbosity projectRoot componentLBIs (nubBy ((==) `on` fst) localPkgs)

      notice verbosity $
        unlines
          [ "cabal buck2: done. You can now:"
          , "    buck2 build //...          # build everything"
          , "    buck2 test //...           # test everything"
          , "    buck2 build //... -m opt   # build everything in opt mode"
          ]
  where
    verbosity = cfgVerbosity normal flags
    depsFlags = flags{installFlags = (installFlags flags){installOnlyDeps = toFlag True}}

localPackageDir :: Verbosity -> ElaboratedConfiguredPackage -> IO FilePath
localPackageDir verbosity elab = case elabPkgSourceLocation elab of
  LocalUnpackedPackage dir -> return dir
  _ -> dieWithException verbosity (Buck2NonLocalPackageLocation (prettyShow (packageId elab)))

-- | Real on-disk source directory for any package this run is going to
-- generate a buck2 rule for - a genuinely local one (always
-- 'LocalUnpackedPackage'; delegates to 'localPackageDir') or an inplace
-- non-local one, resolved the same way
-- 'Distribution.Client.ProjectPlanning.Types.dataDirEnvVarForPackage'
-- does for the same 'BuildInplaceOnly' case: a plain source checkout
-- uses its own path directly, anything fetched as a tarball\/repo was
-- already unpacked to 'distUnpackedSrcDirectory' to be built inplace in
-- the first place.
packageSourceDir :: Verbosity -> DistDirLayout -> ElaboratedConfiguredPackage -> IO FilePath
packageSourceDir verbosity distDirLayout elab
  | elabLocalToProject elab = localPackageDir verbosity elab
  | otherwise = case elabPkgSourceLocation elab of
      LocalUnpackedPackage dir -> return dir
      LocalTarballPackage{} -> return unpackedPath
      RemoteTarballPackage{} -> return unpackedPath
      RepoTarballPackage{} -> return unpackedPath
      RemoteSourceRepoPackage _ (Just localCheckout) -> return localCheckout
      RemoteSourceRepoPackage{} -> dieWithException verbosity (Buck2NonLocalPackageLocation (prettyShow (packageId elab)))
  where
    unpackedPath = distUnpackedSrcDirectory distDirLayout (elabPkgSourceId elab)

-- | A real 'LocalBuildInfo' for one local (or quasi-local) *component*,
-- computed the same way a real build does - via
-- "Distribution.Client.InLibrary", which wraps Cabal's own
-- 'Distribution.Simple.Configure.configureFinal' - rather than
-- hand-assembling the pieces 'Distribution.Simple.Build.Macros.
-- generateCabalMacrosHeader'\/'Distribution.Simple.Build.PathsModule.
-- generatePathsModule' need. @elab@'s own 'elabPkgOrComp' determines
-- which single component gets configured here (matching real Cabal:
-- per-component elaboration means one 'ElaboratedConfiguredPackage' -
-- and so one call here - per component, not per package).
localBuildInfoFor
  :: Verbosity
  -> DistDirLayout
  -> ElaboratedInstallPlan
  -> ElaboratedSharedConfig
  -> InstalledPackageIndex
  -> ElaboratedConfiguredPackage
  -> IO LocalBuildInfo
localBuildInfoFor verbosity distDirLayout plan shared ipi elab = do
  -- Real Cabal's own 'InLibrary.configure' falls back to *searching* the
  -- working directory for a @<pkgname>.cabal@ file whenever
  -- 'Cabal.configCabalFilePath' isn't set (see its own use of
  -- 'tryFindPackageDesc') - so this has to be the package's own source
  -- directory, matching 'setupHsScriptOptions''s own @srcdir@ in the
  -- real build path ("Distribution.Client.ProjectBuilding.UnpackedPackage"),
  -- not the buck2 command's actual cwd (the project root), or this fails
  -- outright with "No cabal file found" for every package but one that
  -- happens to be sitting at the project root itself.
  pkgDir <- packageSourceDir verbosity distDirLayout elab
  let verbHandles = defaultVerbosityHandles
      -- Builtin preprocessors (alex, happy, hsc2hs, ...) restored as
      -- known-but-unconfigured programs, and the compiler's own
      -- already-configured programs - the same starting point a real
      -- build's own 'Distribution.Client.SetupWrapper' constructs (see
      -- its own comment "Note [Constructing the ProgramDb]") - plus
      -- 'elabExeDependencyPaths'\/'elabProgramPathExtra' prepended onto
      -- its search path. That part isn't optional the way the rest of
      -- "Note [Constructing the ProgramDb]"'s extra layering is: a
      -- component with e.g. @build-tool-depends: alex:alex@ needs
      -- 'InLibrary.configure' below to be able to find *this* build's
      -- own just-built @alex@ (never on a bare @$PATH@ - it only exists
      -- under @dist-newstyle@) and query its version, the same way real
      -- Cabal's own subprocess-based configure does via
      -- 'setupHsScriptOptions''s @useExtraPathEnv@ - just via a search
      -- path prepend instead of a subprocess's environment, since this
      -- runs in-process.
      progDb =
        prependProgramSearchPathNoLogging
          (elabExeDependencyPaths elab ++ elabProgramPathExtra elab)
          []
          (restoreProgramDb builtinPrograms (pkgConfigCompilerProgs shared))
      buildType = PD.buildType (elabPkgDescription elab)
      inputs =
        InLibrary.libraryConfigureInputsFromElabPackage
          verbHandles
          buildType
          progDb
          shared
          (ReadyPackage elab)
          ipi
          []
      builddir = makeSymbolicPath (distBuildDirectory distDirLayout (elabDistDirParams shared elab) </> "build")
      commonFlags = setupHsCommonFlags verbosity (Just (makeSymbolicPath pkgDir)) builddir [] False
  cfg <-
    setupHsConfigureFlags
      (fmap makeSymbolicPath . canonicalizePath)
      plan
      (ReadyPackage elab)
      shared
      commonFlags
  InLibrary.configure inputs cfg

-- | If @cname@ names a library component, produce the real, in-place
-- 'InstalledPackageInfo' for it - the same info a real @Setup register@
-- would write to @package.conf.inplace@ after building it, without
-- actually writing anything anywhere. Real Cabal's own
-- 'generateRegistrationInfo', in its in-place branch, needs no built
-- object code to do this: an in-place package's ABI hash is always the
-- fixed placeholder @"inplace"@ (see its own haddock), so this is safe
-- to call immediately after 'localBuildInfoFor' configures the
-- component, before anything is actually compiled. Deliberately doesn't
-- take (or update) an 'InstalledPackageIndex' itself, unlike an earlier
-- version of this function - 'configureComponentsConcurrently' calls
-- this from multiple threads at once, and folding a growing index
-- through a sequence of calls only makes sense single-threaded; the
-- caller is responsible for inserting the result into a shared index
-- itself (atomically).
--
-- Every other kind of component (executable\/test-suite\/benchmark) is
-- skipped ('Nothing'): nothing ever depends on one of those by
-- 'UnitId', so they have nothing to contribute to the index.
libraryInstalledPackageInfo :: Verbosity -> LocalBuildInfo -> PackageDescription -> ComponentName -> IO (Maybe InstalledPackageInfo)
libraryInstalledPackageInfo verbosity lbi pkgDesc cname = case cname of
  CLibName ln
    | Just lib <- listToMaybe [l | l <- PD.allLibraries pkgDesc, PD.libName l == ln]
    , (clbi : _) <- componentNameCLBIs lbi cname ->
        Just <$> generateRegistrationInfo verbosity pkgDesc lib lbi clbi True (relocatable lbi) (distPrefLBI lbi) GlobalPackageDB
  _ -> return Nothing

-- | The buildable component names for one elaborated node - either the
-- single component 'elabComponentName' itself names (per-component
-- elaboration, @ElabComponent@), or *every* buildable component of the
-- whole package it configured (whole-package elaboration,
-- @ElabPackage@ - see 'elabComponentName's own haddock, "there could be
-- more, but default this": one @configureFinal@ call in that mode
-- genuinely produces a 'ComponentLocalBuildInfo' for every component of
-- the package internally, regardless of which single one
-- 'elabComponentName' defaults to).
componentNamesFor :: ElaboratedConfiguredPackage -> PackageDescription -> [ComponentName]
componentNamesFor elab pkgDesc = case elabPkgOrComp elab of
  ElabComponent _ -> maybeToList (elabComponentName elab)
  ElabPackage _ -> [componentName comp | comp <- PD.pkgBuildableComponents pkgDesc]

-- | 'localBuildInfoFor' (and the library registration that has to
-- happen right after it, via 'libraryInstalledPackageInfo') for every
-- local (or quasi-local) component in the plan, run concurrently: each
-- call is a real Cabal 'Distribution.Simple.Configure.configureFinal',
-- genuine CPU-bound work, and a large project (Glean, say, with
-- hundreds of components spread across dozens of packages - a single
-- package there can have 30+ test-suites) made doing this one
-- component at a time the dominant cost of the whole @cabal buck2@
-- command.
--
-- The only real ordering constraint is: a component can't be
-- configured until every *local library* it depends on has already
-- been configured *and registered* into the 'InstalledPackageIndex'
-- 'localBuildInfoFor' is given - the same constraint the old sequential
-- fold enforced by processing the whole plan in
-- 'InstallPlan.reverseTopologicalOrder', one component at a time. That
-- full topological order is far stronger than what's actually needed:
-- most components in a real project (executables, test-suites, ...)
-- don't depend on each other at all, only on a handful of libraries -
-- so scheduling by *direct local-library dependency* (via
-- 'elabOrderLibDependencies') lets every component whose own library
-- dependencies are already satisfied run immediately, concurrently with
-- everything else at the same point in the graph, not just within one
-- "wave" of a full topological sort.
--
-- Implemented as a small hand-rolled STM worker pool, *not* via
-- "Control.Concurrent.Stream" (an earlier version of this function used
-- 'Stream.stream'/'Stream.streamWithOutput' - see the git history if
-- curious): that module is built around a single, sequential producer
-- enumerating a statically-known worklist up front, and silently breaks
-- when @write@ is called from multiple concurrent threads reacting to
-- dynamically-discovered work the way this function needs to. Confirmed
-- concretely, not just suspected, by reproducing a real, silent bug it
-- caused here: stress-testing this exact function against the cabal
-- repo (15+ consecutive `cabal buck2` runs) intermittently generated a
-- `cabal-install/BUCK.cabal.bzl` missing 3 of its 7 components, with no
-- error or warning anywhere. Root cause: 'Stream.stream_'s termination
-- protocol has its single producer decide "everything has been
-- submitted" from *its own* bookkeeping, then immediately flood the
-- queue with @maxConcurrency@ end-of-work markers; a worker thread that
-- has *decided* a dependent is now ready (updating shared "is this
-- submitted yet" state) but hasn't yet *physically* called @write@ for
-- it - a real, unavoidable gap between those two steps once @write@
-- itself is being called from worker threads too, not just the
-- producer - can lose the race: the producer sees its own bookkeeping
-- satisfied, floods the end markers, and every worker thread exits
-- (each on seeing its own marker) before that not-yet-written item ever
-- reaches the queue. It never re-appears anywhere; the run simply
-- finishes short.
--
-- The fix is to make "this component is now ready" and "a worker can
-- now see it" the *same* atomic step, which means owning the ready
-- queue directly instead of going through an opaque library callback:
-- 'readyVar' is that queue, and 'finishNode' - called once a worker
-- finishes a component - updates dependency counts *and* pushes any
-- newly-ready dependents onto it within one STM transaction. A worker
-- only ever gives up (letting 'popReady' return 'Nothing') once
-- 'processedVar' - a plain *count of finished components* - has reached
-- 'totalNodes': at that point every 'finishNode' call that could ever
-- add something new to 'readyVar' has already happened, so it's
-- genuinely safe to stop, not just probably-safe the way
-- 'Stream.stream_'s own submitted-count check turned out to be. An
-- empty queue with work still outstanding elsewhere correctly makes a
-- worker 'retry' (STM's blocking retry, which wakes automatically the
-- moment 'readyVar' or 'processedVar' next changes) rather than give up
-- early.
configureComponentsConcurrently
  :: Verbosity
  -> DistDirLayout
  -> ElaboratedInstallPlan
  -> ElaboratedSharedConfig
  -> InstalledPackageIndex
  -> IO (Map (PackageName, ComponentName) LocalBuildInfo)
configureComponentsConcurrently verbosity distDirLayout plan shared installedIndex = do
  -- cabal-install's own build parallelism doesn't need extra RTS
  -- capabilities (it's almost entirely "spawn ghc, block on it", and a
  -- blocked foreign call already releases its capability under the
  -- threaded RTS) - but 'localBuildInfoFor' below does real, in-Haskell
  -- CPU work per call, which *does* need more than one capability to
  -- actually run in parallel. Only ever raises the cap (never lowers an
  -- explicit @+RTS -N@ the user already asked for).
  numCaps <- getNumCapabilities
  when (numCaps < numberOfProcessors) $
    setNumCapabilities numberOfProcessors

  let localElabs :: Map UnitId ElaboratedConfiguredPackage
      localElabs =
        Map.fromList
          [ (elabUnitId elab, elab)
          | InstallPlan.Configured elab <- InstallPlan.toList plan
          , elabLocalToProject elab || elabBuildStyle elab /= BuildAndInstall
          ]

      -- Direct *local*-library dependencies only - see this function's
      -- own haddock for why neither an external dependency (already in
      -- 'installedIndex' before this starts) nor a non-library local
      -- dependency (nothing ever depends on one by 'UnitId') is a real
      -- scheduling constraint here.
      localLibDeps :: UnitId -> [UnitId]
      localLibDeps uid =
        [ dep
        | Just elab <- [Map.lookup uid localElabs]
        , dep <- elabOrderLibDependencies elab
        , dep `Map.member` localElabs
        ]

      -- Reverse adjacency: for each local library, the components that
      -- become eligible to run the moment *it* finishes.
      dependents :: Map UnitId [UnitId]
      dependents =
        Map.fromListWith (++) [(dep, [uid]) | uid <- Map.keys localElabs, dep <- localLibDeps uid]

      totalNodes = Map.size localElabs

  remainingVar <- newTVarIO (Map.fromList [(uid, length (localLibDeps uid)) | uid <- Map.keys localElabs])
  readyVar <- newTVarIO [uid | uid <- Map.keys localElabs, null (localLibDeps uid)]
  processedVar <- newTVarIO (0 :: Int)
  indexVar <- newTVarIO installedIndex
  componentLBIsVar <- newTVarIO Map.empty

  let -- Decrement the remaining-dependency count of every dependent of
      -- @uid@ (a component that just finished); returns whichever
      -- dependents' count just hit zero.
      unblockDependentsOf :: UnitId -> Map UnitId Int -> (Map UnitId Int, [UnitId])
      unblockDependentsOf uid remaining =
        foldl'
          ( \(rem_, rs) dep ->
              let n = Map.findWithDefault 0 dep rem_ - 1
               in (Map.insert dep n rem_, if n == 0 then dep : rs else rs)
          )
          (remaining, [])
          (Map.findWithDefault [] uid dependents)

      -- One component just finished: record it, and - in the *same*
      -- transaction - push any newly-unblocked dependents straight onto
      -- 'readyVar'. Has to be one atomic step, not two: see this
      -- function's own haddock for the real bug that came from ever
      -- letting "this is now ready" and "a worker can see it" drift
      -- apart, even briefly.
      finishNode :: UnitId -> STM ()
      finishNode uid = do
        remaining <- readTVar remainingVar
        let (remaining', newlyReady) = unblockDependentsOf uid remaining
        writeTVar remainingVar remaining'
        modifyTVar' readyVar (newlyReady ++)
        modifyTVar' processedVar (+ 1)

      -- Pop one ready component for a worker to process, or 'Nothing'
      -- once there's provably nothing left to do, ever: every node that
      -- could still add something to 'readyVar' has already run
      -- 'finishNode' by the time 'processedVar' reaches 'totalNodes'.
      -- An empty queue with work still outstanding elsewhere instead
      -- 'retry's - STM wakes this automatically the moment 'readyVar'
      -- or 'processedVar' next changes.
      popReady :: STM (Maybe UnitId)
      popReady = do
        ready <- readTVar readyVar
        case ready of
          (uid : rest) -> writeTVar readyVar rest >> return (Just uid)
          [] -> do
            processed <- readTVar processedVar
            if processed == totalNodes then return Nothing else retry

      workerLoop :: IO ()
      workerLoop = do
        muid <- atomically popReady
        for_ muid $ \uid -> do
          let elab = localElabs Map.! uid
              pkgDesc = elabPkgDescription elab
          idx <- readTVarIO indexVar
          lbi <- localBuildInfoFor verbosity distDirLayout plan shared idx elab
          for_ (componentNamesFor elab pkgDesc) $ \cname -> do
            mipi <- libraryInstalledPackageInfo verbosity lbi pkgDesc cname
            for_ mipi $ \ipi -> atomically $ modifyTVar' indexVar (PackageIndex.insert ipi)
            atomically $ modifyTVar' componentLBIsVar (Map.insert (packageName pkgDesc, cname) lbi)
          atomically (finishNode uid)
          workerLoop

  Async.replicateConcurrently_ numberOfProcessors workerLoop
  readTVarIO componentLBIsVar

-- | Like 'pruneInstallPlanToDependencies', but when excluding every
-- selected target would leave a dangling edge, keep exactly the targets
-- the failure says are still needed instead of giving up outright - and
-- retry, since keeping one target in can itself reveal another one is
-- needed too (transitively).
--
-- This is a real project shape, not a hypothetical: a @build-type:
-- Custom@ local package's Setup.hs can have @setup-depends@ on another
-- *local* package (e.g. cabal-testsuite's Setup needs Cabal-syntax to be
-- built) - the Setup component that creates is a real node in the plan,
-- but isn't itself one of the ordinary library\/exe\/test\/bench targets
-- 'resolveTargetsFromSolver' selects, so plain
-- 'pruneInstallPlanToDependencies' (asked to exclude literally every
-- selected target) sees its now-dangling edge to Cabal-syntax and
-- refuses outright, even though building Cabal-syntax here is exactly
-- what's needed - it's a real dependency of the build, just not of any
-- selected target directly.
pruneToDependenciesNeeded
  :: Set UnitId
  -> ElaboratedInstallPlan
  -> Either CannotPruneDependencies ElaboratedInstallPlan
pruneToDependenciesNeeded excluded plan =
  case pruneInstallPlanToDependencies excluded plan of
    Right pruned -> Right pruned
    Left err@(CannotPruneDependencies broken)
      | Set.null keepIds || excluded' == excluded -> Left err
      | otherwise -> pruneToDependenciesNeeded excluded' plan
      where
        keepIds = Set.fromList [installedUnitId dep | (_, missing) <- broken, dep <- missing]
        excluded' = excluded `Set.difference` keepIds
