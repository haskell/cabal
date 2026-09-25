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

import qualified Distribution.PackageDescription as PD
import Distribution.Package (HasUnitId (installedUnitId), packageId, packageName)
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
      -- 'localPkgs's own comment above) *component* - built the exact
      -- same way "Distribution.Client.ProjectBuilding.UnpackedPackage"
      -- builds one for a real build, via
      -- "Distribution.Client.InLibrary" (an in-process, no-subprocess
      -- reimplementation of Setup.hs's own configure logic that calls
      -- straight into the real 'Distribution.Simple.Configure.configureFinal'
      -- - so the result is exactly what a real @Setup configure@ would
      -- produce, not a hand-approximation of it). This is what lets
      -- "Distribution.Client.Buck2.CabalToBuck" call Cabal's own
      -- @generateCabalMacrosHeader@\/@generatePathsModule@ directly
      -- instead of reimplementing pieces of them by hand (a real
      -- maintenance risk otherwise - see buck2.md's own note on this).
      -- Confirmed via reading 'InLibrary.configure''s own source that
      -- this is a pure in-memory computation: nothing in it calls
      -- @writePersistBuildConfig@ or otherwise writes to
      -- @dist-newstyle@, so this doesn't give local packages a second,
      -- redundant "configured" state on disk.
      --
      -- Processed in dependency order ('InstallPlan.reverseTopologicalOrder'
      -- - despite the name, dependencies first - not the arbitrary order
      -- 'InstallPlan.toList' returns), threading a *growing*
      -- 'InstalledPackageIndex' through the fold: a local package that
      -- build-depends on another local package (e.g. almost everything
      -- here depends on the local @Cabal@ library) needs that
      -- dependency's own real 'UnitId' resolvable in the index passed to
      -- 'localBuildInfoFor' - exactly the role "Note [Per-project
      -- InstalledPackageIndex]" in
      -- "Distribution.Client.ProjectBuilding" describes for a real
      -- build's own incrementally-registered index - so
      -- 'registerLocalLibrary' below adds each library component's own
      -- (unbuilt, in-place) 'InstalledPackageInfo' as soon as it's
      -- configured, before moving on to whatever depends on it.
      (componentLBIs, _) <-
        foldM
          ( \(cmap, idx) elab -> do
              lbi <- localBuildInfoFor verbosity (distDirLayout baseCtx) elaboratedPlanOriginal elaboratedShared idx elab
              let pkgDesc = elabPkgDescription elab
                  -- Per-component elaboration gives one 'elab' per
                  -- component, correctly named by 'elabComponentName'.
                  -- But not every package gets elaborated that way: a
                  -- package elaborated as a whole ('ElabPackage') gets
                  -- exactly one 'elab' for the *entire* package, and
                  -- 'elabComponentName' defaults that to just the main
                  -- library ("there could be more, but default this" -
                  -- its own haddock) - even though the single 'lbi' this
                  -- configure call just produced genuinely covers every
                  -- component of the package (real Cabal's own
                  -- 'configureFinal' computes a
                  -- 'ComponentLocalBuildInfo' for each one internally,
                  -- regardless of elaboration mode). Registering only
                  -- the library here left every *other* component of
                  -- such a package (executables, test-suites, ...)
                  -- with no 'componentLBIs' entry at all - confirmed as
                  -- the real cause of "no LocalBuildInfo found" wrongly
                  -- skipping e.g. glean-clang's own executables, which
                  -- are elaborated this way. So for 'ElabPackage' mode,
                  -- every buildable component of the package is
                  -- registered under this same 'lbi', not just the one
                  -- 'elabComponentName' names.
                  cnames = case elabPkgOrComp elab of
                    ElabComponent _ -> maybeToList (elabComponentName elab)
                    ElabPackage _ -> [componentName comp | comp <- PD.pkgBuildableComponents pkgDesc]
              idx' <- foldM (\i cname -> registerLocalLibrary verbosity lbi pkgDesc cname i) idx cnames
              let cmap' = foldl' (\m cname -> Map.insert (packageName pkgDesc, cname) lbi m) cmap cnames
              return (cmap', idx')
          )
          (Map.empty, installedIndex)
          [ elab
          | InstallPlan.Configured elab <- InstallPlan.reverseTopologicalOrder elaboratedPlanOriginal
          , elabLocalToProject elab || elabBuildStyle elab /= BuildAndInstall
          ]

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
-- actually writing anything anywhere - and add it to @idx@. Real
-- Cabal's own 'generateRegistrationInfo', in its in-place branch, needs
-- no built object code to do this: an in-place package's ABI hash is
-- always the fixed placeholder @"inplace"@ (see its own haddock), so
-- this is safe to call immediately after 'localBuildInfoFor' configures
-- the component, before anything is actually compiled.
--
-- Every other kind of component (executable\/test-suite\/benchmark) is
-- skipped: nothing ever depends on one of those by 'UnitId', so they
-- have nothing to contribute to the index.
registerLocalLibrary :: Verbosity -> LocalBuildInfo -> PackageDescription -> ComponentName -> InstalledPackageIndex -> IO InstalledPackageIndex
registerLocalLibrary verbosity lbi pkgDesc cname idx = case cname of
  CLibName ln
    | Just lib <- listToMaybe [l | l <- PD.allLibraries pkgDesc, PD.libName l == ln]
    , (clbi : _) <- componentNameCLBIs lbi cname -> do
        ipi <- generateRegistrationInfo verbosity pkgDesc lib lbi clbi True (relocatable lbi) (distPrefLBI lbi) GlobalPackageDB
        return (PackageIndex.insert ipi idx)
  _ -> return idx

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
