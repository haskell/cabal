-- | Tier-0 stable API: building project targets.
--
-- Thin wrapper over the internal @Distribution.Client.CmdBuild@ and the
-- orchestration layer (@Distribution.Client.ProjectOrchestration@,
-- @Distribution.Client.ScriptUtils@).
--
-- This is the stable replacement for third-party tools that used to import
-- the orchestration layer and @buildAction@ directly (see cabal-hoogle).
module Distribution.Client.API.Build
  ( BuildPlanEntry (..)
  , withBuildPlan
  , BuildOptions (..)
  , defaultBuildOptions
  , runBuild
  , BuildTarget (..)
  , buildTargetsAndDirs
  ) where

import qualified Data.Map as Map
import qualified Distribution.Client.API.Project as API (ProjectContext (..))
import Distribution.Client.CmdBuild
  ( BuildFlags
  , buildAction
  , defaultBuildFlags
  , selectComponentTarget
  , selectPackageTargets
  )
import Distribution.Client.CmdErrorMessages (reportTargetProblems)
import Distribution.Client.DistDirLayout (distBuildDirectory)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags)
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (..)
  , ProjectBaseContext (..)
  , ProjectBuildContext (..)
  , TargetAction (..)
  , pruneInstallPlanToTargets
  , resolveTargetsFromSolver
  , runProjectPreBuildPhase
  , withInstallPlan
  )
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedConfiguredPackage
  , elabDistDirParams
  , elabLocalToProject
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup (InstallFlags (..), defaultGlobalFlags)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Package (Package (..), PackageId)
import Distribution.Simple (OptimisationLevel (..))
import Distribution.Simple.Flag (maybeToFlag, toFlag)
import Distribution.Simple.Setup
  ( CommonSetupFlags (..)
  , ConfigFlags (..)
  , HaddockFlags (..)
  )
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity (Verbosity)

-- | One package in the build plan, as a read-only snapshot.
--
-- Deliberately NOT the internal 'ElaboratedConfiguredPackage': the goal is
-- to keep the plan's representation free to change.
data BuildPlanEntry = BuildPlanEntry
  { entryPackageId :: PackageId
  -- ^ The package (with version) this plan node is about.
  , entryIsLocal :: Bool
  -- ^ Whether the package belongs to the current project
  -- (as opposed to a dependency fetched from Hackage or the store).
  , entryIsPreExisting :: Bool
  -- ^ Whether the package is taken from the installed package database.
  }

-- | Solve the project and run the continuation with the resulting plan.
withBuildPlan :: Verbosity -> API.ProjectContext -> ([BuildPlanEntry] -> IO a) -> IO a
withBuildPlan verbosity (API.ProjectContext ctx) action =
  withInstallPlan verbosity ctx $ \installPlan _sharedConfig ->
    action (map toEntry (InstallPlan.toList installPlan))
  where
    toEntry :: InstallPlan.GenericPlanPackage InstalledPackageInfo ElaboratedConfiguredPackage -> BuildPlanEntry
    toEntry (InstallPlan.PreExisting ipkg) =
      BuildPlanEntry
        { entryPackageId = packageId ipkg
        , entryIsLocal = False
        , entryIsPreExisting = True
        }
    toEntry (InstallPlan.Installed elab) = fromElab elab
    toEntry (InstallPlan.Configured elab) = fromElab elab
    fromElab elab =
      BuildPlanEntry
        { entryPackageId = packageId elab
        , entryIsLocal = elabLocalToProject elab
        , entryIsPreExisting = False
        }

-- | Options controlling a build, mirroring the subset of @cabal build@ /
-- @cabal haddock@ flags that known consumers need.
--
-- TODO: expose more of @NixStyleFlags@ as the need arises (dry-run,
-- @--only-dependencies@, verbosity, ...).
data BuildOptions = BuildOptions
  { buildOptBuildDir :: Maybe FilePath
  -- ^ Override for @--builddir@.
  , buildOptNoOptimisation :: Bool
  -- ^ Build with @-O0@.
  , buildOptDocumentation :: Bool
  -- ^ Generate documentation (@installDocumentation@).
  , buildOptHaddockHoogle :: Bool
  -- ^ Run haddock with @--hoogle@.
  , buildOptHaddockHtml :: Bool
  -- ^ Run haddock with @--html@.
  , buildOptHaddockLinkedSource :: Bool
  -- ^ Run haddock with @--hyperlinked-source@.
  , buildOptHaddockQuickJump :: Bool
  -- ^ Run haddock with @--quickjump@.
  }

defaultBuildOptions :: BuildOptions
defaultBuildOptions =
  BuildOptions
    { buildOptBuildDir = Nothing
    , buildOptNoOptimisation = False
    , buildOptDocumentation = False
    , buildOptHaddockHoogle = False
    , buildOptHaddockHtml = False
    , buildOptHaddockLinkedSource = False
    , buildOptHaddockQuickJump = False
    }

mkNixStyleFlags :: BuildOptions -> NixStyleFlags BuildFlags
mkNixStyleFlags opts = flags
  where
    base = defaultNixStyleFlags defaultBuildFlags
    configFlags' =
      (configFlags base)
        { configCommonFlags =
            (configCommonFlags (configFlags base))
              { setupDistPref = maybeToFlag (makeSymbolicPath <$> buildOptBuildDir opts)
              }
        , configOptimization =
            if buildOptNoOptimisation opts
              then toFlag NoOptimisation
              else configOptimization (configFlags base)
        }
    haddockFlags' =
      (haddockFlags base)
        { haddockHoogle = toFlag (buildOptHaddockHoogle opts)
        , haddockHtml = toFlag (buildOptHaddockHtml opts)
        , haddockLinkedSource = toFlag (buildOptHaddockLinkedSource opts)
        , haddockQuickJump = toFlag (buildOptHaddockQuickJump opts)
        }
    installFlags' =
      (installFlags base)
        { installDocumentation = toFlag (buildOptDocumentation opts)
        }
    flags = base {configFlags = configFlags', haddockFlags = haddockFlags', installFlags = installFlags'}

-- | Build the given targets, like @cabal build@. Target strings use the
-- usual cabal target syntax; an empty list means @all@.
runBuild :: BuildOptions -> [String] -> IO ()
runBuild opts targetStrings = buildAction (mkNixStyleFlags opts) targetStrings defaultGlobalFlags

-- | A resolved build target together with its build directory.
data BuildTarget = BuildTarget
  { buildTargetPackageId :: PackageId
  , buildTargetDistDir :: FilePath
  }

-- | Resolve the given target strings against the project and return the
-- build directory of every target package, without building anything.
--
-- This is the read-only part of @cabal build@'s context setup: target
-- selection, solving and plan pruning.
--
-- TODO: decide whether a version of this function taking an existing
-- 'ProjectContext' should be exposed as well.
buildTargetsAndDirs :: Verbosity -> BuildOptions -> [String] -> IO [BuildTarget]
buildTargetsAndDirs verbosity opts targetStrings = do
  let targetStrings' = if null targetStrings then ["all"] else targetStrings
  withContextAndSelectors
    verbosity
    RejectNoTargets
    Nothing
    (mkNixStyleFlags opts)
    targetStrings'
    defaultGlobalFlags
    BuildCommand
    $ \targetCtx ctx targetSelectors -> do
      ctx' <- case targetCtx of
        ProjectContext -> pure ctx
        GlobalContext -> pure ctx
        ScriptContext scriptPath scriptExe ->
          updateContextAndWriteProjectFile ctx scriptPath scriptExe
      buildCtx <-
        runProjectPreBuildPhase verbosity ctx' $ \elaboratedPlan -> do
          targets <-
            either
              (reportTargetProblems verbosity "build")
              pure
              ( resolveTargetsFromSolver
                  selectPackageTargets
                  selectComponentTarget
                  elaboratedPlan
                  Nothing
                  targetSelectors
              )
          pure
            ( pruneInstallPlanToTargets TargetActionBuild targets elaboratedPlan
            , targets
            )
      let plan = elaboratedPlanToExecute buildCtx
          shared = elaboratedShared buildCtx
          tsm = targetsMap buildCtx
      pure
        [ BuildTarget
          { buildTargetPackageId = packageId elab
          , buildTargetDistDir = distBuildDirectory (distDirLayout ctx') (elabDistDirParams shared elab)
          }
        | (unitId, _) <- Map.toList tsm
        , Just planPkg <- [InstallPlan.lookup plan unitId]
        , elab <- case planPkg of
            InstallPlan.Configured e -> [e]
            InstallPlan.Installed e -> [e]
            InstallPlan.PreExisting _ -> []
        ]
