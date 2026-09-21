-- | Tier-0 stable API: building project targets.
--
-- Thin wrapper over the internal @Distribution.Client.CmdBuild@ and the
-- orchestration layer (@Distribution.Client.ProjectOrchestration@,
-- @Distribution.Client.ScriptUtils@).
--
-- This is the stable replacement for third-party tools that used to import
-- the orchestration layer and @buildAction@ directly (see cabal-hoogle).
module Distribution.Client.API.Build
  ( BuildOptions -- Note: cabal-hoogle
  , buildOptBuildDir -- Note: cabal-hoogle
  , buildOptNoOptimisation -- Note: cabal-hoogle
  , buildOptDocumentation -- Note: cabal-hoogle
  , buildOptHaddockHoogle -- Note: cabal-hoogle
  , buildOptHaddockHtml -- Note: cabal-hoogle
  , buildOptHaddockLinkedSource -- Note: cabal-hoogle
  , buildOptHaddockQuickJump -- Note: cabal-hoogle
  , defaultBuildOptions -- Note: cabal-hoogle
  , runBuild -- Note: cabal-hoogle
  , BuildTarget -- Note: cabal-hoogle
  , buildTargetDistDir -- Note: cabal-hoogle
  , buildTargetsAndDirs -- Note: cabal-hoogle
  ) where

import qualified Data.Map as Map
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
  , TargetsMap
  , pruneInstallPlanToTargets
  , resolveTargetsFromSolver
  , runProjectPreBuildPhase
  )
import Distribution.Client.ProjectPlanning.Types (ElaboratedInstallPlan, elabDistDirParams)
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup (InstallFlags (..), defaultGlobalFlags)
import Distribution.Client.TargetSelector (TargetSelector)
import Distribution.Simple.Compiler (OptimisationLevel (..))
import Distribution.Simple.Flag (maybeToFlag, toFlag)
import Distribution.Simple.Setup
  ( CommonSetupFlags (..)
  , ConfigFlags (..)
  , HaddockFlags (..)
  )
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity (Verbosity)

-- | Options controlling a build, mirroring the subset of @cabal build@ /
-- @cabal haddock@ flags that known consumers need.
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
mkNixStyleFlags opts =
  base
    { configFlags = configFlags'
    , haddockFlags = haddockFlags'
    , installFlags = installFlags'
    }
  where
    base :: NixStyleFlags BuildFlags
    base = defaultNixStyleFlags defaultBuildFlags

    configFlags' :: ConfigFlags
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

    haddockFlags' :: HaddockFlags
    haddockFlags' =
      (haddockFlags base)
        { haddockHoogle = toFlag (buildOptHaddockHoogle opts)
        , haddockHtml = toFlag (buildOptHaddockHtml opts)
        , haddockLinkedSource = toFlag (buildOptHaddockLinkedSource opts)
        , haddockQuickJump = toFlag (buildOptHaddockQuickJump opts)
        }

    installFlags' :: InstallFlags
    installFlags' = (installFlags base){installDocumentation = toFlag (buildOptDocumentation opts)}

-- | Build the given targets, like @cabal build@. Target strings use the
-- usual cabal target syntax; an empty list means @all@.
runBuild :: BuildOptions -> [String] -> IO ()
runBuild opts targetStrings = buildAction (mkNixStyleFlags opts) targetStrings defaultGlobalFlags

-- | A resolved build target together with its build directory.
data BuildTarget = BuildTarget
  { buildTargetDistDir :: FilePath
  }

-- | Resolve the given target strings against the project and return the
-- build directory of every target package, without building anything.
--
-- This is the read-only part of @cabal build@'s context setup: target
-- selection, solving and plan pruning.
buildTargetsAndDirs :: Verbosity -> BuildOptions -> [String] -> IO [BuildTarget]
buildTargetsAndDirs verbosity opts targetStrings = do
  withContextAndSelectors
    verbosity
    RejectNoTargets
    Nothing
    (mkNixStyleFlags opts)
    (if null targetStrings then ["all"] else targetStrings)
    defaultGlobalFlags
    BuildCommand
    (targetsAction verbosity)

targetsAction :: Verbosity -> TargetContext -> ProjectBaseContext -> [TargetSelector] -> IO [BuildTarget]
targetsAction verbosity targetCtx projectBaseContext targetSelectors = do
  ctx <- case targetCtx of
    ProjectContext -> pure projectBaseContext
    GlobalContext -> pure projectBaseContext
    ScriptContext scriptPath scriptExe ->
      updateContextAndWriteProjectFile projectBaseContext scriptPath scriptExe
  buildCtx <-
    runProjectPreBuildPhase
      verbosity
      ctx
      (runElaboratedInstallPlan verbosity targetSelectors)
  pure
    [ BuildTarget
      { buildTargetDistDir =
          distBuildDirectory
            (distDirLayout ctx)
            (elabDistDirParams (elaboratedShared buildCtx) elab)
      }
    | (unitId, _) <- Map.toList (targetsMap buildCtx)
    , Just planPkg <- [InstallPlan.lookup (elaboratedPlanToExecute buildCtx) unitId]
    , elab <- case planPkg of
        InstallPlan.Configured e -> [e]
        InstallPlan.Installed e -> [e]
        InstallPlan.PreExisting _InstalledPackageInfo -> []
    ]

runElaboratedInstallPlan :: Verbosity -> [TargetSelector] -> ElaboratedInstallPlan -> IO (ElaboratedInstallPlan, TargetsMap)
runElaboratedInstallPlan verbosity targetSelectors elaboratedPlan = do
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
