{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: build
--
module Distribution.Client.CmdBuild (
    buildAction,
  ) where

import Distribution.Client.ProjectOrchestration
         ( PreBuildHooks(..), runProjectPreBuildPhase, selectTargets
         , ProjectBuildContext(..), runProjectBuildPhase,  printPlan )
import Distribution.Client.ProjectConfig
         ( BuildTimeSettings(..) )
import Distribution.Client.ProjectPlanning
         ( PackageTarget(..) )
import Distribution.Client.BuildTarget
         ( readUserBuildTargets )

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Verbosity
         ( normal )

import Control.Monad (unless)


-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
buildAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
            -> [String] -> GlobalFlags -> IO ()
buildAction (configFlags, configExFlags, installFlags, haddockFlags)
            targetStrings globalFlags = do

    userTargets <- readUserBuildTargets targetStrings

    buildCtx@ProjectBuildContext{buildSettings} <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning      = \_ _ _ -> return (),
          hookSelectPlanSubset = selectBuildTargets userTargets
        }

    printPlan verbosity buildCtx

    unless (buildSettingDryRun buildSettings) $
      runProjectBuildPhase
        verbosity
        buildCtx
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

    -- When we interpret the targets on the command line, interpret them as
    -- repl targets (as opposed to say repl or haddock targets).
    selectBuildTargets =
      selectTargets
        BuildDefaultComponents
        BuildSpecificComponent

