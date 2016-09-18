{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: repl
--
module Distribution.Client.CmdRepl (
    replCommand,
    replAction,
  ) where

import Distribution.Client.ProjectOrchestration
         ( PreBuildHooks(..), runProjectPreBuildPhase, selectTargets
         , ProjectBuildContext(..), runProjectBuildPhase
         , printPlan, reportBuildFailures )
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

import Control.Monad (when, unless)

import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Utils
         ( wrapText, die )
import qualified Distribution.Client.Setup as Client

replCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
replCommand = Client.installCommand {
  commandName         = "new-repl",
  commandSynopsis     = "Open a REPL for the current project",
  commandUsage        = usageAlternatives "new-repl" [ "[FLAGS] TARGET" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Opens a REPL for a Nix-local build project.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-repl cname"
     ++ "    Open a REPL for the component named cname\n"
     ++ "  " ++ pname ++ " new-repl pkgname:cname"
     ++ "    Open a REPL for the component named cname in pkgname\n"
   }

-- | The @repl@ command is very much like @build@. It brings the install plan
-- up to date, selects that part of the plan needed by the given or implicit
-- repl target and then executes the plan.
--
-- Compared to @build@ the difference is that only one target is allowed
-- (given or implicit) and the target type is repl rather than build. The
-- general plan execution infrastructure handles both build and repl targets.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
replAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> GlobalFlags -> IO ()
replAction (configFlags, configExFlags, installFlags, haddockFlags)
           targetStrings globalFlags = do

    userTargets <- readUserBuildTargets targetStrings

    buildCtx@ProjectBuildContext{buildSettings, elaboratedPlan} <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning      = \_ _ _ -> return (),

          hookSelectPlanSubset = \buildSettings elaboratedPlan -> do
            when (buildSettingOnlyDeps buildSettings) $
              die $ "The repl command does not support '--only-dependencies'. "
                 ++ "You may wish to use 'build --only-dependencies' and then "
                 ++ "use 'repl'."
            -- Interpret the targets on the command line as repl targets
            -- (as opposed to say build or haddock targets).
            selectTargets
              verbosity
              ReplDefaultComponent
              ReplSpecificComponent
              userTargets
              False -- onlyDependencies, always False for repl
              elaboratedPlan
            --TODO: [required eventually] reject multiple targets, or at least
            -- targets spanning multiple components. ie it's ok to have two
            -- module/file targets in the same component, but not two that live
            -- in different components.
        }

    printPlan verbosity buildCtx

    unless (buildSettingDryRun buildSettings) $ do
      buildResults <- runProjectBuildPhase verbosity buildCtx
      reportBuildFailures verbosity elaboratedPlan buildResults
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

