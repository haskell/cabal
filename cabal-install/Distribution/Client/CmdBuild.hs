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
         ( PackageTarget(..), BuildFailure(..), ElaboratedInstallPlan )
import Distribution.Client.BuildTarget
         ( readUserBuildTargets )
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import Distribution.Package
         ( packageId )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( die )
import Distribution.Text
         ( display )
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

    unless (buildSettingDryRun buildSettings) $ do
      plan <- runProjectBuildPhase verbosity buildCtx
      printBuildFailures plan
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

    -- When we interpret the targets on the command line, interpret them as
    -- repl targets (as opposed to say repl or haddock targets).
    selectBuildTargets =
      selectTargets
        BuildDefaultComponents
        BuildSpecificComponent


-- | If failures are recoded in the install-plan, print them and 'die'.
printBuildFailures :: ElaboratedInstallPlan -> IO ()
printBuildFailures plan =
  case [ (pkg, reason)
       | InstallPlan.Failed pkg reason <- InstallPlan.toList plan ] of
    []     -> return ()
    failed -> die . unlines
            $ "Error: build failed:"
            : [ display (packageId pkg) ++ printFailureReason reason
              | (pkg, reason) <- failed ]
  where
    printFailureReason reason = case reason of
      DependentFailed pkgid -> " depends on " ++ display pkgid
                            ++ " which failed to install."
      DownloadFailed  e -> " failed while downloading the package."
                        ++ showException e
      UnpackFailed    e -> " failed while unpacking the package."
                        ++ showException e
      ConfigureFailed e -> " failed during the configure step."
                        ++ showException e
      BuildFailed     e -> " failed during the building phase."
                        ++ showException e
      TestsFailed     e -> " failed during the tests phase."
                        ++ showException e
      InstallFailed   e -> " failed during the final install step."
                        ++ showException e

      -- This will never happen, but we include it for completeness
      PlanningFailed -> " failed during the planning phase."

    showException e   =  " The exception was:\n  " ++ show e
