-- | cabal-install CLI command: build
--
module Distribution.Client.CmdBuild (
    buildCommand,
    buildAction,
  ) where

import Distribution.Client.ProjectOrchestration
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

import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Utils
         ( wrapText )
import qualified Distribution.Client.Setup as Client

buildCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
buildCommand = Client.installCommand {
  commandName         = "new-build",
  commandSynopsis     = "Builds a Nix-local build project",
  commandUsage        = usageAlternatives "new-build" [ "[FLAGS]"
                                                      , "[FLAGS] TARGETS" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Builds a Nix-local build project, automatically building and installing"
     ++ "necessary dependencies.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-build           "
     ++ "    Build the package in the current directory or all packages in the project\n"
     ++ "  " ++ pname ++ " new-build pkgname   "
     ++ "    Build the package named pkgname in the project\n"
     ++ "  " ++ pname ++ " new-build cname     "
     ++ "    Build the component named cname in the project\n"
     ++ "  " ++ pname ++ " new-build pkgname:cname"
     ++    " Build the component named cname in the package pkgname\n"
   }


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

    buildCtx <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning      = \_ _ _ -> return (),

          hookSelectPlanSubset = \buildSettings' elaboratedPlan -> do
            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            selectTargets
              verbosity
              BuildDefaultComponents
              BuildSpecificComponent
              userTargets
              (buildSettingOnlyDeps buildSettings')
              elaboratedPlan
        }

    printPlan verbosity buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity buildCtx
    runProjectPostBuildPhase verbosity buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
