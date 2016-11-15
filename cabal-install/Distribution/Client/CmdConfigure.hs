-- | cabal-install CLI command: configure
--
module Distribution.Client.CmdConfigure (
    configureCommand,
    configureAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
         ( PackageTarget(..) )

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

configureCommand :: CommandUI (ConfigFlags, ConfigExFlags
                              ,InstallFlags, HaddockFlags)
configureCommand = Client.installCommand {
  commandName         = "new-configure",
  commandSynopsis     = "Write out a cabal.project.local file.",
  commandUsage        = usageAlternatives "new-configure" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Configures a Nix-local build project, downloading source from"
     ++ " the network and writing out a cabal.project.local file"
     ++ " (or $project_file.local, if --project-file is specified)"
     ++ " which saves any FLAGS, to be reapplied on subsequent invocations to"
     ++ " new-build.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-configure           "
     ++ "    Configure project of the current directory\n"
   }

-- | To a first approximation, the @configure@ just runs the first phase of
-- the @build@ command where we bring the install plan up to date (thus
-- checking that it's possible).
--
-- The only difference is that @configure@ also allows the user to specify
-- some extra config flags which we save in the file @cabal.project.local@.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
configureAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
                -> [String] -> GlobalFlags -> IO ()
configureAction (configFlags, configExFlags, installFlags, haddockFlags)
                _extraArgs globalFlags = do
    --TODO: deal with _extraArgs, since flags with wrong syntax end up there

    buildCtx <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning = \rootDir _ cliConfig ->
            -- Write out the @cabal.project.local@ so it gets picked up by the
            -- planning phase.
            writeProjectLocalExtraConfig installFlags rootDir cliConfig,

          hookSelectPlanSubset = \_buildSettings' elaboratedPlan -> do
            -- Select the same subset of targets as 'CmdBuild' would
            -- pick (ignoring, for example, executables in libraries
            -- we depend on).
            targets <- resolveTargets
                         BuildDefaultComponents
                         BuildSpecificComponent
                         elaboratedPlan
                         []

            return$ pruneInstallPlanToTargets
                      TargetActionBuild
                      (elaboratePackageTargets elaboratedPlan targets)
                      elaboratedPlan
        }

    let buildCtx' = buildCtx {
                      buildSettings = (buildSettings buildCtx) {
                        buildSettingDryRun = True
                      }
                    }

    -- TODO: Hmm, but we don't have any targets. Currently this prints
    -- what we would build if we were to build everything. Could pick
    -- implicit target like "."
    --
    -- TODO: should we say what's in the project (+deps) as a whole?
    printPlan verbosity buildCtx'
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
