-- | cabal-install CLI command: configure
--
module Distribution.Client.CmdConfigure (
    configureCommand,
    configureAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig

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

configureCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
configureCommand = Client.installCommand {
  commandName         = "new-configure",
  commandSynopsis     = "Write out a cabal.project.local file.",
  commandUsage        = usageAlternatives "new-configure" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Configures a Nix-local build project, downloading source from"
     ++ " the network and writing out a cabal.project.local file which"
     ++ " saves any FLAGS, to be reapplied on subsequent invocations to new-build.",
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
          hookPrePlanning = \projectRootDir _ cliConfig ->
            -- Write out the @cabal.project.local@ so it gets picked up by the
            -- planning phase.
            writeProjectLocalExtraConfig projectRootDir cliConfig,

          hookSelectPlanSubset = return
        }

    --TODO: Hmm, but we don't have any targets. Currently this prints what we
    -- would build if we were to build everything. Could pick implicit target like "."
    --TODO: should we say what's in the project (+deps) as a whole?
    printPlan
      verbosity
      buildCtx {
        buildSettings = (buildSettings buildCtx) {
          buildSettingDryRun = True
        }
      }
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

