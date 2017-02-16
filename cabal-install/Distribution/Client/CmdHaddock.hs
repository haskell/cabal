{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: haddock
--
module Distribution.Client.CmdHaddock (
    haddockCommand,
    haddockAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
         ( PackageTarget(..) )
import Distribution.Client.BuildTarget
         ( readUserBuildTargets )

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( wrapText )
import Distribution.Verbosity
         ( normal )

import Control.Monad (unless, void)

haddockCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags
                            ,HaddockFlags)
haddockCommand = Client.installCommand {
  commandName         = "new-haddock",
  commandSynopsis     = "Build Haddock documentation for the current project",
  commandUsage        = usageAlternatives "new-haddock" [ "[FLAGS] TARGET" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Build Haddock documentation for a Nix-local build project.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-haddock cname"
     ++ "    Build documentation for the component named cname\n"
     ++ "  " ++ pname ++ " new-haddock pkgname:cname"
     ++ "    Build documentation for the component named cname in pkgname\n"
   }

-- | The @haddock@ command is TODO.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
haddockAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
                 -> [String] -> GlobalFlags -> IO ()
haddockAction (configFlags, configExFlags, installFlags, haddockFlags)
                targetStrings globalFlags = do

    userTargets <- readUserBuildTargets targetStrings

    buildCtx <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning = \_ _ _ -> return (),
          hookSelectPlanSubset = \_ ->
              -- When we interpret the targets on the command line,
              -- interpret them as haddock targets
              selectTargets
                verbosity
                HaddockDefaultComponents
                (const HaddockDefaultComponents)
                userTargets
                False -- onlyDependencies, always False for haddock
        }

    --TODO: Hmm, but we don't have any targets. Currently this prints
    -- what we would build if we were to build everything. Could pick
    -- implicit target like "."  TODO: should we say what's in the
    -- project (+deps) as a whole?
    printPlan
      verbosity
      buildCtx {
        buildSettings = (buildSettings buildCtx) {
          buildSettingDryRun = True
        }
      }

    unless (buildSettingDryRun (buildSettings buildCtx)) $ void $
      runProjectBuildPhase
        verbosity
        buildCtx
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
