-- | cabal-install CLI command: build
--
module Distribution.Client.CmdBuild (
    buildCommand,
    buildAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
         ( BuildTimeSettings(..) )
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
import qualified Data.Map as Map
import Control.Exception (throwIO)


buildCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
buildCommand = Client.installCommand {
  commandName         = "new-build",
  commandSynopsis     = "Builds a Nix-local build project",
  commandUsage        = usageAlternatives "new-build" [ "[FLAGS]"
                                                      , "[FLAGS] TARGETS" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Builds a Nix-local build project, automatically building and installing "
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

    userTargets <- readUserBuildTargets verbosity targetStrings

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
            targets <- either reportBuildTargetProblems return
                   =<< resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         userTargets

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            elaboratedPlan'' <-
              if buildSettingOnlyDeps buildSettings'
                then either throwIO return $
                     pruneInstallPlanToDependencies (Map.keysSet targets)
                                                    elaboratedPlan'
                else return elaboratedPlan'

            return elaboratedPlan''
        }

    printPlan verbosity buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity buildCtx
    runProjectPostBuildPhase verbosity buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- For build: select all components except non-buildable and disabled
-- tests/benchmarks, fail if there are no such components
--
selectPackageTargets :: BuildTarget PackageId
                     -> [AvailableTarget k] -> Either BuildTargetProblem [k]
selectPackageTargets _bt ts
  | (_:_)  <- enabledts = Right enabledts
  | (_:_)  <- ts        = Left TargetPackageNoEnabledTargets -- allts
  | otherwise           = Left TargetPackageNoTargets
  where
    enabledts = [ k | TargetBuildable k TargetRequestedByDefault
                        <- map availableTargetStatus ts ]

-- For checking an individual component target, for build there's no
-- additional checks we need beyond the basic ones.
--
selectComponentTarget :: BuildTarget PackageId
                      -> AvailableTarget k -> Either BuildTargetProblem k
selectComponentTarget bt =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic bt

data BuildTargetProblem =
     TargetPackageNoEnabledTargets
   | TargetPackageNoTargets
   | TargetProblemCommon TargetProblem
  deriving Show

reportBuildTargetProblems :: [BuildTargetProblem] -> IO a
reportBuildTargetProblems = fail . show
