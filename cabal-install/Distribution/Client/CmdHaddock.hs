{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: haddock
--
module Distribution.Client.CmdHaddock (
    haddockCommand,
    haddockAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
         ( BuildTimeSettings(..) )
import Distribution.Client.BuildTarget
         ( readUserBuildTargets )

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags(..), fromFlagOrDefault, fromFlag )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Verbosity
         ( normal )
import Distribution.Simple.Utils
         ( wrapText, die' )

import qualified Data.Map as Map
import Control.Monad (when)


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

    userTargets <- readUserBuildTargets verbosity targetStrings

    buildCtx <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning = \_ _ _ -> return (),
          hookSelectPlanSubset = \buildSettings elaboratedPlan -> do
            when (buildSettingOnlyDeps buildSettings) $
              die' verbosity
                "The haddock command does not support '--only-dependencies'."

              -- When we interpret the targets on the command line, interpret them as
              -- haddock targets
            targets <- either reportHaddockTargetProblems return
                   =<< resolveTargets
                         (selectPackageTargets haddockFlags)
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         userTargets

            --TODO: [required eventually] handle no targets case
            when (Map.null targets) $
              fail "TODO handle no targets case"

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionHaddock
                                    targets
                                    elaboratedPlan
            return elaboratedPlan'
        }

    printPlan verbosity buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity buildCtx
    runProjectPostBuildPhase verbosity buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- For haddock: select all buildable libraries, and if the --executables flag
-- is on select all the buildable exes. Do similarly for test-suites,
-- benchmarks and foreign libs.
--
-- There are no failure cases, if there's none of any class, we skip it.
--
selectPackageTargets  :: HaddockFlags -> BuildTarget PackageId
                      -> [AvailableTarget k] -> Either HaddockTargetProblem [k]
selectPackageTargets haddockFlags _bt ts =
    Right [ k | AvailableTarget {
                  availableTargetStatus        = TargetBuildable k _,
                  availableTargetComponentName = cname
                } <- ts
              , isRequested cname ]
  where
    isRequested CLibName      = True
    isRequested CSubLibName{} = True --TODO: unclear if this should be always on
    isRequested CFLibName{}   = fromFlag (haddockForeignLibs haddockFlags)
    isRequested CExeName{}    = fromFlag (haddockExecutables haddockFlags)
    isRequested CTestName{}   = fromFlag (haddockTestSuites  haddockFlags)
    isRequested CBenchName{}  = fromFlag (haddockBenchmarks  haddockFlags)


-- For checking an individual component target, for build there's no
-- additional checks we need beyond the basic ones.
--
selectComponentTarget :: BuildTarget PackageId
                      -> AvailableTarget k -> Either HaddockTargetProblem k
selectComponentTarget bt =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic bt

data HaddockTargetProblem =
     TargetPackageNoBuildableLibs
   | TargetPackageNoBuildableExes
   | TargetPackageNoEnabledTargets
   | TargetPackageNoTargets
   | TargetProblemCommon TargetProblem
  deriving Show

reportHaddockTargetProblems :: [HaddockTargetProblem] -> IO a
reportHaddockTargetProblems = fail . show
