{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: new-test
--
module Distribution.Client.CmdTest (
    testCommand,
    testAction,
  ) where

import Distribution.Client.ProjectOrchestration
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

testCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
testCommand = Client.installCommand {
  commandName         = "new-test",
  commandSynopsis     = "Perform new-build and run tests",
  commandUsage        = usageAlternatives "new-test" [ "[FLAGS] TARGET" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Build and run test targets",
  commandNotes        = Just $ \_pname ->
        "Examples:\n"
   }

-- | The @test@ command is very much like @build@. It brings the install plan
-- up to date, selects that part of the plan needed by the given or implicit
-- test arget(s) and then executes the plan.
--
-- Compared to @build@ the difference is that there's also test targets
-- which are ephemeral.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
testAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> GlobalFlags -> IO ()
testAction (configFlags, configExFlags, installFlags, haddockFlags)
           targetStrings globalFlags = do

    userTargets <- readUserBuildTargets verbosity targetStrings

    buildCtx <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning      = \_ _ _ -> return (),

          hookSelectPlanSubset = \_buildSettings elaboratedPlan -> do
            -- Interpret the targets on the command line as test targets
            -- (as opposed to say build or haddock targets).
            targets <- either reportTestTargetProblems return
                   =<< resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         userTargets
            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionTest
                                    targets
                                    elaboratedPlan
            return elaboratedPlan'
        }

    printPlan verbosity buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity buildCtx
    runProjectPostBuildPhase verbosity buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- For test: select all buildable tests.
-- Fail if there are no tests or no buildable tests.
--
selectPackageTargets  :: BuildTarget PackageId
                      -> [AvailableTarget k] -> Either TestTargetProblem [k]
selectPackageTargets _bt ts
  | (_:_)  <- testts    = Right testts
  | (_:_)  <- alltestts = Left (TargetPackageNoEnabledTests alltestts')
  | otherwise           = Left (TargetPackageNoTests        alltestts')
  where
    alltestts  = [ t | t@(AvailableTarget (CTestName _) _) <- ts ]
    testts     = [ k | TargetBuildable k _
                         <- map availableTargetStatus alltestts ]
    alltestts' = [ fmap (const ()) t | t <- alltestts ]

selectComponentTarget :: BuildTarget PackageId
                      -> AvailableTarget k -> Either TestTargetProblem k
selectComponentTarget bt t
  | CTestName _ <- availableTargetComponentName t
  = either (Left . TargetProblemCommon) return $
           selectComponentTargetBasic bt t
  | otherwise
  = Left (TargetComponentNotTest (fmap (const ()) t))

data TestTargetProblem =
     TargetPackageNoEnabledTests [AvailableTarget ()]
   | TargetPackageNoTests        [AvailableTarget ()]
   | TargetComponentNotTest      (AvailableTarget ())
   | TargetProblemCommon          TargetProblem
  deriving Show

reportTestTargetProblems :: [TestTargetProblem] -> IO a
reportTestTargetProblems = fail . show

