{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: test
--
module Distribution.Client.CmdTest (
    testCommand,
    testAction,
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
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Verbosity
         ( normal )
import Distribution.Simple.Utils
         ( wrapText, die' )

import qualified Data.Map as Map
import Control.Monad (when)


testCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
testCommand = Client.installCommand {
  commandName         = "new-test",
  commandSynopsis     = "Run test-suites",
  commandUsage        = usageAlternatives "new-test" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Runs the specified test-suites, first ensuring they are up to "
     ++ "date.\n\n"

     ++ "Any test-suite in any package in the project can be specified. "
     ++ "A package can be specified in which case all the test-suites in the "
     ++ "package are run. The default is to run all the test-suites in the "
     ++ "package in the current directory.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-test\n"
     ++ "    Run all the test-suites in the package in the current directory\n"
     ++ "  " ++ pname ++ " new-test pkgname\n"
     ++ "    Run all the test-suites in the package named pkgname\n"
     ++ "  " ++ pname ++ " new-test cname\n"
     ++ "    Run the test-suite named cname\n"
     ++ "  " ++ pname ++ " new-test cname --enable-coverage\n"
     ++ "    Run the test-suite built with code coverage (including local libs used)\n\n"

     ++ "Note: this command is part of the new project-based system (aka "
     ++ "nix-style\nlocal builds). These features are currently in beta. "
     ++ "Please see\n"
     ++ "http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html "
     ++ "for\ndetails and advice on what you can expect to work. If you "
     ++ "encounter problems\nplease file issues at "
     ++ "https://github.com/haskell/cabal/issues and if you\nhave any time "
     ++ "to get involved and help with testing, fixing bugs etc then\nthat "
     ++ "is very much appreciated.\n"
   }


-- | The @test@ command is very much like @build@. It brings the install plan
-- up to date, selects that part of the plan needed by the given or implicit
-- test target(s) and then executes the plan.
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

          hookSelectPlanSubset = \buildSettings' elaboratedPlan -> do
            when (buildSettingOnlyDeps buildSettings') $
              die' verbosity $
                  "The test command does not support '--only-dependencies'. "
               ++ "You may wish to use 'build --only-dependencies' and then "
               ++ "use 'test'."

            -- Interpret the targets on the command line as test targets
            -- (as opposed to say build or haddock targets).
            targets <- either reportTestTargetProblems return
                   =<< resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         userTargets

            --TODO: [required eventually] handle no targets case
            when (Map.null targets) $
              fail "TODO handle no targets case"

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
selectPackageTargets  :: TargetSelector PackageId
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

selectComponentTarget :: TargetSelector PackageId
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
