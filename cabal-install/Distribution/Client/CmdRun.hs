{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: run
--
module Distribution.Client.CmdRun (
    runCommand,
    runAction,
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


runCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
runCommand = Client.installCommand {
  commandName         = "new-run",
  commandSynopsis     = "Run an executable.",
  commandUsage        = usageAlternatives "new-run"
                          [ "[TARGET] [FLAGS] [-- EXECUTABLE_FLAGS]" ],
  commandDescription  = Just $ \pname -> wrapText $
        "Runs the specified executable, first ensuring it is up to date.\n\n"

     ++ "Any executable in any package in the project can be specified. "
     ++ "A package can be specified if contains just one executable. "
     ++ "The default is to use the package in the current directory if it "
     ++ "contains just one executable.\n\n"

     ++ "Extra arguments can be passed to the program, but use '--' to "
     ++ "separate arguments for the program from arguments for " ++ pname
     ++ ". The executable is run in an environment where it can find its "
     ++ "data files inplace in the build tree.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-run\n"
     ++ "    Run the executable in the package in the current directory\n"
     ++ "  " ++ pname ++ " new-run foo-tool\n"
     ++ "    Run the named executable (in any package in the project)\n"
     ++ "  " ++ pname ++ " new-run pkgfoo:foo-tool\n"
     ++ "    Run the executable 'foo-tool' in the package 'pkgfoo'\n"
     ++ "  " ++ pname ++ " new-run foo -O2 -- dothing --fooflag\n"
     ++ "    Build with '-O2' and run the program, passing it extra arguments.\n\n"

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


-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
runAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
          -> [String] -> GlobalFlags -> IO ()
runAction (configFlags, configExFlags, installFlags, haddockFlags)
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
                  "The repl command does not support '--only-dependencies'. "
               ++ "You may wish to use 'build --only-dependencies' and then "
               ++ "use 'repl'."

            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            targets <- either reportRunTargetProblems return
                   =<< resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         userTargets

            when (Map.size targets > 1) $
              let problem = TargetsMultiple (Map.elems targets)
               in reportRunTargetProblems [problem]

            --TODO: [required eventually] handle no targets case
            when (Map.null targets) $
              fail "TODO handle no targets case"

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            return elaboratedPlan'
        }

    printPlan verbosity buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity buildCtx
    runProjectPostBuildPhase verbosity buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- For run: select the exe if there is only one and it's buildable.
-- Fail if there are no or multiple buildable exe components.
--
selectPackageTargets :: BuildTarget PackageId
                     -> [AvailableTarget k] -> Either RunTargetProblem [k]
selectPackageTargets _bt ts
  | [exet] <- exets    = Right [exet]
  | (_:_)  <- exets    = Left TargetPackageMultipleExes

  | (_:_)  <- allexets = Left TargetPackageNoBuildableExes
  | otherwise          = Left TargetPackageNoTargets
  where
    allexets = [ t | t@(AvailableTarget (CExeName _) _) <- ts ]
    exets    = [ k | TargetBuildable k _ <- map availableTargetStatus allexets ]

selectComponentTarget :: BuildTarget PackageId
                      -> AvailableTarget k -> Either RunTargetProblem  k
selectComponentTarget bt t
  | CExeName _ <- availableTargetComponentName t
  = either (Left . TargetProblemCommon) return $
           selectComponentTargetBasic bt t
  | otherwise
  = Left (TargetComponentNotExe (fmap (const ()) t))

data RunTargetProblem =
     TargetPackageMultipleExes
   | TargetPackageNoBuildableExes
   | TargetPackageNoTargets
   | TargetComponentNotExe (AvailableTarget ())
   | TargetProblemCommon    TargetProblem
   | TargetsMultiple [[ComponentTarget]] --TODO: more detail needed
  deriving Show

reportRunTargetProblems :: [RunTargetProblem] -> IO a
reportRunTargetProblems = fail . show
