{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: run
--
module Distribution.Client.CmdRun (
    runCommand,
    runAction,
  ) where

import Distribution.Client.ProjectOrchestration

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText, die' )

import qualified Data.Set as Set
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

     ++ cmdCommonHelpTextNewBuildBeta
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

    baseCtx <- establishProjectBaseContext verbosity cliConfig

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                   =<< readTargetSelectors (localPackages baseCtx) targetStrings

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            when (buildSettingOnlyDeps (buildSettings baseCtx)) $
              die' verbosity $
                  "The run command does not support '--only-dependencies'. "
               ++ "You may wish to use 'build --only-dependencies' and then "
               ++ "use 'run'."

            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         targetSelectors

            -- Reject multiple targets, or at least targets in different
            -- components. It is ok to have two module/file targets in the
            -- same component, but not two that live in different components.
            when (Set.size (distinctTargetComponents targets) > 1) $
              reportTargetProblems verbosity
                [TargetProblemMultipleTargets targets]

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            return elaboratedPlan'

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

-- | This defines what a 'TargetSelector' means for the @run@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @run@ command we select the exe if there is only one and it's
-- buildable. Fail if there are no or multiple buildable exe components.
--
selectPackageTargets :: TargetSelector PackageId
                     -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsExesBuildable')

    -- If there are executables but none are buildable then we report those
  | not (null targetsExes)
  = Left (TargetProblemNoneEnabled targetSelector targetsExes)

    -- If there are no executables but some other targets then we report that
  | not (null targets)
  = Left (TargetProblemNoExes targetSelector)

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    (targetsExesBuildable,
     targetsExesBuildable') = selectBuildableTargets'
                            . filterTargetsKind ExeKind
                            $ targets

    targetsExes             = forgetTargetsDetail
                            . filterTargetsKind ExeKind
                            $ targets


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @run@ command we just need to check it is a executable, in addition
-- to the basic checks on being buildable etc.
--
selectComponentTarget :: PackageId -> ComponentName -> SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem  k
selectComponentTarget pkgid cname subtarget@WholeComponent t
  | CExeName _ <- availableTargetComponentName t
  = either (Left . TargetProblemCommon) return $
           selectComponentTargetBasic pkgid cname subtarget t
  | otherwise
  = Left (TargetProblemComponentNotExe pkgid cname)

selectComponentTarget pkgid cname subtarget _
  = Left (TargetProblemIsSubComponent pkgid cname subtarget)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @run@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon
     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled (TargetSelector PackageId) [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   (TargetSelector PackageId)

     -- | The 'TargetSelector' matches targets but no executables
   | TargetProblemNoExes      (TargetSelector PackageId)

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple (TargetSelector PackageId) [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap

     -- | The 'TargetSelector' refers to a component that is not an executable
   | TargetProblemComponentNotExe PackageId ComponentName

     -- | Asking to run an individual file or module is not supported
   | TargetProblemIsSubComponent  PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem = show
