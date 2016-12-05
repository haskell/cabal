{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: haddock
--
module Distribution.Client.CmdHaddock (
    haddockCommand,
    haddockAction,
  ) where

import Distribution.Client.ProjectOrchestration

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
  commandSynopsis     = "Build Haddock documentation",
  commandUsage        = usageAlternatives "new-haddock" [ "[FLAGS] TARGET" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Build Haddock documentation for the specified packages within the "
     ++ "project.\n\n"

     ++ "Any package in the project can be specified. If no package is "
     ++ "specified, the default is to build the documentation for the package "
     ++ "in the current directory. The default behaviour is to build "
     ++ "documentation for the exposed modules of the library component (if "
     ++ "any). This can be changed with the '--internal', '--executables', "
     ++ "'--tests', '--benchmarks' or '--all' flags.\n\n"

     ++ "Currently, documentation for dependencies is NOT built. This "
     ++ "behavior may change in future.\n\n"

     ++ "Additional configuration flags can be specified on the command line "
     ++ "and these extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-haddock pkgname"
     ++ "    Build documentation for the package named pkgname\n"
   }
   --TODO: [nice to have] support haddock on specific components, not just
   -- whole packages and the silly --executables etc modifiers.

-- | The @haddock@ command is TODO.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
haddockAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
                 -> [String] -> GlobalFlags -> IO ()
haddockAction (configFlags, configExFlags, installFlags, haddockFlags)
                targetStrings globalFlags = do

    baseCtx <- establishProjectBaseContext verbosity cliConfig
                                           configFlags installFlags --TODO: eliminate use of legacy config types

    targetSelectors <- readTargetSelectors verbosity (localPackages baseCtx) targetStrings

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            when (buildSettingOnlyDeps (buildSettings baseCtx)) $
              die' verbosity
                "The haddock command does not support '--only-dependencies'."

              -- When we interpret the targets on the command line, interpret them as
              -- haddock targets
            targets <- either reportHaddockTargetProblems return
                     $ resolveTargets
                         (selectPackageTargets haddockFlags)
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         targetSelectors

            --TODO: [required eventually] handle no targets case
            when (Map.null targets) $
              fail "TODO handle no targets case"

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionHaddock
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

-- For haddock: select all buildable libraries, and if the --executables flag
-- is on select all the buildable exes. Do similarly for test-suites,
-- benchmarks and foreign libs.
--
-- There are no failure cases, if there's none of any class, we skip it.
--
selectPackageTargets  :: HaddockFlags -> TargetSelector PackageId
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
selectComponentTarget :: TargetSelector PackageId
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
