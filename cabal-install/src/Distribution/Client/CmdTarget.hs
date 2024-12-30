{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.CmdTarget
  ( targetCommand
  , targetAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map
import Distribution.Client.CmdBuild (selectComponentTarget, selectPackageTargets)
import Distribution.Client.CmdErrorMessages
import Distribution.Client.Errors
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags
  )
import Distribution.Client.TargetProblem
  ( TargetProblem'
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )
import Distribution.Simple.Flag (fromFlagOrDefault)
import Distribution.Simple.Utils
  ( dieWithException
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

targetCommand :: CommandUI (NixStyleFlags ())
targetCommand =
  CommandUI
    { commandName = "v2-target"
    , commandSynopsis = "Target disclosure."
    , commandUsage = usageAlternatives "v2-target" ["[TARGETS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Reveal the targets of build plan. "
            ++ "If no [TARGETS] are given 'all' will be used for selecting a build plan.\n\n"
            ++ "A [TARGETS] item can be one of these target forms;\n"
            ++ "- a package target (e.g. [pkg:]package)\n"
            ++ "- a component target (e.g. [package:][ctype:]component)\n"
            ++ "- all packages (e.g. all)\n"
            ++ "- components of a particular type (e.g. package:ctypes or all:ctypes)\n"
            ++ "- a module target: (e.g. [package:][ctype:]module)\n"
            ++ "- a filepath target: (e.g. [package:][ctype:]filepath)\n"
            ++ "- a script target: (e.g. path/to/script)\n\n"
            ++ "The ctypes can be one of: "
            ++ "libs or libraries, "
            ++ "exes or executables, "
            ++ "tests, "
            ++ "benches or benchmarks, "
            ++ " and flibs or foreign-libraries."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-target all\n"
          ++ "    Targets of the package in the current directory "
          ++ "or all packages in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-target pkgname\n"
          ++ "    Targets of the package named pkgname in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-target ./pkgfoo\n"
          ++ "    Targets of the package in the ./pkgfoo directory\n"
          ++ "  "
          ++ pname
          ++ " v2-target cname\n"
          ++ "    Targets of the component named cname in the project\n"
          ++ "  "
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = const []
    }

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

targetAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
targetAction flags@NixStyleFlags{..} ts globalFlags = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand

  targetSelectors <-
    either (reportTargetSelectorProblems verbosity) return
      =<< readTargetSelectors (localPackages baseCtx) Nothing targetStrings

  buildCtx <- runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
    -- Interpret the targets on the command line as build targets
    -- (as opposed to say repl or haddock targets).
    targets <-
      either (reportBuildTargetProblems verbosity) return $
        resolveTargets
          selectPackageTargets
          selectComponentTarget
          elaboratedPlan
          Nothing
          targetSelectors

    let elaboratedPlan' =
          pruneInstallPlanToTargets
            TargetActionConfigure
            targets
            elaboratedPlan
    elaboratedPlan'' <-
      if buildSettingOnlyDeps (buildSettings baseCtx)
        then
          either (reportCannotPruneDependencies verbosity) return $
            pruneInstallPlanToDependencies
              (Map.keysSet targets)
              elaboratedPlan'
        else return elaboratedPlan'

    return (elaboratedPlan'', targets)

  printPlanTargetForms verbosity buildCtx
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    targetStrings = if null ts then ["all"] else ts
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems =
  reportTargetProblems verbosity "target" problems

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
  dieWithException verbosity . ReportCannotPruneDependencies . renderCannotPruneDependencies
