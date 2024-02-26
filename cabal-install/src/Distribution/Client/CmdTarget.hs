{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

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
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
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
    , commandSynopsis = "List target forms within the project."
    , commandUsage = usageAlternatives "v2-target" ["[TARGETS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "List targets within a build plan. "
            ++ "If no [TARGETS] are given 'all' will be used for selecting a build plan.\n\n"
            ++ "The given target can be;\n"
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
          ++ "    List all targets of the package in the current directory "
          ++ "or all packages in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-target pkgname\n"
          ++ "    List targets of the package named pkgname in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-target ./pkgfoo\n"
          ++ "    List targets of the package in the ./pkgfoo directory\n"
          ++ "  "
          ++ pname
          ++ " v2-target cname\n"
          ++ "    List targets of the component named cname in the project\n"
          ++ "  "
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = const []
    }

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

targetAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
targetAction flags@NixStyleFlags{..} ts globalFlags = do
  let targetStrings = if null ts then ["all"] else ts
  withContextAndSelectors RejectNoTargets Nothing flags targetStrings globalFlags BuildCommand $ \targetCtx ctx targetSelectors -> do
    baseCtx <- case targetCtx of
      ProjectContext -> return ctx
      GlobalContext -> return ctx
      ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
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

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems =
  reportTargetProblems verbosity "target" problems

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
  dieWithException verbosity . ReportCannotPruneDependencies . renderCannotPruneDependencies
