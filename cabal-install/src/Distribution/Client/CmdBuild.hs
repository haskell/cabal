{-# LANGUAGE RecordWildCards #-}

-- | cabal-install CLI command: build
module Distribution.Client.CmdBuild
  ( -- * The @build@ CLI and action
    buildCommand
  , buildAction
  , BuildFlags (..)
  , defaultBuildFlags

    -- * Internals exposed for testing
  , selectPackageTargets
  , selectComponentTarget
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdErrorMessages
import Distribution.Client.ProjectFlags
  ( removeIgnoreProjectOption
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  , TargetProblem'
  )

import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags
  , yesNoOpt
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , option
  , usageAlternatives
  )
import Distribution.Simple.Flag (Flag (..), fromFlag, fromFlagOrDefault, toFlag)
import Distribution.Simple.Utils
  ( die'
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )

import qualified Data.Map as Map

buildCommand :: CommandUI (NixStyleFlags BuildFlags)
buildCommand =
  CommandUI
    { commandName = "v2-build"
    , commandSynopsis = "Compile targets within the project."
    , commandUsage = usageAlternatives "v2-build" ["[TARGETS] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Build one or more targets from within the project. The available "
            ++ "targets are the packages in the project as well as individual "
            ++ "components within those packages, including libraries, executables, "
            ++ "test-suites or benchmarks. Targets can be specified by name or "
            ++ "location. If no target is specified then the default is to build "
            ++ "the package in the current directory.\n\n"
            ++ "Dependencies are built or rebuilt as necessary. Additional "
            ++ "configuration flags can be specified on the command line and these "
            ++ "extend the project configuration from the 'cabal.project', "
            ++ "'cabal.project.local' and other files."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-build\n"
          ++ "    Build the package in the current directory "
          ++ "or all packages in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-build pkgname\n"
          ++ "    Build the package named pkgname in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-build ./pkgfoo\n"
          ++ "    Build the package in the ./pkgfoo directory\n"
          ++ "  "
          ++ pname
          ++ " v2-build cname\n"
          ++ "    Build the component named cname in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-build cname --enable-profiling\n"
          ++ "    Build the component in profiling mode "
          ++ "(including dependencies as needed)\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultBuildFlags
    , commandOptions =
        removeIgnoreProjectOption
          . nixStyleOptions
            ( \showOrParseArgs ->
                [ option
                    []
                    ["only-configure"]
                    "Instead of performing a full build just run the configure step"
                    buildOnlyConfigure
                    (\v flags -> flags{buildOnlyConfigure = v})
                    (yesNoOpt showOrParseArgs)
                ]
            )
    }

data BuildFlags = BuildFlags
  { buildOnlyConfigure :: Flag Bool
  }

defaultBuildFlags :: BuildFlags
defaultBuildFlags =
  BuildFlags
    { buildOnlyConfigure = toFlag False
    }

-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
buildAction :: NixStyleFlags BuildFlags -> [String] -> GlobalFlags -> IO ()
buildAction flags@NixStyleFlags{extraFlags = buildFlags, ..} targetStrings globalFlags =
  withContextAndSelectors RejectNoTargets Nothing flags targetStrings globalFlags BuildCommand $ \targetCtx ctx targetSelectors -> do
    -- TODO: This flags defaults business is ugly
    let onlyConfigure =
          fromFlag
            ( buildOnlyConfigure defaultBuildFlags
                <> buildOnlyConfigure buildFlags
            )
        targetAction
          | onlyConfigure = TargetActionConfigure
          | otherwise = TargetActionBuild

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
                targetAction
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

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @build@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
selectPackageTargets
  :: TargetSelector
  -> [AvailableTarget k]
  -> Either TargetProblem' [k]
selectPackageTargets targetSelector targets
  -- If there are any buildable targets then we select those
  | not (null targetsBuildable) =
      Right targetsBuildable
  -- If there are targets but none are buildable then we report those
  | not (null targets) =
      Left (TargetProblemNoneEnabled targetSelector targets')
  -- If there are no targets at all then we report that
  | otherwise =
      Left (TargetProblemNoTargets targetSelector)
  where
    targets' = forgetTargetsDetail targets
    targetsBuildable =
      selectBuildableTargetsWith
        (buildable targetSelector)
        targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _ Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems =
  reportTargetProblems verbosity "build" problems

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
  die' verbosity . renderCannotPruneDependencies
