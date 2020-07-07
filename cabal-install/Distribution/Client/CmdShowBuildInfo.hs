{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | cabal-install CLI command: show-build-info
--
module Distribution.Client.CmdShowBuildInfo (
    -- * The @show-build-info@ CLI and action
    showBuildInfoCommand,
    showBuildInfoAction
  ) where

import Distribution.Client.Compat.Prelude
         (catMaybes, fromMaybe )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
import Distribution.Client.TargetProblem
         ( TargetProblem (..), TargetProblem' )

import Distribution.Client.Setup
         ( GlobalFlags )
import Distribution.Simple.Setup
         (Flag(..), haddockVerbosity, configVerbosity, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), option, reqArg', usageAlternatives )
import Distribution.Verbosity
         (Verbosity, silent )
import Distribution.Simple.Utils
         (wrapText, die' )
import Distribution.Types.UnitId
         ( mkUnitId )
import Distribution.Pretty
         ( prettyShow )

import qualified Data.Map as Map
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Client.ProjectBuilding.Types
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.DistDirLayout
        (distProjectRootDirectory )

import Distribution.Simple.ShowBuildInfo
import Distribution.Utils.Json

import Control.Monad (forM_, unless)
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T

showBuildInfoCommand :: CommandUI (NixStyleFlags ShowBuildInfoFlags)
showBuildInfoCommand = CommandUI {
  commandName         = "show-build-info",
  commandSynopsis     = "Show project build information",
  commandUsage        = usageAlternatives "show-build-info" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Provides detailed json output for the given package.\n"
     ++ "Contains information about the different build components and compiler flags.\n",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " show-build-info\n"
     ++ "    Shows build information about the current package\n"
     ++ "  " ++ pname ++ " show-build-info .\n"
     ++ "    Shows build information about the current package\n"
     ++ "  " ++ pname ++ " show-build-info ./pkgname \n"
     ++ "    Shows build information about the package located in './pkgname'\n"
     ++ cmdCommonHelpTextNewBuildBeta,
  commandOptions = nixStyleOptions $ \_ ->
      [ option [] ["buildinfo-json-output"]
              "Write the result to the given file instead of stdout"
              buildInfoOutputFile (\pf flags -> flags { buildInfoOutputFile = pf })
              (reqArg' "FILE" Just (maybe [] pure)),
        option [] ["unit-ids-json"]
              "Show build-info only for selected unit-id's."
              buildInfoUnitIds (\pf flags -> flags { buildInfoUnitIds = pf })
              (reqArg' "UNIT-ID" (Just . words) (fromMaybe []))
      ],
  commandDefaultFlags = defaultNixStyleFlags defaultShowBuildInfoFlags
  }

data ShowBuildInfoFlags = ShowBuildInfoFlags
    { buildInfoOutputFile :: Maybe FilePath
    , buildInfoUnitIds :: Maybe [String]
    }

defaultShowBuildInfoFlags :: ShowBuildInfoFlags
defaultShowBuildInfoFlags = ShowBuildInfoFlags
    { buildInfoOutputFile = Nothing
    , buildInfoUnitIds = Nothing
    }

-- | The @show-build-info@ exports information about a package and the compiler
-- configuration used to build it as JSON, that can be used by other tooling.
-- See "Distribution.Simple.ShowBuildInfo" for more information.
showBuildInfoAction :: NixStyleFlags ShowBuildInfoFlags -> [String] -> GlobalFlags -> IO ()
showBuildInfoAction flags@NixStyleFlags { extraFlags = (ShowBuildInfoFlags fileOutput unitIdStrs), ..}
  targetStrings globalFlags = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand
  let baseCtx' = baseCtx
        { buildSettings = (buildSettings baseCtx) { buildSettingDryRun = True }
        }

  targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                  =<< readTargetSelectors (localPackages baseCtx') Nothing flags targetStrings

  buildCtx <-
    runProjectPreBuildPhase verbosity baseCtx' $ \elaboratedPlan -> do
      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).

      targets' <- either (reportShowBuildInfoTargetProblems verbosity) return
                $ resolveTargets
                    selectPackageTargets
                    selectComponentTarget
                    elaboratedPlan
                    Nothing
                    targetSelectors

      let unitIds = map mkUnitId <$> unitIdStrs

      -- Check that all the unit ids exist
      forM_ (fromMaybe [] unitIds) $ \ui ->
        unless (Map.member ui targets') $
          die' verbosity ("No unit " ++ prettyShow ui)

      -- Filter out targets that aren't in the specified unit ids
      let targets = Map.filterWithKey (\k _ -> maybe True (elem k) unitIds) targets'
          elaboratedPlan' = pruneInstallPlanToTargets
                        TargetActionBuildInfo
                        targets
                        elaboratedPlan

      return (elaboratedPlan', targets)

  buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
  runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

  -- We can ignore the errors here, since runProjectPostBuildPhase should
  -- have already died and reported them if they exist
  let (_errs, buildResults) = partitionEithers $ Map.elems buildOutcomes

  let componentBuildInfos =
        concatMap T.lines $ -- Component infos are returned each on a newline
        catMaybes (buildResultBuildInfo <$> buildResults)

  let compilerInfo = mkCompilerInfo
        (pkgConfigCompilerProgs (elaboratedShared buildCtx))
        (pkgConfigCompiler (elaboratedShared buildCtx))

      components = map JsonRaw componentBuildInfos
      fields = mkBuildInfo' compilerInfo components
      json = JsonObject $ fields <>
        [ ("project-root", JsonString (T.pack (distProjectRootDirectory (distDirLayout baseCtx))))
        ]
      res = renderJson json ""

  case fileOutput of
    Nothing -> T.putStrLn res
    Just fp -> T.writeFile fp res

  where
    -- Default to silent verbosity otherwise it will pollute our json output
    verbosity = fromFlagOrDefault silent (configVerbosity configFlags)
    -- Also shut up haddock since it dumps warnings to stdout
    flags' = flags { haddockFlags = haddockFlags { haddockVerbosity = Flag silent }
                   , configFlags = configFlags { Cabal.configTests = Flag True
                                               , Cabal.configBenchmarks = Flag True
                                               }
                   }
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags'
                  mempty -- ClientInstallFlags, not needed here

-- | This defines what a 'TargetSelector' means for the @show-build-info@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @show-build-info@ command select all components. Unlike the @build@
-- command, we want to show info for tests and benchmarks even without the
-- @--enable-tests@\/@--enable-benchmarks@ flag set.
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either TargetProblem' [k]
selectPackageTargets targetSelector targets

    -- If there are any buildable targets then we select those
  | not (null targetsBuildable)
  = Right targetsBuildable

    -- If there are targets but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targets')

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targets'         = forgetTargetsDetail targets
    targetsBuildable = selectBuildableTargets targets

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @show-build-info@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic


reportShowBuildInfoTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportShowBuildInfoTargetProblems verbosity problems =
  reportTargetProblems verbosity "show-build-info" problems
