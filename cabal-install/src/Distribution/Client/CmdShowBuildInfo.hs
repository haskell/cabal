{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | cabal-install CLI command: show-build-info
--
module Distribution.Client.CmdShowBuildInfo (
    -- * The @show-build-info@ CLI and action
    showBuildInfoCommand,
    showBuildInfoAction
  ) where

import Distribution.Client.Compat.Prelude
         ( for )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages

import Distribution.Client.Setup
         ( GlobalFlags )
import Distribution.Client.TargetProblem
         ( TargetProblem', TargetProblem (TargetProblemNoneEnabled, TargetProblemNoTargets) )
import Distribution.Simple.Setup
         ( configVerbosity, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), option, reqArg', usageAlternatives )
import Distribution.Verbosity
         ( Verbosity, silent )
import Distribution.Simple.Utils
         ( wrapText, withOutputMarker )

import qualified Data.Map as Map
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.DistDirLayout
        ( distProjectRootDirectory, DistDirLayout (distProjectCacheDirectory) )

import Distribution.Simple.ShowBuildInfo
import Distribution.Utils.Json

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import Distribution.Types.UnitId (unUnitId)

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
     ++ "    Shows build information about the package located in './pkgname'\n",
  commandOptions = nixStyleOptions $ \_ ->
      [ option [] ["buildinfo-json-output"]
              "Write the result to the given file instead of stdout"
              buildInfoOutputFile (\pf flags -> flags { buildInfoOutputFile = pf })
              (reqArg' "FILE" Just (maybe [] pure))
      ],
  commandDefaultFlags = defaultNixStyleFlags defaultShowBuildInfoFlags
  }

data ShowBuildInfoFlags = ShowBuildInfoFlags
    { buildInfoOutputFile :: Maybe FilePath
    }

defaultShowBuildInfoFlags :: ShowBuildInfoFlags
defaultShowBuildInfoFlags = ShowBuildInfoFlags
    { buildInfoOutputFile = Nothing
    }

-- | The @show-build-info@ exports information about a package and the compiler
-- configuration used to build it as JSON, that can be used by other tooling.
-- See "Distribution.Simple.ShowBuildInfo" for more information.
showBuildInfoAction :: NixStyleFlags ShowBuildInfoFlags -> [String] -> GlobalFlags -> IO ()
showBuildInfoAction flags@NixStyleFlags { extraFlags = (ShowBuildInfoFlags fileOutput), ..}
  targetStrings globalFlags = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand

  targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                  =<< readTargetSelectors (localPackages baseCtx) Nothing targetStrings

  buildCtx <-
    runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).
      targets <- either (reportShowBuildInfoTargetProblems verbosity) return
                $ resolveTargets
                    selectPackageTargets
                    selectComponentTarget
                    elaboratedPlan
                    Nothing
                    targetSelectors

      let elaboratedPlan' = pruneInstallPlanToTargets
                        TargetActionBuildInfo
                        targets
                        elaboratedPlan

      return (elaboratedPlan', targets)

  buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
  runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

  let tm = targetsMap buildCtx
  let units = Map.keys tm
  let layout = distDirLayout baseCtx
  let dir = distProjectCacheDirectory layout </> "buildinfo"
  componentBuildInfos <- for units $ \unit -> do
    let fp = dir </> (unUnitId unit) <.> "json"
    T.strip <$> T.readFile fp

  let compilerInfo = mkCompilerInfo
        (pkgConfigCompilerProgs (elaboratedShared buildCtx))
        (pkgConfigCompiler (elaboratedShared buildCtx))

      components = map JsonRaw componentBuildInfos
      fields = mkBuildInfo' compilerInfo components
      json = JsonObject $ fields <>
        [ ("project-root", JsonString (T.pack (addTrailingPathSeparator $ distProjectRootDirectory (distDirLayout baseCtx))))
        ]
      res = renderJson json ""

  case fileOutput of
    Nothing -> T.putStrLn $ T.pack $ withOutputMarker verbosity (T.unpack res)
    Just fp -> T.writeFile fp res

  where
    -- Default to silent verbosity otherwise it will pollute our json output
    verbosity = fromFlagOrDefault silent (configVerbosity configFlags)
    -- Also shut up haddock since it dumps warnings to stdout
    -- flags' = flags { haddockFlags = haddockFlags { haddockVerbosity = Flag silent }
    --                , configFlags = configFlags { Cabal.configTests = Flag True
    --                                            , Cabal.configBenchmarks = Flag True
    --                                            }
    --                }
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags
                  mempty -- ClientInstallFlags, not needed here

-- | This defines what a 'TargetSelector' means for the @show-build-info@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @show-build-info@ command select all components except non-buildable and disabled
-- tests\/benchmarks, fail if there are no such components
--
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
    targetsBuildable = selectBuildableTargetsWith
                         (buildable targetSelector)
                         targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

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