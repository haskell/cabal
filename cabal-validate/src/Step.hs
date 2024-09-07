module Step
  ( Step (..)
  , displayStep
  , nameToStep
  , parseStep
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Step
  = PrintConfig
  | PrintToolVersions
  | Build
  | Doctest
  | LibTests
  | LibSuite
  | LibSuiteExtras
  | CliTests
  | CliSuite
  | SolverBenchmarksTests
  | SolverBenchmarksRun
  | TimeSummary
  deriving (Eq, Enum, Bounded, Show)

displayStep :: Step -> String
displayStep step =
  case step of
    PrintConfig -> "print-config"
    PrintToolVersions -> "print-tool-versions"
    Build -> "build"
    Doctest -> "doctest"
    LibTests -> "lib-tests"
    LibSuite -> "lib-suite"
    LibSuiteExtras -> "lib-suite-extras"
    CliTests -> "cli-tests"
    CliSuite -> "cli-suite"
    SolverBenchmarksTests -> "solver-benchmarks-tests"
    SolverBenchmarksRun -> "solver-benchmarks-run"
    TimeSummary -> "time-summary"

nameToStep :: Map String Step
nameToStep =
  Map.fromList
    [ (displayStep step, step)
    | step <- [minBound .. maxBound]
    ]

parseStep :: String -> Maybe Step
parseStep step = Map.lookup step nameToStep
