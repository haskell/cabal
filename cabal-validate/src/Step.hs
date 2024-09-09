-- | The steps that can be run by @cabal-validate@.

module Step
  ( Step (..)
  , displayStep
  , nameToStep
  , parseStep
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A step to be run by @cabal-validate@.
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

-- | Get the display identifier for a given `Step`.
--
-- This is used to parse the @--step@ command-line argument.
--
-- Note that these names are just kebab-case variants of the `Step` constructor
-- names; they do not attempt to describe the steps.
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

-- | A map from step names to `Steps`.
--
-- This is an inverse of `displayStep`.
nameToStep :: Map String Step
nameToStep =
  Map.fromList
    [ (displayStep step, step)
    | step <- [minBound .. maxBound]
    ]

-- | Parse a string as a `Step`.
parseStep :: String -> Maybe Step
parseStep step = Map.lookup step nameToStep
