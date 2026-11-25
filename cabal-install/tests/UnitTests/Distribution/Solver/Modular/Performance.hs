{-# LANGUAGE LambdaCase #-}

-- | Package databases that make particular parts of the dependency solver slow.
-- Each scenario names the code path it stresses and takes its sizes as
-- parameters, so that the cost of that path can be scaled up until it shows.
module UnitTests.Distribution.Solver.Modular.Performance
  ( Scenario (..)
  , scenarios
  ) where

import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils

-- | A solver problem with sizes that can be varied.
data Scenario = Scenario
  { scenarioName :: String
  , scenarioSummary :: String
  , scenarioSizes :: [(String, Int)]
  -- ^ Names of the size parameters, with their default values.
  , scenarioTest :: [Int] -> SolverTest
  -- ^ Builds the test from one value for each size parameter.
  }

scenarios :: [Scenario]
scenarios =
  [ Scenario
      { scenarioName = "skip-wide"
      , scenarioSummary = "skip versions of a package with many dependencies and many conflicts"
      , scenarioSizes = [("versions", 200), ("conflicts", 1000), ("deps", 200)]
      , scenarioTest = \case
          [v, k, d] -> skipWide v k d
          _ -> error "skip-wide takes versions, conflicts and deps"
      }
  , Scenario
      { scenarioName = "skip-ranges"
      , scenarioSummary = "skip versions that each add a different version range to a conflict set"
      , scenarioSizes = [("versions", 1000), ("range-parts", 8)]
      , scenarioTest = \case
          [v, m] -> skipRanges v m
          _ -> error "skip-ranges takes versions and range-parts"
      }
  ]

-- | Package @a@ has @v@ versions. Every version has @d@ dependencies that are
-- easy to satisfy and one dependency on a version of @x@ that doesn't exist.
-- Package @x@ has @k@ versions. The solver chooses @a@ before @x@.
--
-- The newest @a@ rules out every version of @x@, which leaves @k@ version
-- constraint conflicts against @a@. For each older version of @a@, the solver
-- then asks whether that version could resolve any of the @k@ conflicts,
-- before skipping it. Each of those checks looks through the @d + 1@
-- dependencies of that version of @a@, so the work is proportional to
-- @v * k * d@.
--
-- All versions of @a@ use the same version range for @x@, so that the conflict
-- set for @x@ stays small.
skipWide :: Int -> Int -> Int -> SolverTest
skipWide v k d =
  goalOrder [P QualNone "a", P QualNone "x"] $
    mkTest pkgs "skip-wide" ["target"] anySolverFailure
  where
    pkgs :: ExampleDb
    pkgs =
      map Right $
        [exAv "target" 1 [ExAny "a"]]
          ++ [exAv "a" i (fillerDeps ++ [ExRange "x" (k + 1) (k + 2)]) | i <- [1 .. v]]
          ++ [exAv "x" i [] | i <- [1 .. k]]
          ++ [exAv (filler i) 1 [] | i <- [1 .. d]]

    fillerDeps :: [ExampleDependency]
    fillerDeps = [ExAny (filler i) | i <- [1 .. d]]

    filler :: Int -> ExamplePkgName
    filler i = "filler-" ++ show i

-- | Package @a@ has @v@ versions. Each depends on a version of @x@ that
-- doesn't exist, through @m@ overlapping version ranges that are different for
-- every version of @a@. Package @x@ has one version. The solver chooses @a@
-- before @x@.
--
-- Each version of @a@ that the solver skips adds a version conflict to the
-- conflict set for @x@. That conflict holds the intersection of the @m@ ranges,
-- so the set ends up with @v@ different ranges, each with @m@ parts. The solver
-- compares these ranges with each other every time it inserts into the set or
-- takes a union of two conflict sets.
skipRanges :: Int -> Int -> SolverTest
skipRanges v m =
  goalOrder [P QualNone "a", P QualNone "x"] $
    mkTest pkgs "skip-ranges" ["target"] anySolverFailure
  where
    pkgs :: ExampleDb
    pkgs =
      map Right $
        [exAv "target" 1 [ExAny "a"]]
          ++ [exAv "a" i (xRanges i) | i <- [1 .. v]]
          ++ [exAv "x" 1 []]

    -- Ranges that all contain the version (i + m + 1) of x and nothing below
    -- (i + m).
    xRanges :: Int -> [ExampleDependency]
    xRanges i = [ExRange "x" (i + j) (i + j + m + 2) | j <- [1 .. m]]
