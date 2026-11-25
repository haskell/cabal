-- Keep GHC from sharing one solver run between the timed runs.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Times the dependency solver on the scenarios in
-- "UnitTests.Distribution.Solver.Modular.Performance". The times leave out
-- making the package database for a scenario.
--
-- > solver-perf [--runs N] [SCENARIO [SIZE ...]]
--
-- Without a scenario, it runs them all with their default sizes.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (intercalate, isInfixOf, sort)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Mem (getAllocationCounter, performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Distribution.Client.Dependency (foldProgress)

import UnitTests.Distribution.Solver.Modular.DSL (ExampleIndexes, extractInstallPlan, forceExIndexes)
import UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils
import UnitTests.Distribution.Solver.Modular.Performance

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let (runs, rest) = case args of
        "--runs" : n : xs | Just n' <- readMaybe n -> (n', xs)
        xs -> (5, xs)
  case rest of
    [] -> forM_ scenarios $ \s -> run runs s (map snd $ scenarioSizes s)
    name : sizeArgs ->
      case [s | s <- scenarios, scenarioName s == name] of
        [s] -> do
          sizes <- case traverse readMaybe sizeArgs of
            Just [] -> return (map snd $ scenarioSizes s)
            Just xs | length xs == length (scenarioSizes s) -> return xs
            _ ->
              die $
                name ++ " takes these sizes: " ++ unwords (map fst $ scenarioSizes s)
          run runs s sizes
        _ ->
          die $
            unlines $
              "Scenarios:"
                : [ "  " ++ scenarioName s ++ " " ++ unwords (map fst $ scenarioSizes s) ++ "\n      " ++ scenarioSummary s
                  | s <- scenarios
                  ]

-- | What a solver run produced, in a form that can be compared between runs.
data Outcome = Failed Int | Solved Int
  deriving (Eq, Show)

run :: Int -> Scenario -> [Int] -> IO ()
run runs s sizes = do
  let test = scenarioTest s sizes
      sizeText = intercalate ", " [n ++ " = " ++ show x | ((n, _), x) <- zip (scenarioSizes s) sizes]
  printf "%s (%s)\n" (scenarioName s) sizeText

  -- An untimed run with the full log, to show which path the solver took.
  let logLines = foldProgress (:) (const []) (const []) (solverTestProgress $ setVerbose test)
      count p = length (filter p logLines)
  printf
    "  log: %d lines, %d skipping, %d backjumps\n"
    (length logLines)
    (count ("skipping:" `isInfixOf`))
    (count ("Backjumping" `isInfixOf`) + count ("backjumping" `isInfixOf`))

  -- The package indexes are made and checked once, outside the timings. Each
  -- timed run gets them through a reference that GHC can't see through, so that
  -- no run reuses the solver's work from another.
  let idxs = solverTestIndexes test
  () <- evaluate (forceExIndexes idxs)
  ref <- newIORef (idxs, test)
  results <- replicateM runs $ do
    (idxs', test') <- readIORef ref
    performGC
    alloc0 <- getAllocationCounter
    t0 <- getMonotonicTimeNSec
    outcome <- evaluate (solve idxs' test')
    t1 <- getMonotonicTimeNSec
    alloc1 <- getAllocationCounter
    return (outcome, t1 - t0, alloc0 - alloc1)

  let outcomes = [o | (o, _, _) <- results]
      times = sort [t | (_, t, _) <- results]
      allocs = sort [a | (_, _, a) <- results]
      median xs = xs !! (length xs `div` 2)
  case outcomes of
    [] -> die "  no runs"
    o : os -> do
      unless (all (== o) os) $ die "  the runs gave different results"
      printf "  result: %s\n" (show o)
      printf
        "  time: min %.1f ms, median %.1f ms; allocated %.1f MB (%d runs)\n"
        (ms $ minimum times)
        (ms $ median times)
        (fromIntegral (median allocs) / (1024 * 1024) :: Double)
        runs
  where
    ms :: Word64 -> Double
    ms t = fromIntegral t / 1e6

-- | Runs the solver to the end. A failure is summarised by the length of its
-- error message and a success by the size of its plan, which forces both.
solve :: ExampleIndexes -> SolverTest -> Outcome
solve idxs =
  foldProgress
    (\_ r -> r)
    (\err -> let n = length err in n `seq` Failed n)
    (\plan -> let n = length (extractInstallPlan plan) in n `seq` Solved n)
    . solverTestProgressWith idxs
