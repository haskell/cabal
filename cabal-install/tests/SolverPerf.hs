-- Keep GHC from sharing one solver run between the timed runs.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Times the dependency solver on the scenarios in
-- "UnitTests.Distribution.Solver.Modular.Performance". The times leave out
-- making the package database for a scenario.
--
-- > solver-perf [--runs N] [SCENARIO [--SIZE N[,N...] ...]]
--
-- Without a scenario, it runs them all with their default sizes. A size can
-- take several values, to compare the timings between them. With @--log@ it
-- prints the solver's log for each scenario and doesn't time it. See @--help@ for
-- the scenarios, and @SCENARIO --help@ for the sizes of one.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad ((<=<), forM_, replicateM, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (intercalate, isInfixOf, isPrefixOf, sort)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit (die)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Mem (getAllocationCounter, performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Options.Applicative

import Distribution.Client.Dependency (foldProgress)

import UnitTests.Distribution.Solver.Modular.DSL (ExampleIndexes, extractInstallPlan, forceExIndexes)
import UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils
import UnitTests.Distribution.Solver.Modular.Performance

-- | The number of timed runs of each scenario, whether to print the solver's
-- log in place of timing it, and the scenario to run, if one was named, with
-- the values to try for each of its sizes.
data Options = Options Int Bool (Maybe (Scenario, [[Int]]))

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "runs"
          <> metavar "N"
          <> value 5
          <> showDefault
          <> help "Timed runs of each scenario"
      )
    <*> switch
      ( long "log"
          <> help "Print the solver's log and its error for each scenario, without timing it"
      )
    <*> optional (hsubparser (foldMap scenarioCommand scenarios <> metavar "SCENARIO"))
  where
    scenarioCommand :: Scenario -> Mod CommandFields (Scenario, [[Int]])
    scenarioCommand s =
      command (scenarioName s) $
        info
          ((,) s <$> traverse sizeOption (scenarioSizes s))
          ( progDesc ("Solve a problem where the solver has to " ++ scenarioSummary s)
              <> footer
                "A size can take several values, separated by commas. The \
                \scenario then runs with every combination of the values."
          )

    sizeOption :: Size -> Parser [Int]
    sizeOption size =
      option
        (eitherReader readSizes)
        ( long (sizeName size)
            <> metavar "N[,N...]"
            <> value [sizeDefault size]
            <> showDefaultWith (intercalate "," . map show)
            <> help ("Number of " ++ sizeSummary size)
        )

    readSizes :: String -> Either String [Int]
    readSizes arg =
      case traverse readMaybe (splitOn ',' arg) of
        Just xs@(_ : _) | all (>= 0) xs -> Right xs
        _ -> Left $ "cannot parse `" ++ arg ++ "' as numbers separated by commas"

    splitOn :: Char -> String -> [String]
    splitOn c xs = case break (== c) xs of
      (x, []) -> [x]
      (x, _ : rest) -> x : splitOn c rest

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <-
    -- Inline the scenario's options, so that --runs can come anywhere.
    customExecParser (prefs subparserInline) $
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc
              "Time the dependency solver on problems that stress particular \
              \parts of it. Without a scenario, run them all with their default \
              \sizes."
        )
  dispatch opts

dispatch :: Options -> IO ()
dispatch (Options _ True chosen) =
  forM_ selected $ \(s, sizes) -> do
    let test = scenarioTest s sizes
    putStrLn $ title s sizes
    -- Leave out the solver's settings, which come before the log of its search.
    mapM_ putStrLn . dropWhile (not . isPrefixOf "[") $
      foldProgress (:) (const []) (const []) (solverTestProgress $ setVerbose test)
    putStrLn $ foldProgress (\_ r -> r) id (const "Solved") (solverTestProgress test)
  where
    selected :: [(Scenario, [Int])]
    selected = case chosen of
      Nothing -> [(s, map sizeDefault $ scenarioSizes s) | s <- scenarios]
      Just (s, values) -> [(s, sizes) | sizes <- sequence values]
dispatch (Options runs False chosen) =
  case chosen of
    Nothing -> forM_ scenarios $ \s -> report s =<< measure runs s (map sizeDefault $ scenarioSizes s)
    Just (s, values) ->
      case sequence values of
        [sizes] -> report s =<< measure runs s sizes
        combinations -> do
          tableHeader s
          forM_ combinations $ \sizes -> tableRow =<< measure runs s sizes

-- | What a solver run produced, in a form that can be compared between runs.
data Outcome = Failed Int | Solved Int
  deriving (Eq, Show)

-- | The timings of a scenario at one set of sizes.
data Measurement = Measurement
  { mSizes :: [Int]
  , mLogLines :: Int
  , mSkips :: Int
  , mBackjumps :: Int
  , mOutcome :: Outcome
  , mRuns :: Int
  , mMinMs :: Double
  , mMedianMs :: Double
  , mAllocatedMb :: Double
  }

measure :: Int -> Scenario -> [Int] -> IO Measurement
measure runs s sizes = do
  let test = scenarioTest s sizes

  -- An untimed run with the full log, to show which path the solver took.
  let logLines = foldProgress (:) (const []) (const []) (solverTestProgress $ setVerbose test)
      count p = length (filter p logLines)

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

  let times = sort [t | (_, t, _) <- results]
      allocs = sort [a | (_, _, a) <- results]
      median xs = xs !! (length xs `div` 2)
  case [o | (o, _, _) <- results] of
    [] -> die "--runs must be at least 1"
    o : os -> do
      unless (all (== o) os) $ die "the runs gave different results"
      return
        Measurement
          { mSizes = sizes
          , mLogLines = length logLines
          , mSkips = count ("skipping:" `isInfixOf`)
          , mBackjumps = count ("Backjumping" `isInfixOf`) + count ("backjumping" `isInfixOf`)
          , mOutcome = o
          , mRuns = runs
          , mMinMs = ms (minimum times)
          , mMedianMs = ms (median times)
          , mAllocatedMb = fromIntegral (median allocs) / (1024 * 1024)
          }
  where
    ms :: Word64 -> Double
    ms t = fromIntegral t / 1e6

-- | Prints a measurement in full.
report :: Scenario -> Measurement -> IO ()
report s m = do
  putStrLn $ title s (mSizes m)
  printf "  log: %d lines, %d skipping, %d backjumps\n" (mLogLines m) (mSkips m) (mBackjumps m)
  printf "  result: %s\n" (show $ mOutcome m)
  printf
    "  time: min %.1f ms, median %.1f ms; allocated %.1f MB (%d runs)\n"
    (mMinMs m)
    (mMedianMs m)
    (mAllocatedMb m)
    (mRuns m)

-- | The name of a scenario with its sizes.
title :: Scenario -> [Int] -> String
title s sizes =
  printf
    "%s (%s)"
    (scenarioName s)
    (intercalate ", " [sizeName size ++ " = " ++ show x | (size, x) <- zip (scenarioSizes s) sizes])

-- | Prints the heading for the rows of 'tableRow'.
tableHeader :: Scenario -> IO ()
tableHeader s = do
  putStrLn (scenarioName s)
  putStrLn $
    concatMap (printf "  %-12s" . sizeName) (scenarioSizes s)
      ++ printf "  %-12s  %10s  %10s  %12s" "result" "min ms" "median ms" "allocated MB"

-- | Prints a measurement as one line, for comparing sizes.
tableRow :: Measurement -> IO ()
tableRow m =
  putStrLn $
    concatMap (printf "  %-12d") (mSizes m)
      ++ printf
        "  %-12s  %10.1f  %10.1f  %12.1f"
        (show $ mOutcome m)
        (mMinMs m)
        (mMedianMs m)
        (mAllocatedMb m)

-- | Runs the solver to the end. A failure is summarised by the length of its
-- error message and a success by the size of its plan, which forces both.
solve :: ExampleIndexes -> SolverTest -> Outcome
solve idxs =
  foldProgress
    (\_ r -> r)
    (\err -> let n = length err in n `seq` Failed n)
    (\plan -> let n = length (extractInstallPlan plan) in n `seq` Solved n)
    . solverTestProgressWith idxs
