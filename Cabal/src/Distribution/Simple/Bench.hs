{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Bench
-- Copyright   :  Johan Tibell 2011
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into running the benchmarks in a built
-- package. It performs the \"@.\/setup bench@\" action. It runs
-- benchmarks designated in the package description.
module Distribution.Simple.Bench
  ( bench
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup.Benchmark
import Distribution.Simple.Setup.Common
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils
import Distribution.Utils.Path

import Distribution.Types.UnqualComponentName

import Distribution.Simple.Errors
import System.Directory (doesFileExist)

-- | Perform the \"@.\/setup bench@\" action.
bench
  :: Args
  -- ^ positional command-line arguments
  -> PD.PackageDescription
  -- ^ information from the .cabal file
  -> LBI.LocalBuildInfo
  -- ^ information from the configure step
  -> BenchmarkFlags
  -- ^ flags sent to benchmark
  -> IO ()
bench args pkg_descr lbi flags = do
  let verbosity = fromFlag $ benchmarkVerbosity flags
      benchmarkNames = args
      pkgBenchmarks = PD.benchmarks pkg_descr
      enabledBenchmarks = map fst (LBI.enabledBenchLBIs pkg_descr lbi)
      mbWorkDir = flagToMaybe $ benchmarkWorkingDir flags
      i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path

      -- Run the benchmark
      doBench :: PD.Benchmark -> IO ExitCode
      doBench bm =
        case PD.benchmarkInterface bm of
          PD.BenchmarkExeV10 _ _ -> do
            let cmd = i $ LBI.buildDir lbi </> makeRelativePathEx (name </> name <.> exeExtension (LBI.hostPlatform lbi))
                options =
                  map (benchOption pkg_descr lbi bm) $
                    benchmarkOptions flags
            -- Check that the benchmark executable exists.
            exists <- doesFileExist cmd
            unless exists $
              dieWithException verbosity $
                NoBenchMarkProgram cmd

            notice verbosity $ startMessage name
            -- This will redirect the child process
            -- stdout/stderr to the parent process.
            exitcode <- rawSystemExitCode verbosity mbWorkDir cmd options
            notice verbosity $ finishMessage name exitcode
            return exitcode
          _ -> do
            notice verbosity $
              "No support for running "
                ++ "benchmark "
                ++ name
                ++ " of type: "
                ++ prettyShow (PD.benchmarkType bm)
            exitFailure
        where
          name = unUnqualComponentName $ PD.benchmarkName bm

  unless (PD.hasBenchmarks pkg_descr) $ do
    notice verbosity "Package has no benchmarks."
    exitSuccess

  when (PD.hasBenchmarks pkg_descr && null enabledBenchmarks) $
    dieWithException verbosity EnableBenchMark

  bmsToRun <- case benchmarkNames of
    [] -> return enabledBenchmarks
    names -> for names $ \bmName ->
      let benchmarkMap = zip enabledNames enabledBenchmarks
          enabledNames = map PD.benchmarkName enabledBenchmarks
          allNames = map PD.benchmarkName pkgBenchmarks
       in case lookup (mkUnqualComponentName bmName) benchmarkMap of
            Just t -> return t
            _
              | mkUnqualComponentName bmName `elem` allNames ->
                  dieWithException verbosity $ BenchMarkNameDisabled bmName
              | otherwise -> dieWithException verbosity $ NoBenchMark bmName

  let totalBenchmarks = length bmsToRun
  notice verbosity $ "Running " ++ show totalBenchmarks ++ " benchmarks..."
  exitcodes <- traverse doBench bmsToRun
  let allOk = totalBenchmarks == length (filter (== ExitSuccess) exitcodes)
  unless allOk exitFailure
  where
    startMessage name = "Benchmark " ++ name ++ ": RUNNING...\n"
    finishMessage name exitcode =
      "Benchmark "
        ++ name
        ++ ": "
        ++ ( case exitcode of
              ExitSuccess -> "FINISH"
              ExitFailure _ -> "ERROR"
           )

-- TODO: This is abusing the notion of a 'PathTemplate'.  The result isn't
-- necessarily a path.
benchOption
  :: PD.PackageDescription
  -> LBI.LocalBuildInfo
  -> PD.Benchmark
  -> PathTemplate
  -> String
benchOption pkg_descr lbi bm template =
  fromPathTemplate $ substPathTemplate env template
  where
    env =
      initialPathTemplateEnv
        (PD.package pkg_descr)
        (LBI.localUnitId lbi)
        (compilerInfo $ LBI.compiler lbi)
        (LBI.hostPlatform lbi)
        ++ [(BenchmarkNameVar, toPathTemplate $ unUnqualComponentName $ PD.benchmarkName bm)]
