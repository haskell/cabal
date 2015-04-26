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

module Distribution.Simple.Bench (action, bench) where

import qualified Distribution.PackageDescription as PD
    ( PackageDescription(..), BuildInfo(buildable)
    , Benchmark(..), BenchmarkInterface(..), benchmarkType, hasBenchmarks )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( compilerInfo )
import Distribution.Simple.Configure ( findDistPrefOrDefault )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Reconfigure (Reconfigure)
import Distribution.Simple.Setup
    ( BenchmarkFlags(..), BuildFlags(..), ConfigFlags(..)
    , fromFlag, fromFlagOrDefault, toFlag )
import Distribution.Simple.UserHooks ( Args )
import Distribution.Simple.Utils ( die, notice, rawSystemExitCode )
import Distribution.Text

import Control.Monad ( forM, unless, when )
import Data.Maybe (mapMaybe)
import System.Exit ( ExitCode(..), exitFailure, exitSuccess, exitWith )
import System.Directory ( doesFileExist )
import System.FilePath ( (</>), (<.>) )

action :: Reconfigure
       -> (BuildFlags -> Args -> IO ())
       -> (LocalBuildInfo -> BenchmarkFlags -> Args -> IO ())
       -> BenchmarkFlags -> Args -> IO ()
action reconfigure buildAction benchAction flags args = do
    distPref <- findDistPrefOrDefault (benchmarkDistPref flags)
    let flags' = flags { benchmarkDistPref = toFlag distPref }
        verbosity = fromFlag (benchmarkVerbosity flags')

        withEnabledBenchs fs
          | fromFlagOrDefault False (configBenchmarks fs) = Nothing
          | otherwise = Just (enableBenchs fs, "enabling benchmarks")
        enableBenchs fs = fs { configBenchmarks = toFlag True }

    lbi <- reconfigure [withEnabledBenchs] verbosity distPref

    let buildFlags =
            mempty
            { buildDistPref = benchmarkDistPref flags'
            , buildVerbosity = benchmarkVerbosity flags'
            }
        buildArgs_
          | null args = benchs
          | otherwise = args
        benchs = mapMaybe nameBenchsOnly $ LBI.pkgComponents pkgDescr
        pkgDescr = LBI.localPkgDescr lbi
        nameBenchsOnly =
          LBI.foldComponent
            (const Nothing)
            (const Nothing)
            (const Nothing)
            (\b ->
              if PD.buildable (PD.benchmarkBuildInfo b)
                then Just (PD.benchmarkName b)
              else Nothing)

    if null benchs
      then notice verbosity "benchmark: no buildable benchmarks"
      else do
        -- Ensure that all requested benchmarks are built.
        buildAction buildFlags buildArgs_

        benchAction lbi flags' args

-- | Perform the \"@.\/setup bench@\" action.
bench :: Args                    -- ^positional command-line arguments
      -> PD.PackageDescription   -- ^information from the .cabal file
      -> LocalBuildInfo          -- ^information from the configure step
      -> BenchmarkFlags          -- ^flags sent to benchmark
      -> IO ()
bench args pkg_descr lbi flags = do
    let verbosity         = fromFlag $ benchmarkVerbosity flags
        benchmarkNames    = args
        pkgBenchmarks     = PD.benchmarks pkg_descr
        enabledBenchmarks = [ t | t <- pkgBenchmarks
                            , PD.benchmarkEnabled t
                            , PD.buildable (PD.benchmarkBuildInfo t) ]

        -- Run the benchmark
        doBench :: PD.Benchmark -> IO ExitCode
        doBench bm =
            case PD.benchmarkInterface bm of
              PD.BenchmarkExeV10 _ _ -> do
                  let cmd = LBI.buildDir lbi </> PD.benchmarkName bm
                            </> PD.benchmarkName bm <.> exeExtension
                      options = map (benchOption pkg_descr lbi bm) $
                                benchmarkOptions flags
                      name = PD.benchmarkName bm
                  -- Check that the benchmark executable exists.
                  exists <- doesFileExist cmd
                  unless exists $ die $
                      "Error: Could not find benchmark program \""
                      ++ cmd ++ "\". Did you build the package first?"

                  notice verbosity $ startMessage name
                  -- This will redirect the child process
                  -- stdout/stderr to the parent process.
                  exitcode <- rawSystemExitCode verbosity cmd options
                  notice verbosity $ finishMessage name exitcode
                  return exitcode

              _ -> do
                  notice verbosity $ "No support for running "
                      ++ "benchmark " ++ PD.benchmarkName bm ++ " of type: "
                      ++ show (disp $ PD.benchmarkType bm)
                  exitFailure

    unless (PD.hasBenchmarks pkg_descr) $ do
        notice verbosity "Package has no benchmarks."
        exitSuccess

    when (PD.hasBenchmarks pkg_descr && null enabledBenchmarks) $
        die $ "No benchmarks enabled. Did you remember to configure with "
              ++ "\'--enable-benchmarks\'?"

    bmsToRun <- case benchmarkNames of
            [] -> return enabledBenchmarks
            names -> forM names $ \bmName ->
                let benchmarkMap = zip enabledNames enabledBenchmarks
                    enabledNames = map PD.benchmarkName enabledBenchmarks
                    allNames = map PD.benchmarkName pkgBenchmarks
                in case lookup bmName benchmarkMap of
                    Just t -> return t
                    _ | bmName `elem` allNames ->
                          die $ "Package configured with benchmark "
                                ++ bmName ++ " disabled."
                      | otherwise -> die $ "no such benchmark: " ++ bmName

    let totalBenchmarks = length bmsToRun
    notice verbosity $ "Running " ++ show totalBenchmarks ++ " benchmarks..."
    exitcodes <- mapM doBench bmsToRun
    let allOk = totalBenchmarks == length (filter (== ExitSuccess) exitcodes)
    unless allOk exitFailure
  where
    startMessage name = "Benchmark " ++ name ++ ": RUNNING...\n"
    finishMessage name exitcode = "Benchmark " ++ name ++ ": "
                               ++ (case exitcode of
                                        ExitSuccess -> "FINISH"
                                        ExitFailure _ -> "ERROR")


-- TODO: This is abusing the notion of a 'PathTemplate'.  The result isn't
-- necessarily a path.
benchOption :: PD.PackageDescription
            -> LBI.LocalBuildInfo
            -> PD.Benchmark
            -> PathTemplate
            -> String
benchOption pkg_descr lbi bm template =
    fromPathTemplate $ substPathTemplate env template
  where
    env = initialPathTemplateEnv
          (PD.package pkg_descr) (LBI.localComponentId lbi)
          (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi) ++
          [(BenchmarkNameVar, toPathTemplate $ PD.benchmarkName bm)]
