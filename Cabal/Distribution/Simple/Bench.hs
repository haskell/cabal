-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Bench
-- Copyright   :  Johan Tibell 2011
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into running the benchmarks in a built
-- package. It performs the \"@.\/setup bench@\" action. It runs
-- benchmarks designated in the package description.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Bench
    ( bench
    ) where

import qualified Distribution.PackageDescription as PD
    ( PackageDescription(..), BuildInfo(buildable)
    , Benchmark(..), BenchmarkInterface(..), benchmarkType, hasBenchmarks )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( Compiler(..) )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import qualified Distribution.Simple.LocalBuildInfo as LBI
    ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( BenchmarkFlags(..), fromFlag )
import Distribution.Simple.UserHooks ( Args )
import Distribution.Simple.Utils ( die, notice, rawSystemExitCode )
import Distribution.Text

import Control.Monad ( when, unless )
import System.Exit ( ExitCode(..), exitFailure, exitWith )
import System.Directory ( doesFileExist )
import System.FilePath ( (</>), (<.>) )

-- | Perform the \"@.\/setup bench@\" action.
bench :: Args                    -- ^positional command-line arguments
      -> PD.PackageDescription   -- ^information from the .cabal file
      -> LBI.LocalBuildInfo      -- ^information from the configure step
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

    when (not $ PD.hasBenchmarks pkg_descr) $ do
        notice verbosity "Package has no benchmarks."
        exitWith ExitSuccess

    when (PD.hasBenchmarks pkg_descr && null enabledBenchmarks) $
        die $ "No benchmarks enabled. Did you remember to configure with "
              ++ "\'--enable-benchmarks\'?"

    bmsToRun <- case benchmarkNames of
            [] -> return enabledBenchmarks
            names -> flip mapM names $ \bmName ->
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


-- TODO: This is abusing the notion of a 'PathTemplate'.  The result
-- isn't neccesarily a path.
benchOption :: PD.PackageDescription
            -> LBI.LocalBuildInfo
            -> PD.Benchmark
            -> PathTemplate
            -> String
benchOption pkg_descr lbi bm template =
    fromPathTemplate $ substPathTemplate env template
  where
    env = initialPathTemplateEnv
          (PD.package pkg_descr) (compilerId $ LBI.compiler lbi) ++
          [(BenchmarkNameVar, toPathTemplate $ PD.benchmarkName bm)]
