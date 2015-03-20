module PackageTests.BenchmarkExeV10.Check
       ( checkBenchmark
       ) where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

dir :: FilePath
dir = "PackageTests" </> "BenchmarkExeV10"

checkBenchmark :: FilePath -> Assertion
checkBenchmark ghcPath = do
    let spec = PackageSpec dir Nothing ["--enable-benchmarks"]
    buildResult <- cabal_build spec ghcPath
    assertBuildSucceeded buildResult
