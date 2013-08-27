module PackageTests.BenchmarkExeV10.Check
       ( checkBenchmark
       ) where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit

dir :: FilePath
dir = "PackageTests" </> "BenchmarkExeV10"

checkBenchmark :: FilePath -> Test
checkBenchmark ghcPath = TestCase $ do
    let spec = PackageSpec dir ["--enable-benchmarks"]
    buildResult <- cabal_build spec ghcPath
    assertBuildSucceeded buildResult
