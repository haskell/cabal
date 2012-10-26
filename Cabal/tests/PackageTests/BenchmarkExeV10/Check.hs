module PackageTests.BenchmarkExeV10.Check
       ( checkBenchmark
       ) where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit

dir :: FilePath
dir = "PackageTests" </> "BenchmarkExeV10"

checkBenchmark :: Test
checkBenchmark = TestCase $ do
    let spec = PackageSpec dir ["--enable-benchmarks"]
    buildResult <- cabal_build spec
    assertBuildSucceeded buildResult
