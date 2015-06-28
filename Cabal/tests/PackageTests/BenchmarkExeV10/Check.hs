module PackageTests.BenchmarkExeV10.Check
       ( checkBenchmark
       ) where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

dir :: FilePath
dir = "PackageTests" </> "BenchmarkExeV10"

checkBenchmark :: IO TestsConfig -> Assertion
checkBenchmark cfg = do
    let spec = PackageSpec dir Nothing ["--enable-benchmarks"]
    buildResult <- cabal_build cfg spec
    assertBuildSucceeded buildResult
