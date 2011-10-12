module PackageTests.BenchmarkExeV10.Check
       ( checkBenchmark
       ) where

import Distribution.PackageDescription ( Benchmark(..), emptyBenchmark )
import Distribution.Simple.Hpc
import Distribution.Version
import Test.HUnit
import System.Directory
import System.FilePath
import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "BenchmarkExeV10"

checkBenchmark :: Version -> Test
checkBenchmark cabalVersion = TestCase $ do
    let spec = PackageSpec dir ["--enable-benchmarks"]
    buildResult <- cabal_build spec
    let buildMessage = "\'setup build\' should succeed"
    assertEqual buildMessage True $ successful buildResult
