module PackageTests.BenchmarkOptions.Check where

import Test.HUnit
import System.FilePath
import PackageTests.PackageTester

suite :: Test
suite = TestCase $ do
    let directory = "PackageTests" </> "BenchmarkOptions"
        pdFile = directory </> "BenchmarkOptions" <.> "cabal"
        spec = PackageSpec directory ["--enable-benchmarks"]
    _ <- cabal_build spec
    result <- cabal_bench spec ["--benchmark-options=1 2 3"]
    let message = "\"cabal bench\" did not pass the correct options to the "
                  ++ "benchmark executable with \"--benchmark-options\""
    assertEqual message True $ successful result
    result' <- cabal_bench spec [ "--benchmark-option=1"
                                , "--benchmark-option=2"
                                , "--benchmark-option=3"
                                ]
    let message = "\"cabal bench\" did not pass the correct options to the "
                  ++ "benchmark executable with \"--benchmark-option\""
    assertEqual message True $ successful result'
