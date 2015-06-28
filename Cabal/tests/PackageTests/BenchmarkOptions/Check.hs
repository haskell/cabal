module PackageTests.BenchmarkOptions.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

suite :: IO TestsConfig -> Assertion
suite cfg = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BenchmarkOptions"
            , configOpts = ["--enable-benchmarks"]
            , distPref = Nothing
            }
    _ <- cabal_build cfg spec
    result <- cabal_bench cfg spec ["--benchmark-options=1 2 3"]
    let message = "\"cabal bench\" did not pass the correct options to the "
                  ++ "benchmark executable with \"--benchmark-options\""
    assertEqual message True $ successful result
    result' <- cabal_bench cfg spec [ "--benchmark-option=1"
                                    , "--benchmark-option=2"
                                    , "--benchmark-option=3"
                                    ]
    let message' = "\"cabal bench\" did not pass the correct options to the "
                   ++ "benchmark executable with \"--benchmark-option\""
    assertEqual message' True $ successful result'
