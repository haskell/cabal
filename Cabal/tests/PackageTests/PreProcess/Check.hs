module PackageTests.PreProcess.Check (suite) where

import PackageTests.PackageTester
    (PackageSpec(..), SuiteConfig, assertBuildSucceeded, cabal_build)
import System.FilePath
import Test.Tasty.HUnit

suite :: SuiteConfig -> Assertion
suite config = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "PreProcess"
            , distPref = Nothing
            , configOpts = ["--enable-tests", "--enable-benchmarks"]
            }
    result <- cabal_build config spec
    assertBuildSucceeded result
