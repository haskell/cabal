module PackageTests.PreProcessExtraSources.Check (suite) where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

suite :: IO TestsConfig -> Assertion
suite cfg = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "PreProcessExtraSources"
            , distPref = Nothing
            , configOpts = ["--enable-tests", "--enable-benchmarks"]
            }
    result <- cabal_build cfg spec
    assertBuildSucceeded result
