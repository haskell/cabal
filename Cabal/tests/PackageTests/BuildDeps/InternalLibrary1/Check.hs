module PackageTests.BuildDeps.InternalLibrary1.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit


suite :: SuiteConfig -> Assertion
suite config = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BuildDeps" </> "InternalLibrary1"
            , configOpts = []
            , distPref = Nothing
            }
    result <- cabal_build config spec
    assertBuildSucceeded result
