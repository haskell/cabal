module PackageTests.BuildDeps.InternalLibrary1.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit


suite :: IO TestsConfig -> Assertion
suite cfg = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BuildDeps" </> "InternalLibrary1"
            , configOpts = []
            , distPref = Nothing
            }
    result <- cabal_build cfg spec
    assertBuildSucceeded result
