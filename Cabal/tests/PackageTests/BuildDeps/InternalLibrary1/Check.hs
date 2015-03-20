module PackageTests.BuildDeps.InternalLibrary1.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit


suite :: FilePath -> Assertion
suite ghcPath = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BuildDeps" </> "InternalLibrary1"
            , configOpts = []
            , distPref = Nothing
            }
    result <- cabal_build spec ghcPath
    assertBuildSucceeded result
