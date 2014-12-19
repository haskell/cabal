module PackageTests.BuildDeps.InternalLibrary1.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit


suite :: FilePath -> Test
suite ghcPath = TestCase $ do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BuildDeps" </> "InternalLibrary1"
            , configOpts = []
            , distPref = Nothing
            }
    result <- cabal_build spec ghcPath
    assertBuildSucceeded result
