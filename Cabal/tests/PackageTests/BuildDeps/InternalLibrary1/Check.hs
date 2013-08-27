module PackageTests.BuildDeps.InternalLibrary1.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit


suite :: FilePath -> Test
suite ghcPath = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary1") []
    result <- cabal_build spec ghcPath
    assertBuildSucceeded result
