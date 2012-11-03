module PackageTests.BuildDeps.InternalLibrary1.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary1") []
    result <- cabal_build spec
    assertBuildSucceeded result
