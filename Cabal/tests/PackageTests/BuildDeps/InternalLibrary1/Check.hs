module PackageTests.BuildDeps.InternalLibrary1.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath
import Control.Exception
import Prelude hiding (catch)


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary1") []
    result <- cabal_build spec
    assertBuildSucceeded result
