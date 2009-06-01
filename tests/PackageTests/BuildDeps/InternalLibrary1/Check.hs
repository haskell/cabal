module PackageTests.BuildDeps.InternalLibrary1.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary1") []
    result <- cabal_build spec
    assertEqual "cabal build should succeed - see test-log.txt" True (successful result)
