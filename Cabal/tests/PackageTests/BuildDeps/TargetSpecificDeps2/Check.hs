module PackageTests.BuildDeps.TargetSpecificDeps2.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath
import Data.List


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "TargetSpecificDeps2") []
    result <- cabal_build spec
    assertEqual "cabal build should succeed - see test-log.txt" True (successful result)
