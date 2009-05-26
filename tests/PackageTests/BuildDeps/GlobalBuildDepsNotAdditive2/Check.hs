module PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath
import Data.List


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "GlobalBuildDepsNotAdditive2") []
    result <- cabal_build spec
    assertEqual "cabal build should fail - see test-log.txt" False (successful result)
    assertBool "cabal error should be \"Failed to load interface for `Prelude'\"" $
        "Failed to load interface for `Prelude'" `isInfixOf` outputText result
