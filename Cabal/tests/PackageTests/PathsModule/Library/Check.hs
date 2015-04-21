module PackageTests.PathsModule.Library.Check (suite) where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

suite :: IO TestsConfig -> Assertion
suite cfg = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "PathsModule" </> "Library"
            , distPref = Nothing
            , configOpts = []
            }
    result <- cabal_build cfg spec
    assertBuildSucceeded result
