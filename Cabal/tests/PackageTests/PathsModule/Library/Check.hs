module PackageTests.PathsModule.Library.Check (suite) where

import PackageTests.PackageTester
    (PackageSpec(..), SuiteConfig, assertBuildSucceeded, cabal_build)
import System.FilePath
import Test.Tasty.HUnit

suite :: SuiteConfig -> Assertion
suite config = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "PathsModule" </> "Library"
            , distPref = Nothing
            , configOpts = []
            }
    result <- cabal_build config spec
    assertBuildSucceeded result
