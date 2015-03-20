module PackageTests.PathsModule.Library.Check (suite) where

import PackageTests.PackageTester
    (PackageSpec(..), assertBuildSucceeded, cabal_build)
import System.FilePath
import Test.Tasty.HUnit

suite :: FilePath -> Assertion
suite ghcPath = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "PathsModule" </> "Library"
            , distPref = Nothing
            , configOpts = []
            }
    result <- cabal_build spec ghcPath
    assertBuildSucceeded result
