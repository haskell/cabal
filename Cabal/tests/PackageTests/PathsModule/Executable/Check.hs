module PackageTests.PathsModule.Executable.Check (suite) where

import PackageTests.PackageTester
    (PackageSpec(..), assertBuildSucceeded, cabal_build)
import System.FilePath
import Test.HUnit

suite :: FilePath -> Test
suite ghcPath = TestCase $ do
    let spec = PackageSpec
            { directory = "PackageTests" </> "PathsModule" </> "Executable"
            , distPref = Nothing
            , configOpts = []
            }
    result <- cabal_build spec ghcPath
    assertBuildSucceeded result
