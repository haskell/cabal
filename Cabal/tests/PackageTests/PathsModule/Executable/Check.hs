module PackageTests.PathsModule.Executable.Check (suite) where

import Distribution.Version
import PackageTests.PackageTester (PackageSpec(..), assertBuildSucceeded,
                                   cabal_build)
import System.FilePath
import Test.HUnit

suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "PathsModule" </> "Executable")
               []
    result <- cabal_build spec
    assertBuildSucceeded result
