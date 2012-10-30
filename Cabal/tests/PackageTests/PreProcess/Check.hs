module PackageTests.PreProcess.Check (suite) where

import PackageTests.PackageTester
    (PackageSpec(..), assertBuildSucceeded, cabal_build)
import System.FilePath
import Test.HUnit

suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "PreProcess")
               ["--enable-tests", "--enable-benchmarks"]
    result <- cabal_build spec
    assertBuildSucceeded result
