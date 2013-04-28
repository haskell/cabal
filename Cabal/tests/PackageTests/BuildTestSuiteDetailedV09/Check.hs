module PackageTests.BuildTestSuiteDetailedV09.Check where

import Test.HUnit
import System.FilePath ((</>))

import PackageTests.PackageTester

suite :: Test
suite = TestCase $ do
    let dir = "PackageTests" </> "BuildTestSuiteDetailedV09"
        spec = PackageSpec dir ["--enable-tests"]
    confResult <- cabal_configure spec
    assertEqual "configure failed!" (successful confResult) True
    buildResult <- cabal_build spec
    assertEqual "build failed!" (successful buildResult) True
