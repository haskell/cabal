module PackageTests.BuildTestSuiteDetailedV09.Check where

import Test.HUnit
import System.FilePath ((</>))

import PackageTests.PackageTester

suite :: PackageSpec -> FilePath -> Test
suite inplaceSpec ghcPath = TestCase $ do
    let dir = "PackageTests" </> "BuildTestSuiteDetailedV09"
        spec = inplaceSpec
            { directory = dir
            , configOpts = "--enable-tests" : configOpts inplaceSpec
            }
    confResult <- cabal_configure spec ghcPath
    assertConfigureSucceeded confResult
    buildResult <- cabal_build spec ghcPath
    assertBuildSucceeded buildResult
