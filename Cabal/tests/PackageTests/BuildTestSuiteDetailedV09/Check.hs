module PackageTests.BuildTestSuiteDetailedV09.Check where

import Test.Tasty.HUnit
import System.FilePath ((</>))

import PackageTests.PackageTester

suite :: PackageSpec -> FilePath -> Assertion
suite inplaceSpec ghcPath = do
    let dir = "PackageTests" </> "BuildTestSuiteDetailedV09"
        spec = inplaceSpec
            { directory = dir
            , configOpts = "--enable-tests" : configOpts inplaceSpec
            }
    confResult <- cabal_configure spec ghcPath
    assertConfigureSucceeded confResult
    buildResult <- cabal_build spec ghcPath
    assertBuildSucceeded buildResult
