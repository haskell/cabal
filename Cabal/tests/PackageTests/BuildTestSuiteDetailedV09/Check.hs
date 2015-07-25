module PackageTests.BuildTestSuiteDetailedV09.Check where

import Test.Tasty.HUnit
import System.FilePath ((</>))

import PackageTests.PackageTester

suite :: SuiteConfig -> Assertion
suite config = do
    let dir = "PackageTests" </> "BuildTestSuiteDetailedV09"
        spec = (inplaceSpec config)
            { directory = dir
            , configOpts = "--enable-tests" : configOpts (inplaceSpec config)
            }
    confResult <- cabal_configure config spec
    assertConfigureSucceeded confResult
    buildResult <- cabal_build config spec
    assertBuildSucceeded buildResult
    testResult <- cabal_test config spec [] ["test-Dummy", "test-Dummy2"]
    assertTestSucceeded testResult
