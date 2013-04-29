module PackageTests.BuildTestSuiteDetailedV09.Check where

import Test.HUnit
import System.FilePath ((</>))

import PackageTests.PackageTester

suite :: PackageSpec -> Test
suite inplaceSpec = TestCase $ do
    let dir = "PackageTests" </> "BuildTestSuiteDetailedV09"
        spec = inplaceSpec
            { directory = dir
            , configOpts = "--enable-tests" : configOpts inplaceSpec
            }
    confResult <- cabal_configure spec
    assertEqual "configure failed!" (successful confResult) True
    buildResult <- cabal_build spec
    assertEqual "build failed!" (successful buildResult) True
