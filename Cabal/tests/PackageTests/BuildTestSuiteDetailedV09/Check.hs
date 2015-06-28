{-# LANGUAGE RecordWildCards #-}
module PackageTests.BuildTestSuiteDetailedV09.Check where

import Test.Tasty.HUnit
import System.FilePath ((</>))

import PackageTests.PackageTester

suite :: IO TestsConfig -> Assertion
suite cfg = do
    TestsConfig{..} <- cfg
    let dir = "PackageTests" </> "BuildTestSuiteDetailedV09"
        spec = testsConfigInPlaceSpec
            { directory = dir
            , configOpts = "--enable-tests" : configOpts testsConfigInPlaceSpec
            }
    confResult <- cabal_configure cfg spec
    assertConfigureSucceeded confResult
    buildResult <- cabal_build cfg spec
    assertBuildSucceeded buildResult
