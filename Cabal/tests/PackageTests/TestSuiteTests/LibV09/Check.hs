module PackageTests.TestSuiteTests.LibV09.Check (checks) where

import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath ((</>))

import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "TestSuiteTests" </> "LibV09"

checks :: SuiteConfig -> [TestTree]
checks config =
  [ testCase "Build" (checkBuild config)
  , localOption (mkTimeout $ 10 ^ (8 :: Int))
    $ testCase "Deadlock" (checkDeadlock config)
  ]

checkBuild :: SuiteConfig -> Assertion
checkBuild config = do
  let spec = (inplaceSpec config)
             { directory = dir
             , distPref = Just $ "dist-Build"
             , configOpts = "--enable-tests"
                            : configOpts (inplaceSpec config)
             }
  buildResult <- cabal_build config spec
  assertBuildSucceeded buildResult

checkDeadlock :: SuiteConfig -> Assertion
checkDeadlock config = do
  let spec = (inplaceSpec config)
             { directory = dir
             , distPref = Just $ "dist-Test"
             , configOpts = "--enable-tests"
                            : configOpts (inplaceSpec config)
             }
  buildResult <- cabal_build config spec
  assertBuildSucceeded buildResult
  testResult <- cabal_test config spec [] []
  assertTestFailed testResult
