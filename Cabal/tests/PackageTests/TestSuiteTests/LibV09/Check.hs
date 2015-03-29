module PackageTests.TestSuiteTests.LibV09.Check (checks) where

import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath ((</>))

import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "TestSuiteTests" </> "LibV09"

checks :: PackageSpec -> FilePath -> [TestTree]
checks inplaceSpec ghcPath =
  [ testCase "Build" $ checkBuild inplaceSpec ghcPath
  , localOption (mkTimeout $ 10 * 10 ^ (6 :: Int))
    $ testCase "Deadlock" $ checkDeadlock inplaceSpec ghcPath
  ]

checkBuild :: PackageSpec -> FilePath -> Assertion
checkBuild inplaceSpec ghcPath = do
  let spec = inplaceSpec
             { directory = dir
             , distPref = Just $ "dist-Build"
             , configOpts = "--enable-tests" : configOpts inplaceSpec
             }
  buildResult <- cabal_build spec ghcPath
  assertBuildSucceeded buildResult

checkDeadlock :: PackageSpec -> FilePath -> Assertion
checkDeadlock inplaceSpec ghcPath = do
  let spec = inplaceSpec
             { directory = dir
             , distPref = Just $ "dist-Test"
             , configOpts = "--enable-tests" : configOpts inplaceSpec
             }
  buildResult <- cabal_build spec ghcPath
  assertBuildSucceeded buildResult
  testResult <- cabal_test spec [] [] ghcPath
  assertTestFailed testResult
