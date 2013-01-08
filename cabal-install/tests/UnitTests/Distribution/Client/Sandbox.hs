module UnitTests.Distribution.Client.Sandbox (
  tests
  ) where

import Distribution.Client.Sandbox    (withSandboxBinDirOnSearchPath)

import Test.Framework                 as TF (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (Assertion, assertBool, assertEqual)

import System.FilePath                (getSearchPath, (</>))

tests :: [TF.Test]
tests = [ testCase "sandboxBinDirOnSearchPath" sandboxBinDirOnSearchPathTest
        , testCase "oldSearchPathRestored" oldSearchPathRestoreTest
        ]

sandboxBinDirOnSearchPathTest :: Assertion
sandboxBinDirOnSearchPathTest =
  withSandboxBinDirOnSearchPath "foo" $ do
    r <- getSearchPath
    assertBool "'foo/bin' not on search path" $ ("foo" </> "bin") `elem` r

oldSearchPathRestoreTest :: Assertion
oldSearchPathRestoreTest = do
  r <- getSearchPath
  withSandboxBinDirOnSearchPath "foo" $ return ()
  r' <- getSearchPath
  assertEqual "Old search path wasn't restored" r r'
