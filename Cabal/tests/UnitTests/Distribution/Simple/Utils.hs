module UnitTests.Distribution.Simple.Utils
    ( tests
    ) where

import Distribution.Simple.Utils
import Distribution.Verbosity

import System.Directory (getTemporaryDirectory, removeDirectoryRecursive
                        ,removeFile)
import System.IO (hClose)

import Test.Tasty
import Test.Tasty.HUnit

withTempFileTest :: Assertion
withTempFileTest = do
  tempDir <- getTemporaryDirectory
  withTempFile tempDir ".foo" $ \fileName handle -> do
    hClose handle
    removeFile fileName

withTempDirTest :: Assertion
withTempDirTest = do
  tempDir <- getTemporaryDirectory
  withTempDirectory normal tempDir "foo" $ \dirPath -> do
    removeDirectoryRecursive dirPath

tests :: [TestTree]
tests =
    [ testCase "withTempFile can handle removed files" $
      withTempFileTest
    , testCase "withTempDirectory can handle removed directories" $
      withTempDirTest
    ]
