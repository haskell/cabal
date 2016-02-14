module UnitTests.Distribution.Simple.Utils
    ( tests
    ) where

import Distribution.Simple.Utils
import Distribution.Verbosity

import Data.IORef
import System.Directory ( doesDirectoryExist, doesFileExist
                        , getTemporaryDirectory
                        , removeDirectoryRecursive, removeFile )
import System.IO (hClose)

import Test.Tasty
import Test.Tasty.HUnit

withTempFileTest :: Assertion
withTempFileTest = do
  fileName <- newIORef ""
  tempDir  <- getTemporaryDirectory
  withTempFile tempDir ".foo" $ \fileName' _handle -> do
    writeIORef fileName fileName'
  fileExists <- readIORef fileName >>= doesFileExist
  assertBool "Temporary file not deleted by 'withTempFile'!" (not fileExists)

withTempFileRemovedTest :: Assertion
withTempFileRemovedTest = do
  tempDir <- getTemporaryDirectory
  withTempFile tempDir ".foo" $ \fileName handle -> do
    hClose handle
    removeFile fileName

withTempDirTest :: Assertion
withTempDirTest = do
  dirName <- newIORef ""
  tempDir <- getTemporaryDirectory
  withTempDirectory normal tempDir "foo" $ \dirName' -> do
    writeIORef dirName dirName'
  dirExists <- readIORef dirName >>= doesDirectoryExist
  assertBool "Temporary directory not deleted by 'withTempDirectory'!"
    (not dirExists)

withTempDirRemovedTest :: Assertion
withTempDirRemovedTest = do
  tempDir <- getTemporaryDirectory
  withTempDirectory normal tempDir "foo" $ \dirPath -> do
    removeDirectoryRecursive dirPath

tests :: [TestTree]
tests =
    [ testCase "withTempFile works as expected" $
      withTempFileTest
    , testCase "withTempFile can handle removed files" $
      withTempFileRemovedTest
    , testCase "withTempDirectory works as expected" $
      withTempDirTest
    , testCase "withTempDirectory can handle removed directories" $
      withTempDirRemovedTest
    ]
