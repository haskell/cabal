module UnitTests.Distribution.Simple.Utils
    ( tests
    ) where

import Distribution.Simple.Utils
import Distribution.Verbosity

import Data.IORef
import System.Directory ( doesDirectoryExist, doesFileExist
                        , getTemporaryDirectory
                        , removeDirectoryRecursive, removeFile )
import System.IO (hClose, localeEncoding)
import System.IO.Error
import qualified Control.Exception as Exception

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

rawSystemStdInOutTextDecodingTest :: Assertion
rawSystemStdInOutTextDecodingTest
    -- We can only get this exception when the locale encoding is UTF-8
    -- so skip the test if it's not.
  | show localeEncoding /= "UTF-8" = return ()
  | otherwise = do
  res <- Exception.try $
    rawSystemStdInOut normal
      -- hopefully this is sufficiently portable, we just need to execute a
      -- program that will produce non-unicode output:
      "ghc" ["-e", "Data.ByteString.putStr (Data.ByteString.pack [255])"]
      Nothing Nothing Nothing
      False -- not binary mode output, ie utf8 text mode so try to decode
  case res of
    Right _ -> assertFailure "expected IO decoding exception"
    Left err | isDoesNotExistError err -> Exception.throwIO err -- no ghc!
             | otherwise               -> return ()

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
    , testCase "rawSystemStdInOut reports text decoding errors" $
      rawSystemStdInOutTextDecodingTest
    ]
