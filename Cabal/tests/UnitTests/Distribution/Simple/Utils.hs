module UnitTests.Distribution.Simple.Utils
    ( tests
    ) where

import Distribution.Simple.Utils
import Distribution.Verbosity

import Data.Monoid as Mon
import Data.IORef
import System.Directory ( doesDirectoryExist, doesFileExist
                        , getTemporaryDirectory
                        , removeDirectoryRecursive, removeFile )
import System.IO (hClose, localeEncoding, hPutStrLn)
import System.IO.Error
import qualified Control.Exception as Exception

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Distribution.Compat.Binary (encode, decode)

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
  tempDir  <- getTemporaryDirectory
  res <- withTempFile tempDir ".hs" $ \filenameHs handleHs -> do
    withTempFile tempDir ".exe" $ \filenameExe handleExe -> do
      -- Small program printing not utf8
      hPutStrLn handleHs "import Data.ByteString"
      hPutStrLn handleHs "main = Data.ByteString.putStr (Data.ByteString.pack [32, 32, 255])"
      hClose handleHs

      -- We need to close exe handle as well, otherwise compilation (writing) may fail
      hClose handleExe

      -- Compile
      compilationResult <- rawSystemStdInOut normal
         "ghc" ["-o", filenameExe, filenameHs]
         Nothing Nothing Nothing
        False
      print compilationResult

      -- Execute
      Exception.try $ do
        rawSystemStdInOut normal
           filenameExe []
           Nothing Nothing Nothing
           False -- not binary mode output, ie utf8 text mode so try to decode
  case res of
    Right x -> assertFailure $ "expected IO decoding exception: " ++ show x
    Left err | isDoesNotExistError err -> Exception.throwIO err -- no ghc!
             | otherwise               -> return ()



prop_ShortTextOrd :: String -> String -> Bool
prop_ShortTextOrd a b = compare a b == compare (toShortText a) (toShortText b)

prop_ShortTextMonoid :: String -> String -> Bool
prop_ShortTextMonoid a b = Mon.mappend a b == fromShortText (mappend (toShortText a) (toShortText b))

prop_ShortTextId :: String -> Bool
prop_ShortTextId a = (fromShortText . toShortText) a == a

prop_ShortTextBinaryId :: String -> Bool
prop_ShortTextBinaryId a = (decode . encode) a' == a'
  where
    a' = toShortText a

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

    , testProperty "ShortText Id" prop_ShortTextId
    , testProperty "ShortText Ord" prop_ShortTextOrd
    , testProperty "ShortText Monoid" prop_ShortTextMonoid
    , testProperty "ShortText BinaryId" prop_ShortTextBinaryId
    ]
