{-# LANGUAGE GADTs #-}
module UnitTests.Distribution.Simple.Utils
    ( tests
    ) where

import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Utils
import Distribution.System ( buildPlatform )
import Distribution.Verbosity

import Data.IORef
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory ( doesDirectoryExist, doesFileExist
                        , copyFileWithMetadata, createDirectoryIfMissing
                        , getModificationTime, setModificationTime
                        , getTemporaryDirectory
                        , removePathForcibly )
import System.FilePath ( (<.>), (</>) )
import System.IO.Temp (withSystemTempDirectory)
import System.IO (hClose, localeEncoding, hPutStrLn)
import System.IO.Error
import qualified Control.Exception as Exception

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (NonNegative (..), Property, ioProperty, testProperty, (===))

withTempFileTest :: Assertion
withTempFileTest = do
  fileName <- newIORef ""
  withTempFile ".foo" $ \fileName' _handle -> do
    writeIORef fileName fileName'
  fileExists <- readIORef fileName >>= doesFileExist
  assertBool "Temporary file not deleted by 'withTempFile'!" (not fileExists)

withTempFileRemovedTest :: Assertion
withTempFileRemovedTest = do
  withTempFile ".foo" $ \fileName handle -> do
    hClose handle
    removeFileForcibly fileName

withTempDirTest :: Assertion
withTempDirTest = do
  dirName <- newIORef ""
  tempDir <- getTemporaryDirectory
  withTempDirectory tempDir "foo" $ \dirName' -> do
    writeIORef dirName dirName'
  dirExists <- readIORef dirName >>= doesDirectoryExist
  assertBool "Temporary directory not deleted by 'withTempDirectory'!"
    (not dirExists)

withTempDirRemovedTest :: Assertion
withTempDirRemovedTest = do
  tempDir <- getTemporaryDirectory
  withTempDirectory tempDir "foo" removePathForcibly

rawSystemStdInOutTextDecodingTest :: FilePath -> Assertion
rawSystemStdInOutTextDecodingTest ghcPath
    -- We can only get this exception when the locale encoding is UTF-8
    -- so skip the test if it's not.
    | show localeEncoding /= "UTF-8" = return ()
    | otherwise = do
  res <- withTempFile ".hs" $ \filenameHs handleHs -> do
    withTempFile ".exe" $ \filenameExe handleExe -> do
      -- Small program printing not utf8
      hPutStrLn handleHs "import Data.ByteString"
      hPutStrLn handleHs "main = Data.ByteString.putStr (Data.ByteString.pack [32, 32, 255])"
      hClose handleHs

      -- We need to close exe handle as well, otherwise compilation (writing) may fail
      hClose handleExe

      -- Compile
      (resOutput, resErrors, resExitCode) <- rawSystemStdInOut (mkVerbosity defaultVerbosityHandles normal)
         ghcPath ["-o", filenameExe, filenameHs]
         Nothing Nothing Nothing
         IODataModeText
      print (resOutput, resErrors, resExitCode)

      -- Execute
      Exception.try $ do
        rawSystemStdInOut (mkVerbosity defaultVerbosityHandles normal)
           filenameExe []
           Nothing Nothing Nothing
           IODataModeText -- not binary mode output, ie utf8 text mode so try to decode
  case res of
    Right (x1, x2, x3) -> assertFailure $ "expected IO decoding exception: " ++ show (x1,x2,x3)
    Left err | isDoesNotExistError err -> Exception.throwIO err -- no ghc!
             | otherwise               -> return ()

dropExeExtensionTest :: Assertion
dropExeExtensionTest =
  assertBool "dropExeExtension didn't drop exeExtension!" $
    dropExeExtension ("foo" <.> exeExtension buildPlatform) == "foo"


-- | Copying compilation artifacts with metadata preserves their modification times.
copyDirectoryPreservesModificationTime :: NonNegative Int -> Property
copyDirectoryPreservesModificationTime (NonNegative seconds) = ioProperty $
  withSystemTempDirectory "copy-directory-mtime" $ \dir -> do
    let src = dir </> "src"
        dest = dir </> "dest"
        relativeFile = "nested" </> "Module.hi"
        -- Use an old timestamp, with two-second precision for FAT filesystems.
        timestamp = posixSecondsToUTCTime (946684800 + fromIntegral (seconds `mod` 86400) * 2)
    createDirectoryIfMissing True (src </> "nested")
    writeFile (src </> relativeFile) "interface"
    setModificationTime (src </> relativeFile) timestamp
    sourceTime <- getModificationTime (src </> relativeFile)
    copyDirectoryRecursiveWith copyFileWithMetadata (mkVerbosity defaultVerbosityHandles silent) src dest
    copiedTime <- getModificationTime (dest </> relativeFile)
    return (copiedTime === sourceTime)

tests :: FilePath -> [TestTree]
tests ghcPath =
    [ testCase "withTempFile works as expected" withTempFileTest
    , testCase "withTempFile can handle removed files" withTempFileRemovedTest
    , testCase "withTempDirectory works as expected" withTempDirTest
    , testCase "withTempDirectory can handle removed directories" withTempDirRemovedTest
    , testCase "rawSystemStdInOut reports text decoding errors" $
      rawSystemStdInOutTextDecodingTest ghcPath
    , testCase "dropExeExtension drops exe extension" dropExeExtensionTest
    , testProperty "copyDirectoryRecursiveWith preserves file modification times"
      copyDirectoryPreservesModificationTime
    ]
