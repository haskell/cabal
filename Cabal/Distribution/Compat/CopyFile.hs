{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Distribution.Compat.CopyFile (
  copyFile,
  copyFileChanged,
  filesEqual,
  copyOrdinaryFile,
  copyExecutableFile,
  setFileOrdinary,
  setFileExecutable,
  setDirOrdinary,
  ) where


import Control.Monad
         ( when, unless )
import Control.Exception
         ( bracket, bracketOnError, throwIO )
import qualified Data.ByteString.Lazy as BSL
import Distribution.Compat.Exception
         ( catchIO )
import System.IO.Error
         ( ioeSetLocation )
import System.Directory
         ( doesFileExist, renameFile, removeFile )
import Distribution.Compat.TempFile
         ( openBinaryTempFile )
import System.FilePath
         ( takeDirectory )
import System.IO
         ( openBinaryFile, IOMode(ReadMode), hClose, hGetBuf, hPutBuf
         , withBinaryFile )
import Foreign
         ( allocaBytes )

#ifndef mingw32_HOST_OS
import System.Posix.Internals (withFilePath)
import System.Posix.Types
         ( FileMode )
import System.Posix.Internals
         ( c_chmod )
import Foreign.C
         ( throwErrnoPathIfMinus1_ )
#endif /* mingw32_HOST_OS */

copyOrdinaryFile, copyExecutableFile :: FilePath -> FilePath -> IO ()
copyOrdinaryFile   src dest = copyFile src dest >> setFileOrdinary   dest
copyExecutableFile src dest = copyFile src dest >> setFileExecutable dest

setFileOrdinary,  setFileExecutable, setDirOrdinary  :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
setFileOrdinary   path = setFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = setFileMode path 0o755 -- file perms -rwxr-xr-x

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withFilePath name $ \s -> do
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)
#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
#endif
-- This happens to be true on Unix and currently on Windows too:
setDirOrdinary = setFileExecutable

-- | Copies a file to a new destination.
-- Often you should use `copyFileChanged` instead.
copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
  copy
    `catchIO` (\ioe -> throwIO (ioeSetLocation ioe "copyFile"))
    where copy = bracket (openBinaryFile fromFPath ReadMode) hClose $ \hFrom ->
                 bracketOnError openTmp cleanTmp $ \(tmpFPath, hTmp) ->
                 do allocaBytes bufferSize $ copyContents hFrom hTmp
                    hClose hTmp
                    renameFile tmpFPath toFPath
          openTmp = openBinaryTempFile (takeDirectory toFPath) ".copyFile.tmp"
          cleanTmp (tmpFPath, hTmp) = do
            hClose hTmp          `catchIO` \_ -> return ()
            removeFile tmpFPath  `catchIO` \_ -> return ()
          bufferSize = 4096

          copyContents hFrom hTo buffer = do
                  count <- hGetBuf hFrom buffer bufferSize
                  when (count > 0) $ do
                          hPutBuf hTo buffer count
                          copyContents hFrom hTo buffer

-- | Like `copyFile`, but does not touch the target if source and destination
-- are already byte-identical. This is recommended as it is useful for
-- time-stamp based recompilation avoidance.
copyFileChanged :: FilePath -> FilePath -> IO ()
copyFileChanged src dest = do
  equal <- filesEqual src dest
  unless equal $ copyFile src dest

-- | Checks if two files are byte-identical.
-- Returns False if either of the files do not exist.
filesEqual :: FilePath -> FilePath -> IO Bool
filesEqual f1 f2 = do
  ex1 <- doesFileExist f1
  ex2 <- doesFileExist f2
  if not (ex1 && ex2) then return False else do

    withBinaryFile f1 ReadMode $ \h1 ->
      withBinaryFile f2 ReadMode $ \h2 -> do
        c1 <- BSL.hGetContents h1
        c2 <- BSL.hGetContents h2
        return $! c1 == c2
