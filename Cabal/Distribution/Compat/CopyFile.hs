{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Distribution.Compat.CopyFile (
  copyFile,
  copyOrdinaryFile,
  copyExecutableFile,
  setFileOrdinary,
  setFileExecutable,
  setDirOrdinary,
  ) where


import Control.Monad
         ( when )
import Control.Exception
         ( bracket, bracketOnError, throwIO )
import Distribution.Compat.Exception
         ( catchIO )
import System.IO.Error
         ( ioeSetLocation )
import System.Directory
         ( renameFile, removeFile )
import Distribution.Compat.TempFile
         ( openBinaryTempFile )
import System.FilePath
         ( takeDirectory )
import System.IO
         ( openBinaryFile, IOMode(ReadMode), hClose, hGetBuf, hPutBuf )
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
