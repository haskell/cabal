{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-- #hide
module Distribution.Compat.CopyFile (
  copyFile,
  copyOrdinaryFile,
  copyExecutableFile
  ) where

#ifdef __GLASGOW_HASKELL__

import Prelude hiding ( catch )
import Control.Monad
         ( when )
import Control.Exception
         ( throw, try, catch, bracket, bracketOnError, Exception(IOException) )
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
import System.Posix.Types
         ( FileMode )
import System.Posix.Internals
         ( c_chmod )
import Foreign.C
         ( withCString, throwErrnoPathIfMinus1_ )
#endif
#endif


copyOrdinaryFile, copyExecutableFile :: FilePath -> FilePath -> IO ()

#if defined(__GLASGOW_HASKELL__) && !defined(mingw32_HOST_OS)
copyOrdinaryFile fromFPath toFPath = do
  copyFile fromFPath toFPath
  setFileMode toFPath 0o644 -- file perms -rw-r--r--

copyExecutableFile fromFPath toFPath = do
  copyFile fromFPath toFPath
  setFileMode toFPath 0o755 -- file perms -rwxr-xr-x

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withCString name $ \s -> do
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)
#else
copyOrdinaryFile   = copyFile
copyExecutableFile = copyFile
#endif

copyFile :: FilePath -> FilePath -> IO ()
#ifdef __GLASGOW_HASKELL__
copyFile fromFPath toFPath =
    copy `catch` (\e -> case e of
                        IOException ioe ->
                            throw $ IOException $ ioeSetLocation ioe "copyFile"
                        _ -> throw e)
    where copy = bracket (openBinaryFile fromFPath ReadMode) hClose $ \hFrom ->
                 bracketOnError openTmp cleanTmp $ \(tmpFPath, hTmp) ->
                 do allocaBytes bufferSize $ copyContents hFrom hTmp
                    hClose hTmp
                    renameFile tmpFPath toFPath
          openTmp = openBinaryTempFile (takeDirectory toFPath) ".copyFile.tmp"
          cleanTmp (tmpFPath, hTmp) = do try $ hClose hTmp
                                         try $ removeFile tmpFPath
          bufferSize = 4096

          copyContents hFrom hTo buffer = do
                  count <- hGetBuf hFrom buffer bufferSize
                  when (count > 0) $ do
                          hPutBuf hTo buffer count
                          copyContents hFrom hTo buffer
#else
copyFile fromFPath toFPath = readFile fromFPath >>= writeFile toFPath
#endif
