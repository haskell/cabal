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
  copyExecutableFile,
  setFileOrdinary,
  setFileExecutable,
  ) where

#ifdef __GLASGOW_HASKELL__

import Control.Monad
         ( when )
import Control.Exception
         ( bracket, bracketOnError )
import Distribution.Compat.Exception
         ( catchIO )
#if __GLASGOW_HASKELL__ >= 608
import Distribution.Compat.Exception
         ( throwIOIO )
import System.IO.Error
         ( ioeSetLocation )
#endif
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
#endif /* __GLASGOW_HASKELL__ */

#ifndef mingw32_HOST_OS
import System.Posix.Types
         ( FileMode )
import System.Posix.Internals
         ( c_chmod )
import Foreign.C
         ( withCString )
#if __GLASGOW_HASKELL__ >= 608
import Foreign.C
         ( throwErrnoPathIfMinus1_ )
#else
import Foreign.C
         ( throwErrnoIfMinus1_ )
#endif
#endif /* mingw32_HOST_OS */

copyOrdinaryFile, copyExecutableFile :: FilePath -> FilePath -> IO ()
copyOrdinaryFile   src dest = copyFile src dest >> setFileOrdinary   dest
copyExecutableFile src dest = copyFile src dest >> setFileExecutable dest

setFileOrdinary,  setFileExecutable  :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
setFileOrdinary   path = setFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = setFileMode path 0o755 -- file perms -rwxr-xr-x

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withCString name $ \s -> do
#if __GLASGOW_HASKELL__ >= 608
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)
#else
    throwErrnoIfMinus1_                   name (c_chmod s m)
#endif
#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
#endif

copyFile :: FilePath -> FilePath -> IO ()
#ifdef __GLASGOW_HASKELL__
copyFile fromFPath toFPath =
  copy
#if __GLASGOW_HASKELL__ >= 608
    `catchIO` (\ioe -> throwIOIO (ioeSetLocation ioe "copyFile"))
#endif
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
#else
copyFile fromFPath toFPath = readFile fromFPath >>= writeFile toFPath
#endif
