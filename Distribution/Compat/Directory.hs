{-# OPTIONS_GHC -cpp #-}
-- #hide
module Distribution.Compat.Directory (
        module System.Directory,
#if __GLASGOW_HASKELL__ <= 602
 	findExecutable, copyFile, getHomeDirectory, createDirectoryIfMissing,
        removeDirectoryRecursive,
#endif
        getDirectoryContentsWithoutSpecial
  ) where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 604
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

#if !__GLASGOW_HASKELL__ || __GLASGOW_HASKELL__ > 602

import System.Directory

#else /* to end of file... */

import System.Environment	( getEnv )
import Distribution.Compat.FilePath
import System.IO
import Foreign
import System.Directory
import Distribution.Compat.Exception (bracket)
import Control.Monad (when, unless)
#if !(mingw32_HOST_OS || mingw32_TARGET_OS)
import System.Posix (getFileStatus,setFileMode,fileMode,accessTime,
		     setFileMode,modificationTime,setFileTimes)
#endif

findExecutable :: String -> IO (Maybe FilePath)
findExecutable binary = do
  path <- getEnv "PATH"
  search (parseSearchPath path)
  where
    search :: [FilePath] -> IO (Maybe FilePath)
    search [] = return Nothing
    search (d:ds) = do
       let path = d `joinFileName` binary `joinFileExt` exeSuffix
       b <- doesFileExist path
       if b then return (Just path)
             else search ds

exeSuffix :: String
#if mingw32_HOST_OS || mingw32_TARGET_OS
exeSuffix = "exe"
#else
exeSuffix = ""
#endif

copyPermissions :: FilePath -> FilePath -> IO ()
#if !(mingw32_HOST_OS || mingw32_TARGET_OS)
copyPermissions src dest
    = do srcStatus <- getFileStatus src
         setFileMode dest (fileMode srcStatus)
#else
copyPermissions src dest
    = getPermissions src >>= setPermissions dest
#endif


copyFileTimes :: FilePath -> FilePath -> IO ()
#if !(mingw32_HOST_OS || mingw32_TARGET_OS)
copyFileTimes src dest
   = do st <- getFileStatus src
        let atime = accessTime st
            mtime = modificationTime st
        setFileTimes dest atime mtime
#else
copyFileTimes src dest
    = return ()
#endif

-- |Preserves permissions and, if possible, atime+mtime
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest 
    | dest == src = fail "copyFile: source and destination are the same file"
#if (!(defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 600))
    | otherwise = do readFile src >>= writeFile dest
                     try (copyPermissions src dest)
                     return ()
#else
    | otherwise = bracket (openBinaryFile src ReadMode) hClose $ \hSrc ->
                  bracket (openBinaryFile dest WriteMode) hClose $ \hDest ->
                  do allocaBytes bufSize $ \buffer -> copyContents hSrc hDest buffer
                     try (copyPermissions src dest)
                     try (copyFileTimes src dest)
                     return ()
  where bufSize = 1024
        copyContents hSrc hDest buffer
           = do count <- hGetBuf hSrc buffer bufSize
                when (count > 0) $ do hPutBuf hDest buffer count
                                      copyContents hSrc hDest buffer
#endif

getHomeDirectory :: IO FilePath
getHomeDirectory = getEnv "HOME"

createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
		         -> FilePath -- ^ The path to the directory you want to make
		         -> IO ()
createDirectoryIfMissing parents file = do
  b <- doesDirectoryExist file
  case (b,parents, file) of 
    (_,     _, "") -> return ()
    (True,  _,  _) -> return ()
    (_,  True,  _) -> mapM_ (createDirectoryIfMissing False) (tail (pathParents file))
    (_, False,  _) -> createDirectory file

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive startLoc = do
  cont <- getDirectoryContentsWithoutSpecial startLoc
  mapM_ (rm . joinFileName startLoc) cont
  removeDirectory startLoc
  where
    rm :: FilePath -> IO ()
    rm f = do temp <- try (removeFile f)
              case temp of
                Left e  -> do isDir <- doesDirectoryExist f
                              -- If f is not a directory, re-throw the error
                              unless isDir $ ioError e
                              removeDirectoryRecursive f
                Right _ -> return ()

#endif

getDirectoryContentsWithoutSpecial :: FilePath -> IO [FilePath]
getDirectoryContentsWithoutSpecial =
   fmap (filter (not . flip elem [".", ".."])) . getDirectoryContents
