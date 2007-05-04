{-# OPTIONS -cpp #-}
-- #hide
module Distribution.Compat.TempFile (openTempFile, withTempFile) where

import System.IO (openFile, Handle, IOMode(ReadWriteMode))
import System.Directory (doesFileExist, removeFile)
import Control.Exception (finally,try)

import Distribution.Compat.FilePath (joinFileName,joinFileExt)

#if (__GLASGOW_HASKELL__ || __HUGS__)
import System.Posix.Internals (c_getpid)
#else
import System.Posix.Types (CPid(..))
#endif


-- ------------------------------------------------------------
-- * temporary files
-- ------------------------------------------------------------

-- TODO: this function *really really really* should be
--       eliminated and replaced with System.IO.openTempFile,
--       except that is currently GHC-only for no valid reason.

-- use a temporary filename that doesn't already exist.
-- NB. *not* secure (we don't atomically lock the tmp file we get)
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile tmp_dir template
  = do x <- getProcessID
       findTempName x
  where 
    findTempName x
      = do let filename = template ++ show x
	       path = tmp_dir `joinFileName` filename
  	   b  <- doesFileExist path
	   if b then findTempName (x+1)
		else do hnd <- openFile path ReadWriteMode
                        return (path, hnd)

#if !(__GLASGOW_HASKELL__ || __HUGS__)
foreign import ccall unsafe "getpid" c_getpid :: IO CPid
#endif

getProcessID :: IO Int
getProcessID = c_getpid >>= return . fromIntegral

-- use a temporary filename that doesn't already exist.
-- NB. *not* secure (we don't atomically lock the tmp file we get)
withTempFile :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempFile tmp_dir extn action
  = do x <- getProcessID
       findTempName x
  where
    findTempName x
      = do let filename = ("tmp" ++ show x) `joinFileExt` extn
               path = tmp_dir `joinFileName` filename
           b  <- doesFileExist path
           if b then findTempName (x+1)
                else action path `finally` try (removeFile path)

