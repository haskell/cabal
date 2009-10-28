{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-- #hide
module Distribution.Compat.TempFile (
  createTempDirectory,
  ) where

import System.FilePath        ((</>))
#ifdef mingw32_HOST_OS
import System.Directory       (createDirectory)
#else
import System.Posix.Directory (createDirectory)
#endif
import System.IO.Error        (try, isAlreadyExistsError)

#if __NHC__
import System.Posix.Types     (CPid(..))
foreign import ccall unsafe "getpid" c_getpid :: IO CPid
#else
import System.Posix.Internals (c_getpid)
#endif

createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let dirpath = dir </> template ++ show x
      r <- try $ mkPrivateDir dirpath
      case r of
        Right _ -> return dirpath
        Left  e | isAlreadyExistsError e -> findTempName (x+1)
                | otherwise              -> ioError e

mkPrivateDir :: String -> IO ()
#ifdef mingw32_HOST_OS
mkPrivateDir s = System.Directory.createDirectory s
#else
mkPrivateDir s = System.Posix.Directory.createDirectory s 0o700
#endif
