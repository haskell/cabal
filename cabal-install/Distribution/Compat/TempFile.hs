{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-- #hide
module Distribution.Compat.TempFile (
  createTempDirectory,
  ) where

import System.FilePath        ((</>))
import System.Posix.Internals (mkdir, c_getpid)
import Foreign.C              (withCString, getErrno, eEXIST, errnoToIOError)

createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let dirpath = dir </> template ++ show x
      res <- withCString dirpath $ \s -> mkdir s 0o700
      if res == 0
        then return dirpath
        else do
          errno <- getErrno
          if errno == eEXIST
            then findTempName (x+1)
            else ioError (errnoToIOError "createTempDirectory" errno Nothing (Just dir))
