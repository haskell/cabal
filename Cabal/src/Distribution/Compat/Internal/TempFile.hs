{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Internal.TempFile
  ( openTempFile
  , openBinaryTempFile
  , openNewBinaryFile
  , createTempDirectory
  ) where

import Distribution.Compat.Exception

import System.FilePath ((</>))

import System.IO (Handle, openBinaryTempFile, openBinaryTempFileWithDefaultPermissions, openTempFile)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Internals (c_getpid)

#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
import System.Directory       ( createDirectory )
#else
import qualified System.Posix
#endif

openNewBinaryFile :: FilePath -> String -> IO (FilePath, Handle)
openNewBinaryFile = openBinaryTempFileWithDefaultPermissions

createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let relpath = template ++ "-" ++ show x
          dirpath = dir </> relpath
      r <- tryIO $ mkPrivateDir dirpath
      case r of
        Right _ -> return relpath
        Left e
          | isAlreadyExistsError e -> findTempName (x + 1)
          | otherwise -> ioError e

mkPrivateDir :: String -> IO ()
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
mkPrivateDir s = createDirectory s
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif
