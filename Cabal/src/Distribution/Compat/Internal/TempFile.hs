{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Internal.TempFile
  ( openTempFile
  , openBinaryTempFile
  , openNewBinaryFile
  , createTempDirectory
  ) where

import Distribution.Compat.Exception

import GHC.IORef (IORef, atomicModifyIORef'_, newIORef)
import System.FilePath ((</>))
import System.IO (Handle, openBinaryTempFile, openBinaryTempFileWithDefaultPermissions, openTempFile)
import System.IO.Error (isAlreadyExistsError)
import System.IO.Unsafe (unsafePerformIO)
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
  let findTempName = do
        (counter, _) <- atomicModifyIORef'_ tempDirectoryCounter (+ 1)
        let relpath = template ++ "-" ++ show pid ++ show counter
            dirpath = dir </> relpath
        r <- tryIO $ mkPrivateDir dirpath
        case r of
          Right _ -> pure relpath
          Left e
            | isAlreadyExistsError e -> findTempName
            | otherwise -> ioError e
  findTempName

tempDirectoryCounter :: IORef Word
tempDirectoryCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE tempDirectoryCounter #-}

mkPrivateDir :: String -> IO ()
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
mkPrivateDir s = createDirectory s
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif
