{-# LANGUAGE CPP #-}

module UnitTests.TempTestDir
  ( withTestDir
  , removeDirectoryRecursiveHack
  ) where

import Distribution.Compat.Internal.TempFile (createTempDirectory)
import Distribution.Simple.Utils (warn)
import Distribution.Verbosity

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, throwIO, try)
import Control.Monad (when)

import System.Directory
import System.IO.Error
#if !(MIN_VERSION_directory(1,2,7))
import System.FilePath ((</>))
#endif
import qualified System.Info (os)

-- | Much like 'withTemporaryDirectory' but with a number of hacks to make
-- sure on windows that we can clean up the directory at the end.
withTestDir :: Verbosity -> String -> (FilePath -> IO a) -> IO a
withTestDir verbosity template action = do
  systmpdir <- getTemporaryDirectory
  bracket
    (createTempDirectory systmpdir template)
    (removeDirectoryRecursiveHack verbosity)
    action

-- | On Windows, file locks held by programs we run (in this case VCSs)
-- are not always released prior to completing process termination!
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/aa365202.aspx>
-- This means we run into stale locks when trying to delete the test
-- directory. There is no sane way to wait on those locks being released,
-- we just have to wait, try again and hope.
--
-- In addition, on Windows a file that is not writable also cannot be deleted,
-- so we must try setting the permissions to readable before deleting files.
-- Some VCS tools on Windows create files with read-only attributes.
removeDirectoryRecursiveHack :: Verbosity -> FilePath -> IO ()
removeDirectoryRecursiveHack verbosity dir | isWindows = go 1
  where
    isWindows = System.Info.os == "mingw32"
    limit = 3

    go :: Int -> IO ()
    go n = do
      res <- try $ removePathForcibly dir
      case res of
        Left e
          -- wait a second and try again
          | isPermissionError e && n < limit -> do
              threadDelay 1000000
              go (n + 1)

          -- but if we hit the limt warn and fail.
          | isPermissionError e -> do
              warn verbosity $
                "Windows file locking hack: hit the retry limit "
                  ++ show limit
                  ++ " while trying to remove "
                  ++ dir
              throwIO e

          -- or it's a different error fail.
          | otherwise -> throwIO e
        Right () ->
          when (n > 1) $
            warn verbosity $
              "Windows file locking hack: had to try "
                ++ show n
                ++ " times to remove "
                ++ dir
removeDirectoryRecursiveHack _ dir = removeDirectoryRecursive dir

#if !(MIN_VERSION_directory(1,2,7))
-- A simplified version that ought to work for our use case here, and does
-- not rely on directory internals.
removePathForcibly :: FilePath -> IO ()
removePathForcibly path = do
    makeRemovable path `catchIOError` \ _ -> pure ()
    isDir <- doesDirectoryExist path
    if isDir
       then do
         entries <- getDirectoryContents path
         sequence_
           [ removePathForcibly (path </> entry)
           | entry <- entries, entry /= ".", entry /= ".." ]
         removeDirectory path
       else
         removeFile path
  where
    makeRemovable :: FilePath -> IO ()
    makeRemovable p =
      setPermissions p emptyPermissions {
        readable   = True,
        searchable = True,
        writable   = True
      }
#endif
