module UnitTests.TempTestDir (
    withTestDir
  ) where

import Distribution.Verbosity
import Distribution.Compat.Internal.TempFile (createTempDirectory)
import Distribution.Simple.Utils (warn)

import Control.Monad (when)
import Control.Exception (bracket, try, throwIO)
import Control.Concurrent (threadDelay)

import System.IO.Error (isPermissionError)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import qualified System.Info (os)


withTestDir :: Verbosity -> String ->  (FilePath -> IO a) -> IO a
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
removeDirectoryRecursiveHack :: Verbosity -> FilePath -> IO ()
removeDirectoryRecursiveHack verbosity dir | isWindows = go 0
  where
    isWindows = System.Info.os == "mingw32"
    limit     = 30

    go :: Int -> IO ()
    go n = do
      res <- try $ removeDirectoryRecursive dir
      case res of
        Left e
            -- wait a second and try again
          | isPermissionError e  &&  n+1 < limit -> do
              threadDelay 1000000
              go (n+1)

            -- but if we hit the limt warn and fail.
          | isPermissionError e -> do
              warn verbosity $ "Windows file locking hack: hit the retry limit "
                            ++ show n ++ " while trying to remove " ++ dir
              throwIO e

            -- or it's a different error fail.
          | otherwise -> throwIO e

        Right () ->
          when (n >= 3) $
            warn verbosity $ "Windows file locking hack: had to try " ++ show n
                          ++ " times to remove " ++ dir

removeDirectoryRecursiveHack _ dir = removeDirectoryRecursive dir

