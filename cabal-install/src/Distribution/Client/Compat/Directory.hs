{-# LANGUAGE CPP #-}

module Distribution.Client.Compat.Directory
  ( setModificationTime
  , createFileLink
  , pathIsSymbolicLink
  , getSymbolicLinkTarget
  ) where

#if MIN_VERSION_directory(1,2,3)
import System.Directory (setModificationTime)
#else
import Data.Time.Clock (UTCTime)
#endif

#if MIN_VERSION_directory(1,3,1)
import System.Directory (createFileLink, getSymbolicLinkTarget, pathIsSymbolicLink)
#elif defined(MIN_VERSION_unix)
import System.Posix.Files (createSymbolicLink, getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
#endif

-------------------------------------------------------------------------------
-- setModificationTime
-------------------------------------------------------------------------------

#if !MIN_VERSION_directory(1,2,3)

setModificationTime :: FilePath -> UTCTime -> IO ()
setModificationTime _fp _t = return ()

#endif

-------------------------------------------------------------------------------
-- Symlink
-------------------------------------------------------------------------------

#if MIN_VERSION_directory(1,3,1)
#elif defined(MIN_VERSION_unix)
createFileLink :: FilePath -> FilePath -> IO ()
createFileLink = createSymbolicLink

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink fp = do
    status <- getSymbolicLinkStatus fp
    return (isSymbolicLink status)

getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget = readSymbolicLink

#else
createFileLink :: FilePath -> FilePath -> IO ()
createFileLink _ _ = fail "Symlinking feature not available"

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink _ = fail "Symlinking feature not available"

getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget _ = fail "Symlinking feature not available"
#endif
