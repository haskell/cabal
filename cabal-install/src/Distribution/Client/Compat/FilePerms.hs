{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Distribution.Client.Compat.FilePerms (
  setFileOrdinary,
  setFileExecutable,
  setFileHidden,
  ) where

import Prelude (FilePath, IO, return, ($))
import Data.Bits ((.|.))

#ifndef mingw32_HOST_OS
import System.Posix.Types
         ( FileMode )
import System.Posix.Internals
         ( withFilePath
         , c_chmod
         , c_stat, sizeof_stat, st_mode )
import Foreign.C
         ( throwErrnoPathIfMinus1_ )
import Foreign.Marshal.Alloc
         ( allocaBytes )
#else
import System.Win32.File (setFileAttributes, fILE_ATTRIBUTE_HIDDEN)
#endif /* mingw32_HOST_OS */

setFileHidden, setFileOrdinary, setFileExecutable :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
-- When running with a restrictive UMASK such as 0077 we still want to
-- install files and directories that are accessible to other users.
setFileOrdinary   path = addFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = addFileMode path 0o755 -- file perms -rwxr-xr-x
setFileHidden     _    = return ()

addFileMode :: FilePath -> FileMode -> IO ()
addFileMode name m =
  withFilePath name $ \s -> allocaBytes sizeof_stat $ \ptr_stat -> do
    throwErrnoPathIfMinus1_ "maskFileMode: stat" name $
         c_stat s ptr_stat
    o <- st_mode ptr_stat
    throwErrnoPathIfMinus1_ "maskFileMode: chmod" name $
         c_chmod s (m .|. o)
#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
setFileHidden  path = setFileAttributes path fILE_ATTRIBUTE_HIDDEN
#endif
