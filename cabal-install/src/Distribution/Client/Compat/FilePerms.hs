{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Distribution.Client.Compat.FilePerms (
  setFileOrdinary,
  setFileExecutable,
  setFileHidden,
  ) where

import Prelude (FilePath, IO, return)
import Data.Bits ((.|.))

#ifndef mingw32_HOST_OS
import Prelude ((<$>))
import System.Posix.Types
         ( FileMode )
import System.Posix.Files
         ( getFileStatus, fileMode, setFileMode )
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
addFileMode name m = do
  o <- fileMode <$> getFileStatus name
  setFileMode name (m .|. o)

#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
setFileHidden  path = setFileAttributes path fILE_ATTRIBUTE_HIDDEN
#endif
