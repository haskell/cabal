{-# LANGUAGE CPP #-}
-- #hide
module Distribution.Compat.FilePerms (
  setFileOrdinary,
  setFileExecutable,
  ) where

#ifndef mingw32_HOST_OS
import System.Posix.Types
         ( FileMode )
import System.Posix.Internals
         ( c_chmod )
import Foreign.C
         ( withCString )
#if MIN_VERSION_base(4,0,0)
import Foreign.C
         ( throwErrnoPathIfMinus1_ )
#else
import Foreign.C
         ( throwErrnoIfMinus1_ )
#endif
#endif /* mingw32_HOST_OS */

setFileOrdinary,  setFileExecutable  :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
setFileOrdinary   path = setFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = setFileMode path 0o755 -- file perms -rwxr-xr-x

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withCString name $ \s -> do
#if __GLASGOW_HASKELL__ >= 608
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)
#else
    throwErrnoIfMinus1_                   name (c_chmod s m)
#endif
#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
#endif
