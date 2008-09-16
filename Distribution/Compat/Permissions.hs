{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}

module Distribution.Compat.Permissions (copyPermissions) where

#ifndef __NHC__
import Foreign
import Foreign.C
import System.Posix.Internals

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions source dest = do
  allocaBytes sizeof_stat $ \ p_stat -> do
  withCString source $ \p_source -> do
  withCString dest $ \p_dest -> do
    throwErrnoIfMinus1_ "copyPermissions" $ c_stat p_source p_stat
    mode <- st_mode p_stat
    throwErrnoIfMinus1_ "copyPermissions" $ c_chmod p_dest mode

#else
import Directory (Permissions(..),getPermissions,setPermissions)

-- The nhc98 version of this function is broken.  (But it is the way ghc
-- and Hugs implemented it too, until 2008-09-13.)  Unfortunately, nhc98
-- does not have System.Posix.Internals, so cannot (yet) do it correctly.
copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions source dest = do
  perms <- getPermissions source
  setPermissions dest perms
#endif
