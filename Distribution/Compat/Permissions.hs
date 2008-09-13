-- #hide
module Distribution.Compat.Permissions (copyPermissions) where

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

