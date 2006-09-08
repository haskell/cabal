{-# OPTIONS_GHC -cpp #-}
-- #hide
module Distribution.Compat.Exception (bracket,finally) where

#ifdef __NHC__
import System.IO.Error (catch, ioError)
import IO (bracket)
#else
import Control.Exception (bracket,finally)
#endif

#ifdef __NHC__
finally :: IO a -> IO b -> IO a
finally thing after = bracket (return ()) (const after) (const thing)
#endif
