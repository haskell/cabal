{-# OPTIONS -cpp #-}
module Distribution.Compat.Exception (bracket,finally) where

#ifdef __NHC__
import System.IO.Error (catch, ioError)
#else
import Control.Exception (bracket,finally)
#endif

#ifdef __NHC__
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing
  = do a <- before
       r <- thing a `catch` \e -> do after a
                                     ioError e
       after a
       return r

finally :: IO a -> IO b -> IO a
finally thing after = bracket (return ()) (const after) thing
#endif
