{-# OPTIONS -cpp #-}
module Compat.Exception (bracket) where

#ifdef __NHC__
import System.IO.Error (catch, ioError)
#else
import Control.Exception (bracket)
#endif

#ifdef __NHC__
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing
  = do a <- before
       r <- thing a `catch` \e -> do after a
                                     ioError e
       after a
       return r
#endif
