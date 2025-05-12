{-# LANGUAGE CPP #-}
module Lib where

#if defined(javascript_HOST_ARCH)
foreign import javascript foo :: IO ()
#else
foo :: IO ()
foo = putStrLn "Not JS foo"
#endif
