{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import Foreign.C (CInt (..))

hello :: IO CInt
hello = do
  putStrLn "hello!"
  return 11

foreign export ccall "hello" hello :: IO CInt

