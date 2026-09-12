module Lib where

foreign import ccall unsafe "ghc_toolchain_c_value"
  c_value :: IO Int
