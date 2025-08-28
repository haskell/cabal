{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import Foreign.C.Types

-- Import a C function
foreign import ccall "add_numbers" addNumbers :: CInt -> CInt -> CInt

-- Use the C function in Haskell code
libValue :: Int
libValue = fromIntegral $ addNumbers 20 22

