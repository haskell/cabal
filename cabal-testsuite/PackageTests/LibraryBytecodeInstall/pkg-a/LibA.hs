{-# LANGUAGE ForeignFunctionInterface #-}

module LibA where

import Foreign.C.Types

-- Import a C function
foreign import ccall "multiply_numbers" multiplyNumbers :: CInt -> CInt -> CInt

-- Export a function that uses the C function
libAValue :: Int
libAValue = fromIntegral $ multiplyNumbers 6 7

