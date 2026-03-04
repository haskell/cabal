{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types (CInt(..))

import A (bobble)

foreign import ccall razzle :: CInt -> CInt

main = do
  print bobble
  print (razzle 3)

