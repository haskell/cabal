{-# LANGUAGE CApiFFI #-}

module Main where

import Foreign.C (CInt (..))

foreign import capi "clib.h myplus"
  myplus :: CInt -> CInt -> IO CInt

main :: IO ()
main = do
    result <- myplus 5 6
    if (result == 11)
        then putStrLn ("The result is " ++ show result)
        else error ("Expected value 11, got " ++ show result)
