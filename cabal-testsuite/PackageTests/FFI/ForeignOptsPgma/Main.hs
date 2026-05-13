{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CInt (..))

foreign import ccall "pgma.h meaning_of_life_asm"
  meaning_of_life_asm :: IO CInt

main :: IO ()
main = do
  secret <- meaning_of_life_asm
  if (secret == 33)
    then putStrLn ("The secret is " ++ show secret)
    else error ("Expected value 33, got " ++ show secret)
