{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CInt (..))

foreign import ccall "clib.h meaning_of_life_c"
  meaning_of_life_c :: IO CInt

main :: IO ()
main = do
    secret <- meaning_of_life_c
    -- The value 11 comes from __TESTOPT_C__ - see the cabal file.
    if (secret == 11)
        then putStrLn ("The secret is " ++ show secret)
        else error ("Expected value 11, got " ++ show secret)
