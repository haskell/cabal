{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CInt (..))

foreign import ccall "meaning_of_life_c"
  meaning_of_life_c :: IO CInt

main :: IO ()
main = do
    secret <- meaning_of_life_c
    -- The value 11 comes from the exported Lib.hello
    if (secret == 11)
        then putStrLn ("The secret is " ++ show secret)
        else error ("Expected value 11, got " ++ show secret)
