{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CInt (..))

foreign import ccall "cxxlib.h meaning_of_life_cxx"
  meaning_of_life_cxx :: IO CInt

main :: IO ()
main = do
    secret <- meaning_of_life_cxx
    -- The value 22 comes from __TESTOPT_CXX__ - see the cabal file.
    if (secret == 22)
        then putStrLn ("The secret is " ++ show secret)
        else error ("Expected value 22, got " ++ show secret)
