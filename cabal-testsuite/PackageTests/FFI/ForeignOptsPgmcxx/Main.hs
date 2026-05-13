{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CInt (..))

foreign import ccall "pgmcxxlib.h meaning_of_life_pgmcxx"
  meaning_of_life_pgmcxx :: IO CInt

main :: IO ()
main = do
  secret <- meaning_of_life_pgmcxx
  -- The value 67 comes from __TESTOPT_PGMCXX__ - see cxx-wrapper.sh.
  if (secret == 67)
    then putStrLn ("The secret is " ++ show secret)
    else error ("Expected value 67, got " ++ show secret)
