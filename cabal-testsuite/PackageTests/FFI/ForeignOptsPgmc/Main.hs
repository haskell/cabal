{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CInt (..))

foreign import ccall "pgmclib.h meaning_of_life_pgmc"
  meaning_of_life_pgmc :: IO CInt

main :: IO ()
main = do
  secret <- meaning_of_life_pgmc
  -- The value 66 comes from __TESTOPT_PGMC__ - see cc-wrapper.sh.
  if (secret == 66)
    then putStrLn ("The secret is " ++ show secret)
    else error ("Expected value 66, got " ++ show secret)
