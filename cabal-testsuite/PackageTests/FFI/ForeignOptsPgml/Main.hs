{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CInt (..))

foreign import ccall "pgmllib.h meaning_of_life_pgml"
  meaning_of_life_pgml :: IO CInt

main :: IO ()
main = do
    secret <- meaning_of_life_pgml
    -- The value 66 comes from __wrap_meaning_of_life_pgml: it is only
    -- linked in when GHC's linker is driven by scripts/cc-wrapper.sh,
    -- which Cabal passes to GHC as -pgml (--with-gcc selects it).
    if secret == 66
        then putStrLn ("The secret is " ++ show secret)
        else error ("Expected value 66, got " ++ show secret)
