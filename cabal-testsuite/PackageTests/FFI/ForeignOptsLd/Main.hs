{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CInt (..))

-- With ld-options: -Wl,--wrap=meaning_of_life_ld_real, the linker redirects
-- this call to __wrap_meaning_of_life_ld_real, which returns 55.
foreign import ccall "ldlib.h meaning_of_life_ld_real"
  meaning_of_life_ld_real :: IO CInt

main :: IO ()
main = do
    secret <- meaning_of_life_ld_real
    -- The value 55 comes from __wrap_meaning_of_life_ld_real - see ld-options in the cabal file.
    if (secret == 55)
        then putStrLn ("The secret is " ++ show secret)
        else error ("Expected value 55, got " ++ show secret)
