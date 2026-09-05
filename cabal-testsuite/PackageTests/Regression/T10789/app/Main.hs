{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Foreign.C (CInt (..))
import Lib (greeting)

-- With `ld-options: -Wl,--wrap=meaning_of_life_ld_real` in cabal.project,
-- the linker redirects all calls to `meaning_of_life_ld_real` to
-- `__wrap_meaning_of_life_ld_real`, which returns 55.
foreign import ccall "ldlib.h meaning_of_life_ld_real"
  meaning_of_life_ld_real :: IO CInt

main :: IO ()
main = do
    secret <- meaning_of_life_ld_real
    -- The value 55 comes from __wrap_meaning_of_life_ld_real, see
    -- `ld-options` in cabal.project.
    if secret == 55
        then putStrLn ("The secret is " ++ show secret)
        else error ("Expected value 55, got " ++ show secret)
    putStrLn greeting
