{-# LANGUAGE CPP #-}

module Main (main) where

-- The header itself, for the modules that include it, is found too.
#include "cabal_macros.h"

import Versions (versions)

main :: IO ()
main = do
  mapM_ putStrLn versions
#if MIN_VERSION_mhs_macros(1,0,0)
  putStrLn ("library " ++ VERSION_mhs_macros)
#endif
