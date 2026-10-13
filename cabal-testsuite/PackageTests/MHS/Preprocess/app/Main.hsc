module Main (main) where

import Sizes (incr, intSize)

#include <limits.h>

-- | The bits of a C char, filled in by hsc2hs.
charBits :: Int
charBits = #const CHAR_BIT

main :: IO ()
main = do
  putStrLn ("sizeof int = " ++ show intSize)
  putStrLn ("char bits = " ++ show charBits)
  putStrLn ("incr 41 = " ++ show (incr 41))
