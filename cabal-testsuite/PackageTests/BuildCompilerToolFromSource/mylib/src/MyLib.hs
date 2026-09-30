{-# LANGUAGE CPP #-}
module MyLib (libGhcVersion, answer) where

-- | The __GLASGOW_HASKELL__ of the compiler that compiled this copy of the
-- library. Under cross-compilation the library is built twice, once per
-- stage, and each copy reports its own compiler.
libGhcVersion :: Int
libGhcVersion = __GLASGOW_HASKELL__

answer :: Int
answer = 42
