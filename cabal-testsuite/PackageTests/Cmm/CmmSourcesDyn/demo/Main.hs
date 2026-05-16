{-# LANGUAGE CPP #-}
module Main (main) where

-- Qualified due to https://gitlab.haskell.org/ghc/ghc/-/issues/19397
--
import qualified Demo (main)

main :: IO ()
main = Demo.main
