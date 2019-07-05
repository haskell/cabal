#! /usr/bin/env cabal
{- cabal:
build-depends: base >= 4.3 && <5
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude

main :: IO ()
main = putStrLn "Hello World"
