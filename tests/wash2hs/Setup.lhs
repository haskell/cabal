> module Main where

> import Distribution.Simple

> main :: IO ()
> main = defaultMain

to compile: 
ghc -package Cabal -package parsec Setup.lhs -o setup
