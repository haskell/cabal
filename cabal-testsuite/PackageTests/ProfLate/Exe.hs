module Main where

{-# NOINLINE foo #-}
foo = putStrLn "Hello World!"

main = foo
