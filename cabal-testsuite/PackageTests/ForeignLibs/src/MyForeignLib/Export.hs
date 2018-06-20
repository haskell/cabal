{-# LANGUAGE ForeignFunctionInterface #-}
module MyForeignLib.Export
  ( foo ) where

foo :: Int -> Int
foo x = x + 1

foreign export ccall foo :: Int -> Int
