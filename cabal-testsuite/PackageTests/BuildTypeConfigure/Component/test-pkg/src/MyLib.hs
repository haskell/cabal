{-# LANGUAGE CPP #-}
module MyLib (someFunc) where

someFunc :: IO String
someFunc = pure LIB
