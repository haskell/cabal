{-# LANGUAGE CPP #-}
module Main (main) where

main :: IO ()
main = putStrLn ("app-ghc: " ++ show (__GLASGOW_HASKELL__ :: Int))
