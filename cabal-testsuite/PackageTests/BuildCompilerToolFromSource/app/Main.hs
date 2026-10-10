{-# LANGUAGE CPP #-}
module Main (main) where

import MyLib (answer, libGhcVersion)

main :: IO ()
main = do
  putStrLn ("app: " ++ show answer)
  putStrLn ("app-ghc: " ++ show (__GLASGOW_HASKELL__ :: Int))
  putStrLn ("app-lib-ghc: " ++ show libGhcVersion)
