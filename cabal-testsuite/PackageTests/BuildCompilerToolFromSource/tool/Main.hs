{-# LANGUAGE CPP #-}
module Main (main) where

import MyLib (libGhcVersion)

-- A build tool: it runs on the build machine, so it (and the copy of mylib it
-- links against) must be compiled by the build compiler.
main :: IO ()
main = do
  putStrLn ("tool-ghc: " ++ show (__GLASGOW_HASKELL__ :: Int))
  putStrLn ("tool-lib-ghc: " ++ show libGhcVersion)
