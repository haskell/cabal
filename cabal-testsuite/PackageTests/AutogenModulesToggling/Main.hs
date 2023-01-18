module Main (main) where

import Lib (bar)

main :: IO ()
main = do
  -- Make sure cabal sees this because this test is about which
  -- 'Generated' module the 'Lib' was compiled against.
  putStrLn "-----BEGIN CABAL OUTPUT-----"
  putStrLn bar
  putStrLn "-----END CABAL OUTPUT-----"

