module Main where

import Control.Monad
import Foo
import System.Exit

main :: IO ()
main
  | fooTest [] = do
      replicateM 5 $ putStrLn "The quick brown fox jumps over the lazy dog"
      exitSuccess
  | otherwise = exitFailure
