module Main where

import Control.Monad
import Foo
import System.Exit

main :: IO ()
main
  | fooTest [] = do
      -- Make sure that the output buffer is drained
      replicateM 10000 $ putStrLn "The quick brown fox jumps over the lazy dog"
      exitSuccess
  | otherwise = exitFailure
