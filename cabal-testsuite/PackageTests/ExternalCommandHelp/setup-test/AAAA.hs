module Main where

import System.Environment

main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn "I am helping with the aaaa command"
    _ -> putStrLn "aaaa"
