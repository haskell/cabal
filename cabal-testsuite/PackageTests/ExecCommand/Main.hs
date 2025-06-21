
module Main (main) where

import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--required", "foo"] -> pure ()
    other -> die $ "Unexpected arguments: " ++ show other
