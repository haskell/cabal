module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  if args == ["1", "2 3", "4", "5 6"]
     then exitSuccess
     else putStrLn ("Got: " ++ show args) >> exitFailure
