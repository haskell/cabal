module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  let allArgs = unwords args
  if allArgs == "1 2 3 4 5 6"
     then exitSuccess
     else putStrLn ("Got: " ++ allArgs) >> exitFailure
