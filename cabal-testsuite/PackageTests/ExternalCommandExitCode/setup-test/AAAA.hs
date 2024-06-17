module Main where

import System.Environment
import System.Exit

main = do
  getArgs >>= print
  exitWith (ExitFailure 99)
