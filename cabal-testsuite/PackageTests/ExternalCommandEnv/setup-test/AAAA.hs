module Main where

import System.Environment
import System.Process

main = do
  cabal_proc <- getEnv "CABAL"
  other_var <- getEnv "OTHER_VAR"
  putStrLn ("OTHER_VAR is set to: " ++ other_var)
  callProcess cabal_proc ["--version"]

