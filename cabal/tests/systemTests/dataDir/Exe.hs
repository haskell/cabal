module Main where

import Control.Monad (unless)
import Paths_test (getDataFileName)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (putStrLn)

main :: IO ()
main = do
  fname <- getDataFileName "data-file"
  exists <- doesFileExist fname
  if exists
     then return ()
     else do putStrLn "Failure."
             print fname
             exitFailure
