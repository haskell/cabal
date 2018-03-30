module SetupLib (printExampleTxt) where

import Paths_setup_lib

printExampleTxt :: IO ()
printExampleTxt = do
  ex <- getDataFileName "example.txt"
  exContents <- readFile ex
  putStrLn exContents
