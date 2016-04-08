module Main where

import Paths_CopyAssumeDepsUpToDate

main :: IO ()
main = do
    data_fn <- getDataFileName "data"
    putStrLn data_fn -- for debugging
    data_contents <- readFile data_fn
    putStrLn data_contents
