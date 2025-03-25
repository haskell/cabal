module Main where

import Data.List (group, sort)
import System.Environment (getEnvironment)

main = do
    env <- getEnvironment
    let sortedEnv = sort env
        duplicates = filter (\g -> length g > 1) $ group $ map fst sortedEnv

    if null duplicates
        then putStrLn "No duplicate environment variables found."
        else do
            putStrLn "Found duplicate environment variables:"
            mapM_ (\d -> putStrLn $ "  - " ++ head d) duplicates
            fail "Test failed due to duplicate environment variables"
