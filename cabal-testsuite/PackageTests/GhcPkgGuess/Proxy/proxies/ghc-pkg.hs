module Main where

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args
    | "--version" `elem` args = putStrLn "GHC package manager version 0.0.0"
    | otherwise = error (shows args " not supported")
