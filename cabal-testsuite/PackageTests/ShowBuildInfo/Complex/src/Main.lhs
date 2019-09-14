module Main where

import Lib

main :: IO ()
main = do
    let i = foo 5
    putStrLn "Hello, Haskell!"
    print i
