module Main where

import MyLib (value)

main :: IO ()
main = putStrLn ("app: " ++ show value)
