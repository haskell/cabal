module Main where

import MyLib (value)

main :: IO ()
main = putStrLn ("tool: " ++ show value)
