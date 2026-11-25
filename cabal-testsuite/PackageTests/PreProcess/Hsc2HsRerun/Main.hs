module Main where

import Foo

main :: IO ()
main = putStrLn $ "hsc2hs value: " ++ show foo
