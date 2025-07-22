module Main (main) where

import Foo

main :: IO ()
main = putStrLn $ "Result = " ++ show foo
