module Main where

import Lib ( libFun )

main :: IO ()
main = putStrLn $ libFun "benchmark"


-- Just something that causes GHC to emit a warning
unusedBench :: Int -> Int
unusedBench 3 = 4
