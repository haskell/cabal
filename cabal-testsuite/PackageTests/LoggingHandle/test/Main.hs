module Main where

import Lib ( libFun )

main :: IO ()
main = putStrLn $ libFun "test-suite"


-- Just something that causes GHC to emit a warning
unusedTest :: Int -> Int
unusedTest 3 = 4
