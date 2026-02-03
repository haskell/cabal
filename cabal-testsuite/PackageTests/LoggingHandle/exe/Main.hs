module Main where

import Lib ( libFun )

main :: IO ()
main = putStrLn $ libFun "executable"


-- Just something that causes GHC to emit a warning
unusedExe :: Int -> Int
unusedExe 3 = 4
