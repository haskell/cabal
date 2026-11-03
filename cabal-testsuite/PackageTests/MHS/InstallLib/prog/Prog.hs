module Main (main) where

import Triple (triple)

main :: IO ()
main = putStrLn ("triple 14 = " ++ show (triple 14))
