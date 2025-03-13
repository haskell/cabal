module Main (main) where

main :: IO ()
main = readFile "do-i-exist.txt" >>= print
