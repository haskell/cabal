module Main where

import qualified Dynamic (number)

main :: IO ()
main = do
    putStrLn $ "Dynamic's number is " ++ (show Dynamic.number) ++ "."
