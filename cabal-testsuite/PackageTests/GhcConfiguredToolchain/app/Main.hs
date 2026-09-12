module Main (main) where

import Lib (c_value)

main :: IO ()
main = c_value >>= print
