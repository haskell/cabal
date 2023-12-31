module Main where

import Data.Hashable

main :: IO ()
main = print $ hash "foo"
