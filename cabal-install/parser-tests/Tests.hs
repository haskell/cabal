module Main where

import Test.Tasty (defaultMain)
import Tests.ParserTests (parserTests)

main :: IO ()
main = defaultMain parserTests
