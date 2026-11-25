module Main where

import Test.Tasty (defaultMain, testGroup)
import Tests.ParserTests (packageTestsProjectTests, parserTests)

main :: IO ()
main = do
  packageTests <- packageTestsProjectTests
  defaultMain $ testGroup "parser-tests" [parserTests, packageTests]
