module Main where

import HUnit

main :: IO ()
main = do runTestTT $ TestCase $ assertBool "foo!" True
          putStrLn "Works :)"
