module Main where

import           Test.Sanity.Layout.Parse

import           Test.Tasty



main :: IO ()
main =
  defaultMain $
    testGroup "Tests"
      [ parseT
      ]
