module Main (main) where

import qualified DepClang
import qualified DepZ

main :: IO ()
main = do
  DepClang.hello
  DepZ.hello
