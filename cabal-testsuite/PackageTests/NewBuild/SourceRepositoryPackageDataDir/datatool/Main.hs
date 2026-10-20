module Main (main) where

import App (app)
import Paths_datatool (getDataFileName)

main :: IO ()
main = do
  greeting <- getDataFileName "greeting.txt" >>= readFile
  putStr greeting
  print app
