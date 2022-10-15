module Main where

import PackageInfo_PackageInfoModule (version)

main :: IO ()
main = do
  print version
