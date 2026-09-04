module Main where

import PkgA (pkgA)
import PkgB (pkgB)

main :: IO ()
main = putStrLn (pkgA ++ pkgB)
