#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}
{- project:
import: yops/yops-1.config
import: yops/yops-3.config
import: yops/yops-5.config
import: yops/yops-7.config
import: yops/yops-9.config
-}

main :: IO ()
main = putStrLn "yops project imports"
