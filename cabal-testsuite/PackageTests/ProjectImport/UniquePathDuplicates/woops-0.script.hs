#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}
{- project:
import: woops/woops-1.config
import: woops/woops-3.config
import: woops/woops-5.config
import: woops/woops-7.config
import: woops/woops-9.config
-}

main :: IO ()
main = putStrLn "woops project imports"
