#! /usr/bin/env cabal
{- cabal:
build-depends: base >= 4.3 && <5
other-modules: A
-}

import A (helloworld)

main :: IO ()
main = putStrLn helloworld
