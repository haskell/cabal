-- Simple general-purpose Cabal setup script

module Main (main) where

import Distribution.Simple (defaultMainWithHooks, defaultUserHooks)

main :: IO ()
main = defaultMainWithHooks defaultUserHooks
