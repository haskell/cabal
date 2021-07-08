-- Setup.hs taken from 'cabal-testsuite/Setup.hs'
{-# LANGUAGE Haskell2010 #-}
module Main (main) where

import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { buildHook = \pkg lbi hooks flags -> do
        putStrLn "Custom Setup.hs has been invoked!"
        buildHook simpleUserHooks pkg lbi hooks flags
    }
