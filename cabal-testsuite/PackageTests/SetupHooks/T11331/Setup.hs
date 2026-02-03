{-# LANGUAGE NoImplicitPrelude #-}

module Main where

-- Cabal
import Distribution.Simple ( defaultMainWithSetupHooks )

-- T11331
import SetupHooks ( setupHooks )

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithSetupHooks setupHooks
