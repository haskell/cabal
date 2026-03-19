module Main where

import Distribution.Simple ( defaultMainWithSetupHooks )
import SetupHooks ( setupHooks )

main :: IO ()
main = defaultMainWithSetupHooks setupHooks
