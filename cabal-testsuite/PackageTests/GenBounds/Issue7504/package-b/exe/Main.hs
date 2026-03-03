module Main where

import ModuleB (getEnhancedMessage)

main :: IO ()
main = putStrLn getEnhancedMessage
