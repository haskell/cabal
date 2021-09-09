module Main (main) where

main :: IO ()
-- Intentional typo, should fail to compile
main = putStrn "Test suite not yet implemented."
--          ^^------- missing 'L'
