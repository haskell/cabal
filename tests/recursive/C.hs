module Main where
import B
import A -- FIX: GHC doesn't seem to figure out this dependency?!

main :: IO ()
main = let f = g in putStrLn "C"
