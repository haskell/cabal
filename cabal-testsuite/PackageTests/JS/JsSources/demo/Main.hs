module Main where

foreign import javascript "foo" :: IO ()

main :: IO ()
main = foo
