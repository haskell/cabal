{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

foreign import ccall "foo" foo :: Int -> Int

main :: IO ()
main = print (foo 0)
