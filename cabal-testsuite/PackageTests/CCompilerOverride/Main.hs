{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

foreign import ccall "foo" foo :: Int -> Int

main :: IO ()
main = do
  let x = foo 0
      y = x
  let x = y
  print x
  pure ()
