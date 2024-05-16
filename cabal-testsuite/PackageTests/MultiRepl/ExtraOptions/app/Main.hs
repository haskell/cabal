module Main where

import qualified MyLib (someFunc)

#ifdef FOO
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
#endif
