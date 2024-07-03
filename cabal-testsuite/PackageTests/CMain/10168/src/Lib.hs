module Lib ( myMax ) where

myMax :: Int -> Int -> Int
myMax x1 x2 = if x1 > x2 then x1 else x2

foreign export ccall myMax :: Int -> Int -> Int
