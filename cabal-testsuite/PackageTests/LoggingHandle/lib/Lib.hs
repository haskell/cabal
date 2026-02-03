module Lib ( libFun ) where

libFun :: String -> String
libFun str = "I am the " ++ str ++ "!"


-- Just something that causes GHC to emit a warning
unusedLib :: Int -> Int
unusedLib 3 = 4
