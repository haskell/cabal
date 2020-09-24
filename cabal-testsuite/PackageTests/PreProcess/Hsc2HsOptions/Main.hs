{-# LANGUAGE CPP #-}
module Main where

import Foo

bar :: Int
#ifdef TEST_OPTION
bar = TEST_OPTION
#else
bar = 0
#endif

main :: IO ()
main = do
    putStrLn $ "hsc2hs value: " ++ show foo
    putStrLn $ "ghc value: " ++ show bar
