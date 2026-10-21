{-# LANGUAGE CPP #-}
module Main (main) where

main :: IO ()
main = do
  putStrLn ("tool-ghc: " ++ show (__GLASGOW_HASKELL__ :: Int))
  putStrLn ("tool-setup-ghc: " ++ show (SETUP_GHC_VERSION :: Int))
