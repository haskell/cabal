{-# LANGUAGE CPP #-}
module Main where

main :: IO ()
main = do
  -- SETUP_GHC_VERSION is injected by Setup.hs using the build compiler's
  -- __GLASGOW_HASKELL__ macro, so it reflects the build-stage GHC.
  putStrLn $ "setup-ghc: " ++ show (SETUP_GHC_VERSION :: Int)
  -- __GLASGOW_HASKELL__ here reflects the host compiler (the one compiling Main.hs).
  putStrLn $ "self-ghc: " ++ show (__GLASGOW_HASKELL__ :: Int)
