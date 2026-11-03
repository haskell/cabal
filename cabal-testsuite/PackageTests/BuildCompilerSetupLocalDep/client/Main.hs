{-# LANGUAGE CPP #-}
module Main where

main :: IO ()
main =
  -- HELPER_GHC_VERSION is injected by Setup.hs from the helper library, so
  -- it is the version of the compiler that built the copy of helper the
  -- Setup.hs linked against.
  putStrLn $ "helper-ghc: " ++ show (HELPER_GHC_VERSION :: Int)
