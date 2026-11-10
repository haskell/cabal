{-# LANGUAGE CPP #-}
module Main (main) where

-- A build tool of a build tool. 'tool' needs it at build time, and 'tool'
-- itself is a build tool of 'app', so this must be compiled by the build
-- compiler too: tools of a build-machine package run on the build machine too.
main :: IO ()
main = putStrLn ("gen-ghc: " ++ show (__GLASGOW_HASKELL__ :: Int))
