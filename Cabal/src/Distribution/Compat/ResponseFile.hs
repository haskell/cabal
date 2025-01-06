{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- Compatibility layer for GHC.ResponseFile
-- Implementation from base 4.12.0 is used.
-- http://hackage.haskell.org/package/base-4.12.0.0/src/LICENSE
module Distribution.Compat.ResponseFile (expandResponse, escapeArgs) where

import Distribution.Compat.Prelude

import GHC.ResponseFile (escapeArgs, unescapeArgs)

import Prelude ()

import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.IO.Error

-- | The arg file / response file parser.
--
-- This is not a well-documented capability, and is a bit eccentric
-- (try @cabal \@foo \@bar@ to see what that does), but is crucial
-- for allowing complex arguments to cabal and cabal-install when
-- using command prompts with strongly-limited argument length.
expandResponse :: [String] -> IO [String]
expandResponse = go recursionLimit "."
  where
    recursionLimit = 100

    go :: Int -> FilePath -> [String] -> IO [String]
    go n dir
      | n >= 0 = fmap concat . traverse (expand n dir)
      | otherwise = const $ hPutStrLn stderr "Error: response file recursion limit exceeded." >> exitFailure

    expand :: Int -> FilePath -> String -> IO [String]
    expand n dir arg@('@' : f) = readRecursively n (dir </> f) `catchIOError` const (print "?" >> return [arg])
    expand _n _dir x = return [x]

    readRecursively :: Int -> FilePath -> IO [String]
    readRecursively n f = go (n - 1) (takeDirectory f) =<< unescapeArgs <$> readFile f
