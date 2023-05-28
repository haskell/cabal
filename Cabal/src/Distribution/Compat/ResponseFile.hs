{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- Compatibility layer for GHC.ResponseFile
-- Implementation from base 4.12.0 is used.
-- http://hackage.haskell.org/package/base-4.12.0.0/src/LICENSE
module Distribution.Compat.ResponseFile (expandResponse, escapeArgs) where

import Distribution.Compat.Prelude
import Prelude ()

import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.IO.Error

#if MIN_VERSION_base(4,12,0)
import GHC.ResponseFile (unescapeArgs, escapeArgs)
#else

unescapeArgs :: String -> [String]
unescapeArgs = filter (not . null) . unescape

data Quoting = NoneQ | SngQ | DblQ

unescape :: String -> [String]
unescape args = reverse . map reverse $ go args NoneQ False [] []
    where
      -- n.b., the order of these cases matters; these are cribbed from gcc
      -- case 1: end of input
      go []     _q    _bs   a as = a:as
      -- case 2: back-slash escape in progress
      go (c:cs) q     True  a as = go cs q     False (c:a) as
      -- case 3: no back-slash escape in progress, but got a back-slash
      go (c:cs) q     False a as
        | '\\' == c              = go cs q     True  a     as
      -- case 4: single-quote escaping in progress
      go (c:cs) SngQ  False a as
        | '\'' == c              = go cs NoneQ False a     as
        | otherwise              = go cs SngQ  False (c:a) as
      -- case 5: double-quote escaping in progress
      go (c:cs) DblQ  False a as
        | '"' == c               = go cs NoneQ False a     as
        | otherwise              = go cs DblQ  False (c:a) as
      -- case 6: no escaping is in progress
      go (c:cs) NoneQ False a as
        | isSpace c              = go cs NoneQ False []    (a:as)
        | '\'' == c              = go cs SngQ  False a     as
        | '"'  == c              = go cs DblQ  False a     as
        | otherwise              = go cs NoneQ False (c:a) as

escapeArgs :: [String] -> String
escapeArgs = unlines . map escapeArg

escapeArg :: String -> String
escapeArg = reverse . foldl' escape []

escape :: String -> Char -> String
escape cs c
  |    isSpace c
    || '\\' == c
    || '\'' == c
    || '"'  == c = c:'\\':cs -- n.b., our caller must reverse the result
  | otherwise    = c:cs

#endif

expandResponse :: [String] -> IO [String]
expandResponse = go recursionLimit "."
  where
    recursionLimit = 100

    go :: Int -> FilePath -> [String] -> IO [String]
    go n dir
      | n >= 0 = fmap concat . traverse (expand n dir)
      | otherwise = const $ hPutStrLn stderr "Error: response file recursion limit exceeded." >> exitFailure

    expand :: Int -> FilePath -> String -> IO [String]
    expand n dir arg@('@' : f) = readRecursively n (dir </> f) `catchIOError` (const $ print "?" >> return [arg])
    expand _n _dir x = return [x]

    readRecursively :: Int -> FilePath -> IO [String]
    readRecursively n f = go (n - 1) (takeDirectory f) =<< unescapeArgs <$> readFile f
