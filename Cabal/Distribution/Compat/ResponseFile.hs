{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,12,0)
{-# LANGUAGE ScopedTypeVariables #-}
#endif

-- Compatibility layer for GHC.ResponseFile
-- Implementation from base 4.12.0 is used.
module Distribution.Compat.ResponseFile (expandResponse) where

#if MIN_VERSION_base(4,12,0)
import GHC.ResponseFile (expandResponse)
#else

import Control.Exception
import System.Exit       (exitFailure)
import System.IO
import Data.Char         (isSpace)

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

expandResponse :: [String] -> IO [String]
expandResponse = fmap concat . mapM expand
  where
    expand :: String -> IO [String]
    expand ('@':f) = readFileExc f >>= return . unescapeArgs
    expand x = return [x]

    readFileExc f =
      readFile f `catch` \(e :: IOException) -> do
        hPutStrLn stderr $ "Error while expanding response file: " ++ show e
        exitFailure

#endif
