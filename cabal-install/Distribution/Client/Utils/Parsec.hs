module Distribution.Client.Utils.Parsec (
    renderParseError,
    ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

import Distribution.Parsec.Common
       (PError (..), PWarning (..), Position (..), showPError, showPWarning, zeroPos)
import Distribution.Simple.Utils  (fromUTF8BS)

-- | Render parse error highlighting the part of the input file.
renderParseError
    :: FilePath
    -> BS.ByteString
    -> [PError]
    -> [PWarning]
    -> String
renderParseError filepath contents errors warnings = unlines $
    [ "Errors encountered when parsing cabal file " <> filepath <> ":"
    ]
    ++ renderedErrors
    ++ renderedWarnings
  where
    -- lines of the input file.
    ls = BS8.lines contents

    nths :: Int -> [a] -> [a]
    nths n | n <= 0 = take 2
    nths n = take 3 . drop (n - 1)

    -- empty line before each error and warning
    renderedErrors   = concatMap (prepend . renderError) errors
    renderedWarnings = concatMap (prepend . renderWarning) warnings

    prepend = ("" :)

    renderError :: PError -> [String]
    renderError e@(PError pos@(Position row _col) _)
        -- if position is 0:0, then it doesn't make sense to show input
        -- looks like, Parsec errors have line-feed in them
        | pos == zeroPos = [trim $ showPError filepath e]
        | otherwise      = [trim $ showPError filepath e, ""] ++
            zipWith formatInputLine (nths (row - 1) ls) [row - 1 ..]

    -- sometimes there are (especially trailing) newlines.
    trim = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse

    renderWarning :: PWarning -> [String]
    renderWarning w@(PWarning _ pos@(Position row _col) _)
        | pos == zeroPos = [showPWarning filepath w]
        | otherwise      = [showPWarning filepath w, ""] ++
            zipWith formatInputLine (nths (row - 1) ls) [row - 1 ..]

    -- format line: prepend the given line number
    formatInputLine :: BS.ByteString -> Int -> String
    formatInputLine bs l =
        showN l ++ " | " ++ fromUTF8BS bs

    -- hopefully we don't need to work with over 99999 lines .cabal files
    -- at that point small glitches in error messages are hopefully fine.
    showN n = let s = show n in replicate (5 - length s) ' ' ++ s
