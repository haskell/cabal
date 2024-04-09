{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

----------------------------------------------------------------------------

----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Program.ResponseFile
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style
--
-- Maintainer  :  cabal-devel@haskell.org
-- Created     :  23 July 2017
module Distribution.Simple.Program.ResponseFile (withResponseFile) where

import System.IO (TextEncoding, hClose, hPutStr, hSetEncoding)
import Prelude ()

import Distribution.Compat.Prelude
import Distribution.Simple.Utils (TempFileOptions, debug, withTempFileEx)
import Distribution.Utils.Path
import Distribution.Verbosity

withResponseFile
  :: Verbosity
  -> TempFileOptions
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ Working directory
  -> SymbolicPath Pkg (Dir Response)
  -- ^ Directory to create response file in.
  -> String
  -- ^ Template for response file name.
  -> Maybe TextEncoding
  -- ^ Encoding to use for response file contents.
  -> [String]
  -- ^ Arguments to put into response file.
  -> (FilePath -> IO a)
  -> IO a
withResponseFile verbosity tmpFileOpts mbWorkDir responseDir fileNameTemplate encoding arguments f =
  withTempFileEx tmpFileOpts mbWorkDir responseDir fileNameTemplate $ \responsePath hf -> do
    let responseFileName = getSymbolicPath responsePath
    traverse_ (hSetEncoding hf) encoding
    let responseContents =
          unlines $
            map escapeResponseFileArg $
              arguments
    hPutStr hf responseContents
    hClose hf
    debug verbosity $ responseFileName ++ " contents: <<<"
    debug verbosity responseContents
    debug verbosity $ ">>> " ++ responseFileName
    f responseFileName

-- Support a gcc-like response file syntax.  Each separate
-- argument and its possible parameter(s), will be separated in the
-- response file by an actual newline; all other whitespace,
-- single quotes, double quotes, and the character used for escaping
-- (backslash) are escaped.  The called program will need to do a similar
-- inverse operation to de-escape and re-constitute the argument list.
escapeResponseFileArg :: String -> String
escapeResponseFileArg = reverse . foldl' escape []
  where
    escape :: String -> Char -> String
    escape cs c =
      case c of
        '\\' -> c : '\\' : cs
        '\'' -> c : '\\' : cs
        '"' -> c : '\\' : cs
        _
          | isSpace c -> c : '\\' : cs
          | otherwise -> c : cs
