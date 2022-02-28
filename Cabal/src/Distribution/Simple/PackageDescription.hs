-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.PackageDescription
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines parsers for the @.cabal@ format

module Distribution.Simple.PackageDescription (
    -- * Read and Parse files
    readGenericPackageDescription,
    readHookedBuildInfo,

    -- * Utility Parsing function
    parseString,
    ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Fields.ParseResult
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Parsec.Error    (showPError)
import Distribution.Parsec.Warning  (showPWarning)
import Distribution.Simple.Utils
import Distribution.Verbosity

import qualified Data.ByteString    as BS
import System.Directory (doesFileExist)

readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readAndParseFile parseGenericPackageDescription

readHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
readHookedBuildInfo = readAndParseFile parseHookedBuildInfo

-- | Helper combinator to do parsing plumbing for files.
--
-- Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
--
-- Argument order is chosen to encourage partial application.
readAndParseFile
    :: (BS.ByteString -> ParseResult a)  -- ^ File contents to final value parser
    -> Verbosity                         -- ^ Verbosity level
    -> FilePath                          -- ^ File to read
    -> IO a
readAndParseFile parser verbosity fpath = do
    exists <- doesFileExist fpath
    unless exists $
      die' verbosity $
        "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
    bs <- BS.readFile fpath
    parseString parser verbosity fpath bs

parseString
    :: (BS.ByteString -> ParseResult a)  -- ^ File contents to final value parser
    -> Verbosity                         -- ^ Verbosity level
    -> String                            -- ^ File name
    -> BS.ByteString
    -> IO a
parseString parser verbosity name bs = do
    let (warnings, result) = runParseResult (parser bs)
    traverse_ (warn verbosity . showPWarning name) warnings
    case result of
        Right x -> return x
        Left (_, errors) -> do
            traverse_ (warn verbosity . showPError name) errors
            die' verbosity $ "Failed parsing \"" ++ name ++ "\"."

