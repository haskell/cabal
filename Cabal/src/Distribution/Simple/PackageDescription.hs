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
module Distribution.Simple.PackageDescription
  ( -- * Read and Parse files
    readGenericPackageDescription
  , readHookedBuildInfo

    -- * Utility Parsing function
  , parseString
  , readAndParseFile
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Fields.ParseResult
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription
  , parseHookedBuildInfo
  )
import Distribution.Parsec.Error (showPError)
import Distribution.Parsec.Warning
  ( PWarnType (PWTExperimental)
  , PWarning (..)
  , showPWarning
  )
import Distribution.Simple.Utils (die', equating, warn)
import Distribution.Verbosity (Verbosity, normal)

import qualified Data.ByteString as BS
import Data.List (groupBy)
import System.Directory (doesFileExist)
import Text.Printf (printf)

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
  :: (BS.ByteString -> ParseResult a)
  -- ^ File contents to final value parser
  -> Verbosity
  -- ^ Verbosity level
  -> FilePath
  -- ^ File to read
  -> IO a
readAndParseFile parser verbosity fpath = do
  exists <- doesFileExist fpath
  unless exists $
    die' verbosity $
      "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
  bs <- BS.readFile fpath
  parseString parser verbosity fpath bs

parseString
  :: (BS.ByteString -> ParseResult a)
  -- ^ File contents to final value parser
  -> Verbosity
  -- ^ Verbosity level
  -> String
  -- ^ File name
  -> BS.ByteString
  -> IO a
parseString parser verbosity name bs = do
  let (warnings, result) = runParseResult (parser bs)
  traverse_ (warn verbosity . showPWarning name) (flattenDups verbosity warnings)
  case result of
    Right x -> return x
    Left (_, errors) -> do
      traverse_ (warn verbosity . showPError name) errors
      die' verbosity $ "Failed parsing \"" ++ name ++ "\"."

-- | Collapse duplicate experimental feature warnings into single warning, with
-- a count of further sites
flattenDups :: Verbosity -> [PWarning] -> [PWarning]
flattenDups verbosity ws
  | verbosity <= normal = rest ++ experimentals
  | otherwise = ws -- show all instances
  where
    (exps, rest) = partition (\(PWarning w _ _) -> w == PWTExperimental) ws
    experimentals =
      concatMap flatCount
        . groupBy (equating warningStr)
        . sortBy (comparing warningStr)
        $ exps

    warningStr (PWarning _ _ w) = w

    -- flatten if we have 3 or more examples
    flatCount :: [PWarning] -> [PWarning]
    flatCount w@[] = w
    flatCount w@[_] = w
    flatCount w@[_, _] = w
    flatCount (PWarning t pos w : xs) =
      [ PWarning
          t
          pos
          (w <> printf " (and %d more occurrences)" (length xs))
      ]
