{-# LANGUAGE DataKinds #-}

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
  , flattenDups
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Data.ByteString as BS
import Data.List (groupBy)
import Distribution.Fields.ParseResult
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription
  , parseHookedBuildInfo
  )
import Distribution.Parsec.Error (showPErrorWithSource)
import Distribution.Parsec.Source
import Distribution.Parsec.Warning
  ( PWarnType (PWTExperimental)
  , PWarning (..)
  , PWarningWithSource (..)
  , showPWarningWithSource
  )
import Distribution.Simple.Errors
import Distribution.Simple.Utils (dieWithException, equating, warn)
import Distribution.Utils.Path
import Distribution.Verbosity (Verbosity, VerbosityLevel (..), verbosityLevel)
import System.Directory (doesFileExist)
import Text.Printf (printf)

readGenericPackageDescription
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg File
  -> IO GenericPackageDescription
readGenericPackageDescription =
  readAndParseFile parseGenericPackageDescription

readHookedBuildInfo
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ working directory
  -> SymbolicPath Pkg File
  -> IO HookedBuildInfo
readHookedBuildInfo =
  readAndParseFile parseHookedBuildInfo

-- | Helper combinator to do parsing plumbing for files.
--
-- Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
--
-- Argument order is chosen to encourage partial application.
readAndParseFile
  :: (BS.ByteString -> ParseResult CabalFileSource a)
  -- ^ File contents to final value parser
  -> Verbosity
  -- ^ Verbosity level
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ Working directory
  -> SymbolicPath Pkg File
  -- ^ File to read
  -> IO a
readAndParseFile parser verbosity mbWorkDir fpath = do
  let ipath = interpretSymbolicPath mbWorkDir fpath
      upath = getSymbolicPath fpath
  exists <- doesFileExist ipath
  unless exists $
    dieWithException verbosity $
      ErrorParsingFileDoesntExist upath
  bs <- BS.readFile ipath
  parseString parser verbosity upath bs

parseString
  :: (BS.ByteString -> ParseResult CabalFileSource a)
  -- ^ File contents to final value parser
  -> Verbosity
  -- ^ Verbosity level
  -> String
  -- ^ File name
  -> BS.ByteString
  -> IO a
parseString parser verbosity name bs = do
  let (warnings, result) = runParseResult $ withSource (PCabalFile (name, bs)) (parser bs)
  traverse_ (warn verbosity . showPWarningWithSource . fmap renderCabalFileSource) (flattenDups verbosity warnings)
  case result of
    Right x -> return x
    Left (_, errors) -> do
      traverse_ (warn verbosity . showPErrorWithSource . fmap renderCabalFileSource) errors
      dieWithException verbosity $ FailedParsing name

-- | Collapse duplicate experimental feature warnings into single warning, with
-- a count of further sites
flattenDups :: Verbosity -> [PWarningWithSource src] -> [PWarningWithSource src]
flattenDups verbosity ws
  | verbosityLevel verbosity <= Normal = rest ++ experimentals
  | otherwise = ws -- show all instances
  where
    (exps, rest) = partition (\(PWarningWithSource _ (PWarning w _ _)) -> w == PWTExperimental) ws
    experimentals =
      concatMap flatCount
        . groupBy (equating (warningStr . pwarning))
        . sortBy (comparing (warningStr . pwarning))
        $ exps

    warningStr (PWarning _ _ w) = w

    -- flatten if we have 3 or more examples
    flatCount :: [PWarningWithSource src] -> [PWarningWithSource src]
    flatCount w@[] = w
    flatCount w@[_] = w
    flatCount w@[_, _] = w
    flatCount (PWarningWithSource source (PWarning t pos w) : xs) =
      [ PWarningWithSource
          source
          ( PWarning
              t
              pos
              (w <> printf " (and %d more occurrences)" (length xs))
          )
      ]
