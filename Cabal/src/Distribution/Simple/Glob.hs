{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Glob
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- License     :  BSD3
--                portions Copyright (c) 2007, Galois Inc.
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Simple file globbing.
module Distribution.Simple.Glob
  ( -- * Globs
    Glob

    -- * Matching on globs
  , GlobResult (..)
  , globMatches
  , fileGlobMatches
  , matchDirFileGlob
  , matchDirFileGlobWithDie
  , runDirFileGlob

    -- * Parsing globs
  , parseFileGlob
  , GlobSyntaxError (..)
  , explainGlobSyntaxError

    -- * Utility
  , isRecursiveInRoot
  )
where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Simple.Errors
  ( CabalException (MatchDirFileGlob, MatchDirFileGlobErrors)
  )
import Distribution.Simple.Glob.Internal
import Distribution.Simple.Utils (dieWithException, warn)
import Distribution.Utils.Path
import Distribution.Verbosity (Verbosity)

-------------------------------------------------------------------------------

-- * Matching

--------------------------------------------------------------------------------

-- | Extract the matches from a list of 'GlobResult's.
--
-- Note: throws away the 'GlobMissingDirectory' results; chances are
-- that you want to check for these and error out if any are present.
--
-- @since 3.12.0.0
globMatches :: [GlobResult a] -> [a]
globMatches input = [a | GlobMatch a <- input]

-- | This will 'die'' when the glob matches no files, or if the glob
-- refers to a missing directory, or if the glob fails to parse.
--
-- The 'Version' argument must be the spec version of the package
-- description being processed, as globs behave slightly differently
-- in different spec versions.
--
-- The first 'FilePath' argument is the directory that the glob is
-- relative to. It must be a valid directory (and hence it can't be
-- the empty string). The returned values will not include this
-- prefix.
--
-- The second 'FilePath' is the glob itself.
matchDirFileGlob
  :: Verbosity
  -> CabalSpecVersion
  -> Maybe (SymbolicPath CWD (Dir dir))
  -> SymbolicPathX allowAbs dir file
  -> IO [SymbolicPathX allowAbs dir file]
matchDirFileGlob v = matchDirFileGlobWithDie v dieWithException

-- | Like 'matchDirFileGlob' but with customizable 'die'
--
-- @since 3.6.0.0
matchDirFileGlobWithDie
  :: Verbosity
  -> (forall res. Verbosity -> CabalException -> IO [res])
  -> CabalSpecVersion
  -> Maybe (SymbolicPath CWD (Dir dir))
  -> SymbolicPathX allowAbs dir file
  -> IO [SymbolicPathX allowAbs dir file]
matchDirFileGlobWithDie verbosity rip version mbWorkDir symPath =
  let rawFilePath = getSymbolicPath symPath
      dir = maybe "." getSymbolicPath mbWorkDir
   in case parseFileGlob version rawFilePath of
        Left err -> rip verbosity $ MatchDirFileGlob (explainGlobSyntaxError rawFilePath err)
        Right glob -> do
          results <- runDirFileGlob verbosity (Just version) dir glob
          let missingDirectories =
                [missingDir | GlobMissingDirectory missingDir <- results]
              matches = globMatches results
              directoryMatches = [a | GlobMatchesDirectory a <- results]

          let errors :: [String]
              errors =
                [ "filepath wildcard '"
                  ++ rawFilePath
                  ++ "' refers to the directory"
                  ++ " '"
                  ++ missingDir
                  ++ "', which does not exist or is not a directory."
                | missingDir <- missingDirectories
                ]
                  ++ [ "filepath wildcard '" ++ rawFilePath ++ "' does not match any files."
                     | null matches && null directoryMatches
                     -- we don't error out on directory matches, simply warn about them and ignore.
                     ]

              warns :: [String]
              warns =
                [ "Ignoring directory '" ++ path ++ "'" ++ " listed in a Cabal package field which should only include files (not directories)."
                | path <- directoryMatches
                ]

          if null errors
            then do
              unless (null warns) $
                warn verbosity $
                  unlines warns
              return $ map unsafeMakeSymbolicPath matches
            else rip verbosity $ MatchDirFileGlobErrors errors
