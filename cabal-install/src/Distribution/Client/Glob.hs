{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Glob
  ( -- * cabal-install globbing features
    RootedGlob (..)
  , isTrivialRootedGlob
  , FilePathRoot (..)
  , getFilePathRootDirectory

    -- * Additional re-exports
  , module Distribution.Simple.Glob
  , Glob (..)
  , GlobPiece (..)
  , GlobPieces
  , matchFileGlob
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.FileMonitor.Types
import Distribution.Simple.Glob
import Distribution.Simple.Glob.Internal
  ( Glob (..)
  , GlobPiece (..)
  , GlobPieces
  )

import System.Directory
import System.FilePath

--------------------------------------------------------------------------------

-- | Check if a 'RootedGlob' doesn't actually make use of any globbing and
-- is in fact equivalent to a non-glob 'FilePath'.
--
-- If it is trivial in this sense then the result is the equivalent constant
-- 'FilePath'. On the other hand, if it is not trivial (so could in principle
-- match more than one file), then the result is @Nothing@.
isTrivialRootedGlob :: RootedGlob -> Maybe FilePath
isTrivialRootedGlob (RootedGlob root pathglob) =
  case root of
    FilePathRelative -> go [] pathglob
    FilePathRoot root' -> go [root'] pathglob
    -- TODO: why don't we do the following?
    -- > go ["~"] pathglob
    FilePathHomeDir -> Nothing
  where
    go paths (GlobDir [Literal path] globs) = go (path : paths) globs
    go paths (GlobFile [Literal path]) = Just (joinPath (reverse (path : paths)))
    go paths GlobDirTrailing =
      Just
        ( addTrailingPathSeparator
            (joinPath (reverse paths))
        )
    go _ _ = Nothing

-- | Get the 'FilePath' corresponding to a 'FilePathRoot'.
--
-- The 'FilePath' argument is required to supply the path for the
-- 'FilePathRelative' case.
getFilePathRootDirectory
  :: FilePathRoot
  -> FilePath
  -- ^ root for relative paths
  -> IO FilePath
getFilePathRootDirectory FilePathRelative root = return root
getFilePathRootDirectory (FilePathRoot root) _ = return root
getFilePathRootDirectory FilePathHomeDir _ = getHomeDirectory

------------------------------------------------------------------------------
-- Matching
--

-- | Match a 'RootedGlob' against the file system, starting from a given
-- root directory for relative paths. The results of relative globs are
-- relative to the given root. Matches for absolute globs are absolute.
matchFileGlob :: FilePath -> RootedGlob -> IO [FilePath]
matchFileGlob relroot (RootedGlob globroot glob) = do
  root <- getFilePathRootDirectory globroot relroot
  matches <- matchGlob root glob
  case globroot of
    FilePathRelative -> return matches
    _ -> return (map (root </>) matches)
