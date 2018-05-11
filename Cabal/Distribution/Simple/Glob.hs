{-# LANGUAGE FlexibleContexts #-}
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

module Distribution.Simple.Glob (
        matchFileGlob,
        matchDirFileGlob,
        fileGlobMatches,
        parseFileGlob,
        explainGlobSyntaxError,
        GlobSyntaxError(..),
        GlobPat,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version

import System.FilePath (joinPath, splitExtensions, splitDirectories, takeExtensions, (</>))

-- Note throughout that we use splitDirectories, not splitPath. On
-- Posix, this makes no difference, but, because Windows accepts both
-- slash and backslash as its path separators, if we left in the
-- separators from the glob we might not end up properly normalised.

data GlobSyntaxError
  = StarInDirectory
  | StarInFileName
  | StarInExtension
  | NoExtensionOnStar
  | EmptyGlob
  | LiteralFileNameGlobStar
  | VersionDoesNotSupportGlobStar
  | VersionDoesNotSupportGlob
  deriving (Eq, Show)

explainGlobSyntaxError :: FilePath -> GlobSyntaxError -> String
explainGlobSyntaxError filepath StarInDirectory =
     "invalid file glob '" ++ filepath
  ++ "'. A wildcard '**' is only allowed as the final parent"
  ++ " directory. Stars must not otherwise appear in the parent"
  ++ " directories."
explainGlobSyntaxError filepath StarInExtension =
     "invalid file glob '" ++ filepath
  ++ "'. Wildcards '*' are only allowed as the"
  ++ " file's base name, not in the file extension."
explainGlobSyntaxError filepath StarInFileName =
     "invalid file glob '" ++ filepath
  ++ "'. Wildcards '*' may only totally replace the"
  ++ " file's base name, not only parts of it."
explainGlobSyntaxError filepath NoExtensionOnStar =
     "invalid file glob '" ++ filepath
  ++ "'. If a wildcard '*' is used it must be with an file extension."
explainGlobSyntaxError filepath LiteralFileNameGlobStar =
     "invalid file glob '" ++ filepath
  ++ "'. If a wildcard '**' is used as a parent directory, the"
  ++ " file's base name must be a wildcard '*'."
explainGlobSyntaxError _ EmptyGlob =
     "invalid file glob. A glob cannot be the empty string."
explainGlobSyntaxError filepath VersionDoesNotSupportGlobStar =
     "invalid file glob '" ++ filepath
  ++ "'. Using the double-star syntax requires 'cabal-version: 3.0'"
  ++ " or greater. Alternatively, for compatibility with earlier Cabal"
  ++ " versions, list the included directories explicitly."
explainGlobSyntaxError filepath VersionDoesNotSupportGlob =
     "invalid file glob '" ++ filepath
  ++ "'. Using star wildcards requires 'cabal-version: >= 1.6'. "
  ++ "Alternatively if you require compatibility with earlier Cabal "
  ++ "versions then list all the files explicitly."

data IsRecursive = Recursive | NonRecursive

data GlobPat = PatStem String GlobPat
             -- ^ A single subdirectory component + remainder.
             | PatMatch IsRecursive String
             -- ^ First argument: Is this a @**/*.ext@ pattern?
             --   Second argument: the extensions to accept.
             | PatLit FilePath
             -- ^ Literal file name.

fileGlobMatches :: GlobPat -> FilePath -> Bool
fileGlobMatches pat = fileGlobMatchesSegments pat . splitDirectories

fileGlobMatchesSegments :: GlobPat -> [FilePath] -> Bool
fileGlobMatchesSegments _ [] = False
fileGlobMatchesSegments pat (seg : segs) = case pat of
  PatStem dir pat' ->
    dir == seg && fileGlobMatchesSegments pat' segs
  PatMatch Recursive ext ->
    ext == takeExtensions (last $ seg:segs)
  PatMatch NonRecursive ext ->
    null segs && ext == takeExtensions seg
  PatLit filename ->
    null segs && filename == seg

parseFileGlob :: Version -> FilePath -> Either GlobSyntaxError GlobPat
parseFileGlob version filepath = case reverse (splitDirectories filepath) of
  [] ->
        Left EmptyGlob
  (filename : "**" : segments)
    | allowGlobStar -> do
        ext <- case splitExtensions filename of
          ("*", ext) | '*' `elem` ext -> Left StarInExtension
                     | null ext       -> Left NoExtensionOnStar
                     | otherwise      -> Right ext
          _                           -> Left LiteralFileNameGlobStar
        foldM addStem (PatMatch Recursive ext) segments
    | otherwise -> Left VersionDoesNotSupportGlobStar
  (filename : segments) -> do
        pat <- case splitExtensions filename of
          ("*", ext) | not allowGlob       -> Left VersionDoesNotSupportGlob
                     | '*' `elem` ext      -> Left StarInExtension
                     | null ext            -> Left NoExtensionOnStar
                     | otherwise           -> Right (PatMatch NonRecursive ext)
          (_, ext)   | '*' `elem` ext      -> Left StarInExtension
                     | '*' `elem` filename -> Left StarInFileName
                     | otherwise           -> Right (PatLit filename)
        foldM addStem pat segments
  where
    allowGlob = version >= mkVersion [1,6]
    allowGlobStar = version >= mkVersion [3,0]
    addStem pat seg
      | '*' `elem` seg = Left StarInDirectory
      | otherwise      = Right (PatStem seg pat)

matchFileGlob :: Verbosity -> Version -> FilePath -> IO [FilePath]
matchFileGlob verbosity version = matchDirFileGlob verbosity version "."

-- The returned values do not include the supplied @dir@ prefix.
matchDirFileGlob :: Verbosity -> Version -> FilePath -> FilePath -> IO [FilePath]
matchDirFileGlob verbosity version dir filepath = case parseFileGlob version filepath of
  Left err -> die' verbosity $ explainGlobSyntaxError filepath err
  Right pat -> do
    -- This function might be called from the project root with dir as
    -- ".". Walking the tree starting there involves going into .git/
    -- and dist-newstyle/, which is a lot of work for no reward, so
    -- extract the constant prefix from the pattern and start walking
    -- there. If the pattern is **/*.blah, then of course we'll have
    -- to walk the whole thing anyway, but that's what the user asked
    -- for!
    let (prefixSegments, pat') = splitConstantPrefix pat
        joinedPrefix = joinPath prefixSegments
    files <- getDirectoryContentsRecursive (dir </> joinedPrefix)
    case filter (fileGlobMatches pat') files of
      []      -> die' verbosity $
           "filepath wildcard '" ++ filepath
        ++ "' does not match any files."
      matches -> return $ fmap (joinedPrefix </>) matches

unfoldr' :: (a -> Either r (b, a)) -> a -> ([b], r)
unfoldr' f a = case f a of
  Left r -> ([], r)
  Right (b, a') -> case unfoldr' f a' of
    (bs, r) -> (b : bs, r)

-- | Extract the (possibly null) constant prefix from the pattern.
-- This has the property that, if @(pref, pat') = splitConstantPrefix pat@,
-- then @pat === foldr PatStem pat' pref@.
splitConstantPrefix :: GlobPat -> ([FilePath], GlobPat)
splitConstantPrefix = unfoldr' step
  where
    step (PatStem seg pat) = Right (seg, pat)
    step pat = Left pat
