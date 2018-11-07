{-# LANGUAGE DeriveFunctor #-}
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
        GlobSyntaxError(..),
        GlobResult(..),
        matchDirFileGlob,
        runDirFileGlob,
        fileGlobMatches,
        parseFileGlob,
        explainGlobSyntaxError,
        Glob,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Control.Monad (guard)

import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version

import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (joinPath, splitExtensions, splitDirectories, takeFileName, (</>), (<.>))

-- Note throughout that we use splitDirectories, not splitPath. On
-- Posix, this makes no difference, but, because Windows accepts both
-- slash and backslash as its path separators, if we left in the
-- separators from the glob we might not end up properly normalised.

data GlobResult a
  = GlobMatch a
    -- ^ The glob matched the value supplied.
  | GlobWarnMultiDot a
    -- ^ The glob did not match the value supplied because the
    --   cabal-version is too low and the extensions on the file did
    --   not precisely match the glob's extensions, but rather the
    --   glob was a proper suffix of the file's extensions; i.e., if
    --   not for the low cabal-version, it would have matched.
  | GlobMissingDirectory FilePath
    -- ^ The glob couldn't match because the directory named doesn't
    --   exist. The directory will be as it appears in the glob (i.e.,
    --   relative to the directory passed to 'matchDirFileGlob', and,
    --   for 'data-files', relative to 'data-dir').
  deriving (Show, Eq, Ord, Functor)

-- | Extract the matches from a list of 'GlobResult's.
--
-- Note: throws away the 'GlobMissingDirectory' results; chances are
-- that you want to check for these and error out if any are present.
globMatches :: [GlobResult a] -> [a]
globMatches input = [ a | GlobMatch a <- input ]

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
  ++ "'. Using the double-star syntax requires 'cabal-version: 2.4'"
  ++ " or greater. Alternatively, for compatibility with earlier Cabal"
  ++ " versions, list the included directories explicitly."
explainGlobSyntaxError filepath VersionDoesNotSupportGlob =
     "invalid file glob '" ++ filepath
  ++ "'. Using star wildcards requires 'cabal-version: >= 1.6'. "
  ++ "Alternatively if you require compatibility with earlier Cabal "
  ++ "versions then list all the files explicitly."

data IsRecursive = Recursive | NonRecursive

data MultiDot = MultiDotDisabled | MultiDotEnabled

data Glob
  = GlobStem FilePath Glob
    -- ^ A single subdirectory component + remainder.
  | GlobFinal GlobFinal

data GlobFinal
  = FinalMatch IsRecursive MultiDot String
    -- ^ First argument: Is this a @**/*.ext@ pattern?
    --   Second argument: should we match against the exact extensions, or accept a suffix?
    --   Third argument: the extensions to accept.
  | FinalLit FilePath
    -- ^ Literal file name.

reconstructGlob :: Glob -> FilePath
reconstructGlob (GlobStem dir glob) =
  dir </> reconstructGlob glob
reconstructGlob (GlobFinal final) = case final of
  FinalMatch Recursive _ exts -> "**" </> "*" <.> exts
  FinalMatch NonRecursive _ exts -> "*" <.> exts
  FinalLit path -> path

-- | Returns 'Nothing' if the glob didn't match at all, or 'Just' the
--   result if the glob matched (or would have matched with a higher
--   cabal-version).
fileGlobMatches :: Glob -> FilePath -> Maybe (GlobResult FilePath)
fileGlobMatches pat candidate = do
  match <- fileGlobMatchesSegments pat (splitDirectories candidate)
  return (candidate <$ match)

fileGlobMatchesSegments :: Glob -> [FilePath] -> Maybe (GlobResult ())
fileGlobMatchesSegments _ [] = Nothing
fileGlobMatchesSegments pat (seg : segs) = case pat of
  GlobStem dir pat' -> do
    guard (dir == seg)
    fileGlobMatchesSegments pat' segs
  GlobFinal final -> case final of
    FinalMatch Recursive multidot ext -> do
      let (candidateBase, candidateExts) = splitExtensions (last $ seg:segs)
      guard (not (null candidateBase))
      checkExt multidot ext candidateExts
    FinalMatch NonRecursive multidot ext -> do
      let (candidateBase, candidateExts) = splitExtensions seg
      guard (null segs && not (null candidateBase))
      checkExt multidot ext candidateExts
    FinalLit filename -> do
      guard (null segs && filename == seg)
      return (GlobMatch ())

checkExt
  :: MultiDot
  -> String -- ^ The pattern's extension
  -> String -- ^ The candidate file's extension
  -> Maybe (GlobResult ())
checkExt multidot ext candidate
  | ext == candidate = Just (GlobMatch ())
  | ext `isSuffixOf` candidate = case multidot of
      MultiDotDisabled -> Just (GlobWarnMultiDot ())
      MultiDotEnabled -> Just (GlobMatch ())
  | otherwise = Nothing

parseFileGlob :: Version -> FilePath -> Either GlobSyntaxError Glob
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
        foldM addStem (GlobFinal $ FinalMatch Recursive multidot ext) segments
    | otherwise -> Left VersionDoesNotSupportGlobStar
  (filename : segments) -> do
        pat <- case splitExtensions filename of
          ("*", ext) | not allowGlob       -> Left VersionDoesNotSupportGlob
                     | '*' `elem` ext      -> Left StarInExtension
                     | null ext            -> Left NoExtensionOnStar
                     | otherwise           -> Right (FinalMatch NonRecursive multidot ext)
          (_, ext)   | '*' `elem` ext      -> Left StarInExtension
                     | '*' `elem` filename -> Left StarInFileName
                     | otherwise           -> Right (FinalLit filename)
        foldM addStem (GlobFinal pat) segments
  where
    allowGlob = version >= mkVersion [1,6]
    allowGlobStar = version >= mkVersion [2,4]
    addStem pat seg
      | '*' `elem` seg = Left StarInDirectory
      | otherwise      = Right (GlobStem seg pat)
    multidot
      | version >= mkVersion [2,4] = MultiDotEnabled
      | otherwise = MultiDotDisabled

-- | This will 'die'' when the glob matches no files, or if the glob
-- refers to a missing directory, or if the glob fails to parse.
--
-- The returned values do not include the supplied @dir@ prefix, which
-- must itself be a valid directory (hence, it can't be the empty
-- string).
matchDirFileGlob :: Verbosity -> Version -> FilePath -> FilePath -> IO [FilePath]
matchDirFileGlob verbosity version dir filepath = case parseFileGlob version filepath of
  Left err -> die' verbosity $ explainGlobSyntaxError filepath err
  Right glob -> do
    results <- runDirFileGlob verbosity dir glob
    let missingDirectories =
          [ missingDir | GlobMissingDirectory missingDir <- results ]
        matches = globMatches results
    -- Check for missing directories first, since we'll obviously have
    -- no matches in that case.
    for_ missingDirectories $ \ missingDir ->
      die' verbosity $
           "filepath wildcard '" ++ filepath ++ "' refers to the directory"
        ++ " '" ++ missingDir ++ "', which does not exist or is not a directory."
    when (null matches) $ die' verbosity $
         "filepath wildcard '" ++ filepath
      ++ "' does not match any files."
    return matches

-- | Match files against a pre-parsed glob, starting in a directory.
--
-- The returned values do not include the supplied @dir@ prefix, which
-- must itself be a valid directory (hence, it can't be the empty
-- string).
runDirFileGlob :: Verbosity -> FilePath -> Glob -> IO [GlobResult FilePath]
runDirFileGlob verbosity rawDir pat = do
  -- The default data-dir is null. Our callers -should- be
  -- converting that to '.' themselves, but it's a certainty that
  -- some future call-site will forget and trigger a really
  -- hard-to-debug failure if we don't check for that here.
  when (null rawDir) $
    warn verbosity $
         "Null dir passed to runDirFileGlob; interpreting it "
      ++ "as '.'. This is probably an internal error."
  let dir = if null rawDir then "." else rawDir
  debug verbosity $ "Expanding glob '" ++ reconstructGlob pat ++ "' in directory '" ++ dir ++ "'."
  -- This function might be called from the project root with dir as
  -- ".". Walking the tree starting there involves going into .git/
  -- and dist-newstyle/, which is a lot of work for no reward, so
  -- extract the constant prefix from the pattern and start walking
  -- there, and only walk as much as we need to: recursively if **,
  -- the whole directory if *, and just the specific file if it's a
  -- literal.
  let (prefixSegments, final) = splitConstantPrefix pat
      joinedPrefix = joinPath prefixSegments
  case final of
    FinalMatch recursive multidot exts -> do
      let prefix = dir </> joinedPrefix
      directoryExists <- doesDirectoryExist prefix
      if directoryExists
        then do
          candidates <- case recursive of
            Recursive -> getDirectoryContentsRecursive prefix
            NonRecursive -> filterM (doesFileExist . (prefix </>)) =<< getDirectoryContents prefix
          let checkName candidate = do
                let (candidateBase, candidateExts) = splitExtensions $ takeFileName candidate
                guard (not (null candidateBase))
                match <- checkExt multidot exts candidateExts
                return (joinedPrefix </> candidate <$ match)
          return $ mapMaybe checkName candidates
        else
          return [ GlobMissingDirectory joinedPrefix ]
    FinalLit fn -> do
      exists <- doesFileExist (dir </> joinedPrefix </> fn)
      return [ GlobMatch (joinedPrefix </> fn) | exists ]

unfoldr' :: (a -> Either r (b, a)) -> a -> ([b], r)
unfoldr' f a = case f a of
  Left r -> ([], r)
  Right (b, a') -> case unfoldr' f a' of
    (bs, r) -> (b : bs, r)

-- | Extract the (possibly null) constant prefix from the pattern.
-- This has the property that, if @(pref, final) = splitConstantPrefix pat@,
-- then @pat === foldr GlobStem (GlobFinal final) pref@.
splitConstantPrefix :: Glob -> ([FilePath], GlobFinal)
splitConstantPrefix = unfoldr' step
  where
    step (GlobStem seg pat) = Right (seg, pat)
    step (GlobFinal pat) = Left pat
