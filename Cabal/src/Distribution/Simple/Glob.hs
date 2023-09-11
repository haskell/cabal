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
module Distribution.Simple.Glob
  ( GlobSyntaxError (..)
  , GlobResult (..)
  , matchDirFileGlob
  , matchDirFileGlobWithDie
  , runDirFileGlob
  , fileGlobMatches
  , parseFileGlob
  , explainGlobSyntaxError
  , isRecursiveInRoot
  , Glob
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Simple.Utils
import Distribution.Verbosity

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (joinPath, splitDirectories, splitExtensions, takeFileName, (<.>), (</>))

import qualified Data.List.NonEmpty as NE
import Distribution.Simple.Errors

-- Note throughout that we use splitDirectories, not splitPath. On
-- Posix, this makes no difference, but, because Windows accepts both
-- slash and backslash as its path separators, if we left in the
-- separators from the glob we might not end up properly normalised.

data GlobResult a
  = -- | The glob matched the value supplied.
    GlobMatch a
  | -- | The glob did not match the value supplied because the
    --   cabal-version is too low and the extensions on the file did
    --   not precisely match the glob's extensions, but rather the
    --   glob was a proper suffix of the file's extensions; i.e., if
    --   not for the low cabal-version, it would have matched.
    GlobWarnMultiDot a
  | -- | The glob couldn't match because the directory named doesn't
    --   exist. The directory will be as it appears in the glob (i.e.,
    --   relative to the directory passed to 'matchDirFileGlob', and,
    --   for 'data-files', relative to 'data-dir').
    GlobMissingDirectory FilePath
  deriving (Show, Eq, Ord, Functor)

-- | Extract the matches from a list of 'GlobResult's.
--
-- Note: throws away the 'GlobMissingDirectory' results; chances are
-- that you want to check for these and error out if any are present.
globMatches :: [GlobResult a] -> [a]
globMatches input = [a | GlobMatch a <- input]

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
  "invalid file glob '"
    ++ filepath
    ++ "'. A wildcard '**' is only allowed as the final parent"
    ++ " directory. Stars must not otherwise appear in the parent"
    ++ " directories."
explainGlobSyntaxError filepath StarInExtension =
  "invalid file glob '"
    ++ filepath
    ++ "'. Wildcards '*' are only allowed as the"
    ++ " file's base name, not in the file extension."
explainGlobSyntaxError filepath StarInFileName =
  "invalid file glob '"
    ++ filepath
    ++ "'. Wildcards '*' may only totally replace the"
    ++ " file's base name, not only parts of it."
explainGlobSyntaxError filepath NoExtensionOnStar =
  "invalid file glob '"
    ++ filepath
    ++ "'. If a wildcard '*' is used it must be with an file extension."
explainGlobSyntaxError filepath LiteralFileNameGlobStar =
  "invalid file glob '"
    ++ filepath
    ++ "'. Prior to 'cabal-version: 3.8'"
    ++ " if a wildcard '**' is used as a parent directory, the"
    ++ " file's base name must be a wildcard '*'."
explainGlobSyntaxError _ EmptyGlob =
  "invalid file glob. A glob cannot be the empty string."
explainGlobSyntaxError filepath VersionDoesNotSupportGlobStar =
  "invalid file glob '"
    ++ filepath
    ++ "'. Using the double-star syntax requires 'cabal-version: 2.4'"
    ++ " or greater. Alternatively, for compatibility with earlier Cabal"
    ++ " versions, list the included directories explicitly."
explainGlobSyntaxError filepath VersionDoesNotSupportGlob =
  "invalid file glob '"
    ++ filepath
    ++ "'. Using star wildcards requires 'cabal-version: >= 1.6'. "
    ++ "Alternatively if you require compatibility with earlier Cabal "
    ++ "versions then list all the files explicitly."

data IsRecursive = Recursive | NonRecursive deriving (Eq)

data MultiDot = MultiDotDisabled | MultiDotEnabled

data Glob
  = -- | A single subdirectory component + remainder.
    GlobStem FilePath Glob
  | GlobFinal GlobFinal

data GlobFinal
  = -- | First argument: Is this a @**/*.ext@ pattern?
    --   Second argument: should we match against the exact extensions, or accept a suffix?
    --   Third argument: the extensions to accept.
    FinalMatch IsRecursive MultiDot String
  | -- | Literal file name.
    FinalLit IsRecursive FilePath

reconstructGlob :: Glob -> FilePath
reconstructGlob (GlobStem dir glob) =
  dir </> reconstructGlob glob
reconstructGlob (GlobFinal final) = case final of
  FinalMatch Recursive _ exts -> "**" </> "*" <.> exts
  FinalMatch NonRecursive _ exts -> "*" <.> exts
  FinalLit Recursive path -> "**" </> path
  FinalLit NonRecursive path -> path

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
      let (candidateBase, candidateExts) = splitExtensions (NE.last $ seg :| segs)
      guard (not (null candidateBase))
      checkExt multidot ext candidateExts
    FinalMatch NonRecursive multidot ext -> do
      let (candidateBase, candidateExts) = splitExtensions seg
      guard (null segs && not (null candidateBase))
      checkExt multidot ext candidateExts
    FinalLit isRecursive filename -> do
      guard ((isRecursive == Recursive || null segs) && filename == seg)
      return (GlobMatch ())

checkExt
  :: MultiDot
  -> String
  -- ^ The pattern's extension
  -> String
  -- ^ The candidate file's extension
  -> Maybe (GlobResult ())
checkExt multidot ext candidate
  | ext == candidate = Just (GlobMatch ())
  | ext `isSuffixOf` candidate = case multidot of
      MultiDotDisabled -> Just (GlobWarnMultiDot ())
      MultiDotEnabled -> Just (GlobMatch ())
  | otherwise = Nothing

parseFileGlob :: CabalSpecVersion -> FilePath -> Either GlobSyntaxError Glob
parseFileGlob version filepath = case reverse (splitDirectories filepath) of
  [] ->
    Left EmptyGlob
  (filename : "**" : segments)
    | allowGlobStar -> do
        finalSegment <- case splitExtensions filename of
          ("*", ext)
            | '*' `elem` ext -> Left StarInExtension
            | null ext -> Left NoExtensionOnStar
            | otherwise -> Right (FinalMatch Recursive multidot ext)
          _ ->
            if allowLiteralFilenameGlobStar
              then Right (FinalLit Recursive filename)
              else Left LiteralFileNameGlobStar
        foldM addStem (GlobFinal finalSegment) segments
    | otherwise -> Left VersionDoesNotSupportGlobStar
  (filename : segments) -> do
    pat <- case splitExtensions filename of
      ("*", ext)
        | not allowGlob -> Left VersionDoesNotSupportGlob
        | '*' `elem` ext -> Left StarInExtension
        | null ext -> Left NoExtensionOnStar
        | otherwise -> Right (FinalMatch NonRecursive multidot ext)
      (_, ext)
        | '*' `elem` ext -> Left StarInExtension
        | '*' `elem` filename -> Left StarInFileName
        | otherwise -> Right (FinalLit NonRecursive filename)
    foldM addStem (GlobFinal pat) segments
  where
    allowGlob = version >= CabalSpecV1_6
    allowGlobStar = version >= CabalSpecV2_4
    addStem pat seg
      | '*' `elem` seg = Left StarInDirectory
      | otherwise = Right (GlobStem seg pat)
    multidot
      | version >= CabalSpecV2_4 = MultiDotEnabled
      | otherwise = MultiDotDisabled
    allowLiteralFilenameGlobStar = version >= CabalSpecV3_8

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
matchDirFileGlob :: Verbosity -> CabalSpecVersion -> FilePath -> FilePath -> IO [FilePath]
matchDirFileGlob v = matchDirFileGlobWithDie v dieWithException

-- | Like 'matchDirFileGlob' but with customizable 'die'
--
-- @since 3.6.0.0
matchDirFileGlobWithDie :: Verbosity -> (Verbosity -> CabalException -> IO [FilePath]) -> CabalSpecVersion -> FilePath -> FilePath -> IO [FilePath]
matchDirFileGlobWithDie verbosity rip version dir filepath = case parseFileGlob version filepath of
  Left err -> rip verbosity $ MatchDirFileGlob (explainGlobSyntaxError filepath err)
  Right glob -> do
    results <- runDirFileGlob verbosity dir glob
    let missingDirectories =
          [missingDir | GlobMissingDirectory missingDir <- results]
        matches = globMatches results

    let errors :: [String]
        errors =
          [ "filepath wildcard '"
            ++ filepath
            ++ "' refers to the directory"
            ++ " '"
            ++ missingDir
            ++ "', which does not exist or is not a directory."
          | missingDir <- missingDirectories
          ]
            ++ [ "filepath wildcard '" ++ filepath ++ "' does not match any files."
               | null matches
               ]

    if null errors
      then return matches
      else rip verbosity $ MatchDirFileGlobErrors errors

-- | Match files against a pre-parsed glob, starting in a directory.
--
-- The 'Version' argument must be the spec version of the package
-- description being processed, as globs behave slightly differently
-- in different spec versions.
--
-- The 'FilePath' argument is the directory that the glob is relative
-- to. It must be a valid directory (and hence it can't be the empty
-- string). The returned values will not include this prefix.
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
        else return [GlobMissingDirectory joinedPrefix]
    FinalLit Recursive fn -> do
      let prefix = dir </> joinedPrefix
      directoryExists <- doesDirectoryExist prefix
      if directoryExists
        then do
          candidates <- getDirectoryContentsRecursive prefix
          let checkName candidate
                | takeFileName candidate == fn = Just $ GlobMatch (joinedPrefix </> candidate)
                | otherwise = Nothing
          return $ mapMaybe checkName candidates
        else return [GlobMissingDirectory joinedPrefix]
    FinalLit NonRecursive fn -> do
      exists <- doesFileExist (dir </> joinedPrefix </> fn)
      return [GlobMatch (joinedPrefix </> fn) | exists]

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

isRecursiveInRoot :: Glob -> Bool
isRecursiveInRoot (GlobFinal (FinalMatch Recursive _ _)) = True
isRecursiveInRoot _ = False
