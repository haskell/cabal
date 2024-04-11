-- |
-- Module      :  Distribution.PackageDescription.Check.Paths
-- Copyright   :  Lennart Kolmodin 2008, Francesco Ariis 2023
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Functions to check filepaths, directories, globs, etc.
module Distribution.PackageDescription.Check.Paths
  ( checkGlob
  , checkPath
  , fileExtensionSupportedLanguage
  , isGoodRelativeDirectoryPath
  , isGoodRelativeFilePath
  , isGoodRelativeGlob
  , isInsideDist
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.PackageDescription.Check.Common
import Distribution.PackageDescription.Check.Monad
import Distribution.Simple.CCompiler
import Distribution.Simple.Glob
  ( Glob
  , explainGlobSyntaxError
  , isRecursiveInRoot
  , parseFileGlob
  )
import Distribution.Simple.Utils hiding (findPackageDesc, notice)
import System.FilePath (splitDirectories, splitPath, takeExtension)

import qualified System.FilePath.Windows as FilePath.Windows (isValid)

fileExtensionSupportedLanguage :: FilePath -> Bool
fileExtensionSupportedLanguage path =
  isHaskell || isC
  where
    extension = takeExtension path
    isHaskell = extension `elem` [".hs", ".lhs"]
    isC = isJust (filenameCDialect extension)

-- Boolean: are absolute paths allowed?
checkPath
  :: Monad m
  => Bool -- Can be absolute path?
  -> CabalField -- .cabal field that we are checking.
  -> PathKind -- Path type.
  -> FilePath -- Path.
  -> CheckM m ()
checkPath isAbs title kind path = do
  checkP
    (isOutsideTree path)
    (PackageBuildWarning $ RelativeOutside title path)
  checkP
    (isInsideDist path)
    (PackageDistInexcusable $ DistPoint (Just title) path)
  checkPackageFileNamesWithGlob kind path

  -- Skip if "can be absolute path".
  checkP
    (not isAbs && isAbsoluteOnAnyPlatform path)
    (PackageDistInexcusable $ AbsolutePath title path)
  case grl path kind of
    Just e ->
      checkP
        (not isAbs)
        (PackageDistInexcusable $ BadRelativePath title path e)
    Nothing -> return ()
  checkWindowsPath (kind == PathKindGlob) path
  where
    isOutsideTree wpath = case splitDirectories wpath of
      ".." : _ -> True
      "." : ".." : _ -> True
      _ -> False

    -- These are not paths, but globs...
    grl wfp PathKindFile = isGoodRelativeFilePath wfp
    grl wfp PathKindGlob = isGoodRelativeGlob wfp
    grl wfp PathKindDirectory = isGoodRelativeDirectoryPath wfp

-- | Is a 'FilePath' inside `dist`, `dist-newstyle` and friends?
isInsideDist :: FilePath -> Bool
isInsideDist path =
  case map lowercase (splitDirectories path) of
    "dist" : _ -> True
    "." : "dist" : _ -> True
    "dist-newstyle" : _ -> True
    "." : "dist-newstyle" : _ -> True
    _ -> False

checkPackageFileNamesWithGlob
  :: Monad m
  => PathKind
  -> FilePath -- Filepath or possibly a glob pattern.
  -> CheckM m ()
checkPackageFileNamesWithGlob kind fp = do
  checkWindowsPath (kind == PathKindGlob) fp
  checkTarPath fp

checkWindowsPath
  :: Monad m
  => Bool -- Is it a glob pattern?
  -> FilePath -- Path.
  -> CheckM m ()
checkWindowsPath isGlob path =
  checkP
    (not . FilePath.Windows.isValid $ escape isGlob path)
    (PackageDistInexcusable $ InvalidOnWin [path])
  where
    -- Force a relative name to catch invalid file names like "f:oo" which
    -- otherwise parse as file "oo" in the current directory on the 'f' drive.
    escape :: Bool -> String -> String
    escape wisGlob wpath =
      (".\\" ++)
      -- Glob paths will be expanded before being dereferenced, so asterisks
      -- shouldn't count against them.
      $
        map (\c -> if c == '*' && wisGlob then 'x' else c) wpath

-- | Check a file name is valid for the portable POSIX tar format.
--
-- The POSIX tar format has a restriction on the length of file names. It is
-- unfortunately not a simple restriction like a maximum length. The exact
-- restriction is that either the whole path be 100 characters or less, or it
-- be possible to split the path on a directory separator such that the first
-- part is 155 characters or less and the second part 100 characters or less.
checkTarPath :: Monad m => FilePath -> CheckM m ()
checkTarPath path
  | length path > 255 = tellP longPath
  | otherwise = case pack nameMax (reverse (splitPath path)) of
      Left err -> tellP err
      Right [] -> return ()
      Right (h : rest) -> case pack prefixMax remainder of
        Left err -> tellP err
        Right [] -> return ()
        Right (_ : _) -> tellP noSplit
        where
          -- drop the '/' between the name and prefix:
          remainder = safeInit h : rest
  where
    nameMax, prefixMax :: Int
    nameMax = 100
    prefixMax = 155

    pack _ [] = Left emptyName
    pack maxLen (c : cs)
      | n > maxLen = Left longName
      | otherwise = Right (pack' maxLen n cs)
      where
        n = length c

    pack' maxLen n (c : cs)
      | n' <= maxLen = pack' maxLen n' cs
      where
        n' = n + length c
    pack' _ _ cs = cs

    longPath = PackageDistInexcusable (FilePathTooLong path)
    longName = PackageDistInexcusable (FilePathNameTooLong path)
    noSplit = PackageDistInexcusable (FilePathSplitTooLong path)
    emptyName = PackageDistInexcusable FilePathEmpty

-- `checkGlob` checks glob patterns and returns good ones for further
-- processing.
checkGlob
  :: Monad m
  => CabalField -- .cabal field we are checking.
  -> FilePath -- glob filepath pattern
  -> CheckM m (Maybe Glob)
checkGlob title pat = do
  ver <- asksCM ccSpecVersion

  -- Glob sanity check.
  case parseFileGlob ver pat of
    Left e -> do
      tellP
        ( PackageDistInexcusable $
            GlobSyntaxError title (explainGlobSyntaxError pat e)
        )
      return Nothing
    Right wglob -> do
      -- \* Miscellaneous checks on sane glob.
      -- Checks for recursive glob in root.
      checkP
        (isRecursiveInRoot wglob)
        ( PackageDistSuspiciousWarn $
            RecursiveGlobInRoot title pat
        )
      return (Just wglob)

-- | Whether a path is a good relative path.  We aren't worried about perfect
-- cross-platform compatibility here; this function just checks the paths in
-- the (local) @.cabal@ file, while only Hackage needs the portability.
--
-- >>> let test fp = putStrLn $ show (isGoodRelativeDirectoryPath fp) ++ "; " ++ show (isGoodRelativeFilePath fp)
--
-- Note that "foo./bar.hs" would be invalid on Windows.
--
-- >>> traverse_ test ["foo/bar/quu", "a/b.hs", "foo./bar.hs"]
-- Nothing; Nothing
-- Nothing; Nothing
-- Nothing; Nothing
--
-- Trailing slash is not allowed for files, for directories it is ok.
--
-- >>> test "foo/"
-- Nothing; Just "trailing slash"
--
-- Leading @./@ is fine, but @.@ and @./@ are not valid files.
--
-- >>> traverse_ test [".", "./", "./foo/bar"]
-- Nothing; Just "trailing dot segment"
-- Nothing; Just "trailing slash"
-- Nothing; Nothing
--
-- Lastly, not good file nor directory cases:
--
-- >>> traverse_ test ["", "/tmp/src", "foo//bar", "foo/.", "foo/./bar", "foo/../bar"]
-- Just "empty path"; Just "empty path"
-- Just "posix absolute path"; Just "posix absolute path"
-- Just "empty path segment"; Just "empty path segment"
-- Just "trailing same directory segment: ."; Just "trailing same directory segment: ."
-- Just "same directory segment: ."; Just "same directory segment: ."
-- Just "parent directory segment: .."; Just "parent directory segment: .."
--
-- For the last case, 'isGoodRelativeGlob' doesn't warn:
--
-- >>> traverse_ (print . isGoodRelativeGlob) ["foo/../bar"]
-- Just "parent directory segment: .."
isGoodRelativeFilePath :: FilePath -> Maybe String
isGoodRelativeFilePath = state0
  where
    -- initial state
    state0 [] = Just "empty path"
    state0 (c : cs)
      | c == '.' = state1 cs
      | c == '/' = Just "posix absolute path"
      | otherwise = state5 cs

    -- after initial .
    state1 [] = Just "trailing dot segment"
    state1 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = state2 cs
      | otherwise = state5 cs

    -- after ./ or after / between segments
    state2 [] = Just "trailing slash"
    state2 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = Just "empty path segment"
      | otherwise = state5 cs

    -- after non-first segment's .
    state3 [] = Just "trailing same directory segment: ."
    state3 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = Just "same directory segment: ."
      | otherwise = state5 cs

    -- after ..
    state4 [] = Just "trailing parent directory segment: .."
    state4 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = Just "parent directory segment: .."
      | otherwise = state5 cs

    -- in a segment which is ok.
    state5 [] = Nothing
    state5 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = state2 cs
      | otherwise = state5 cs

-- | See 'isGoodRelativeFilePath'.
--
-- This is barebones function. We check whether the glob is a valid file
-- by replacing stars @*@ with @x@ses.
isGoodRelativeGlob :: FilePath -> Maybe String
isGoodRelativeGlob = isGoodRelativeFilePath . map f
  where
    f '*' = 'x'
    f c = c

-- | See 'isGoodRelativeFilePath'.
isGoodRelativeDirectoryPath :: FilePath -> Maybe String
isGoodRelativeDirectoryPath = state0
  where
    -- initial state
    state0 [] = Just "empty path"
    state0 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = Just "posix absolute path"
      | otherwise = state4 cs

    -- after initial ./ or after / between segments
    state1 [] = Nothing
    state1 (c : cs)
      | c == '.' = state2 cs
      | c == '/' = Just "empty path segment"
      | otherwise = state4 cs

    -- after non-first setgment's .
    state2 [] = Just "trailing same directory segment: ."
    state2 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = Just "same directory segment: ."
      | otherwise = state4 cs

    -- after ..
    state3 [] = Just "trailing parent directory segment: .."
    state3 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = Just "parent directory segment: .."
      | otherwise = state4 cs

    -- in a segment which is ok.
    state4 [] = Nothing
    state4 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = state1 cs
      | otherwise = state4 cs

    -- after initial .
    state5 [] = Nothing -- "."
    state5 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = state1 cs
      | otherwise = state4 cs

-- [Note: Good relative paths]
--
-- Using @kleene@ we can define an extended regex:
--
-- @
-- import Algebra.Lattice
-- import Kleene
-- import Kleene.ERE (ERE (..), intersections)
--
-- data C = CDot | CSlash | CChar
--   deriving (Eq, Ord, Enum, Bounded, Show)
--
-- reservedR :: ERE C
-- reservedR = notChar CSlash
--
-- pathPieceR :: ERE C
-- pathPieceR = intersections
--     [ plus reservedR
--     , ERENot (string [CDot])
--     , ERENot (string [CDot,CDot])
--     ]
--
-- filePathR :: ERE C
-- filePathR = optional (string [CDot, CSlash]) <> pathPieceR <> star (char CSlash <> pathPieceR)
--
-- dirPathR :: ERE C
-- dirPathR = (char CDot \/ filePathR) <> optional (char CSlash)
--
-- plus :: ERE C -> ERE C
-- plus r = r <> star r
--
-- optional :: ERE C -> ERE C
-- optional r = mempty \/ r
-- @
--
-- Results in following state machine for @filePathR@
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 1
--     | otherwise           -> 5
-- 1 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 2
--     | otherwise           -> 5
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | otherwise           -> 5
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | otherwise           -> 5
-- 4 -> \x -> if
--     | x <= CDot           -> 5
--     | otherwise           -> 5
-- 5+ -> \x -> if
--     | x <= CDot           -> 5
--     | x <= CSlash         -> 2
--     | otherwise           -> 5
-- @
--
-- and @dirPathR@:
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 5
--     | otherwise           -> 4
-- 1+ -> \x -> if
--     | x <= CDot           -> 2
--     | otherwise           -> 4
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | otherwise           -> 4
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | otherwise           -> 4
-- 4+ -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 1
--     | otherwise           -> 4
-- 5+ -> \x -> if
--     | x <= CDot           -> 3
--     | x <= CSlash         -> 1
--     | otherwise           -> 4
-- @
