{-# LANGUAGE DeriveGeneric #-}

-- TODO: [code cleanup] plausibly much of this module should be merged with
-- similar functionality in Cabal.
module Distribution.Client.Glob
  ( FilePathGlob (..)
  , FilePathRoot (..)
  , FilePathGlobRel (..)
  , Glob
  , GlobPiece (..)
  , matchFileGlob
  , matchFileGlobRel
  , matchGlob
  , isTrivialFilePathGlob
  , getFilePathRootDirectory
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.List (stripPrefix)
import System.Directory
import System.FilePath

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | A file path specified by globbing
data FilePathGlob = FilePathGlob FilePathRoot FilePathGlobRel
  deriving (Eq, Show, Generic)

data FilePathGlobRel
  = GlobDir !Glob !FilePathGlobRel
  | GlobFile !Glob
  | -- | trailing dir, a glob ending in @/@
    GlobDirTrailing
  deriving (Eq, Show, Generic)

-- | A single directory or file component of a globbed path
type Glob = [GlobPiece]

-- | A piece of a globbing pattern
data GlobPiece
  = WildCard
  | Literal String
  | Union [Glob]
  deriving (Eq, Show, Generic)

data FilePathRoot
  = FilePathRelative
  | -- | e.g. @"/"@, @"c:\"@ or result of 'takeDrive'
    FilePathRoot FilePath
  | FilePathHomeDir
  deriving (Eq, Show, Generic)

instance Binary FilePathGlob
instance Binary FilePathRoot
instance Binary FilePathGlobRel
instance Binary GlobPiece

instance Structured FilePathGlob
instance Structured FilePathRoot
instance Structured FilePathGlobRel
instance Structured GlobPiece

-- | Check if a 'FilePathGlob' doesn't actually make use of any globbing and
-- is in fact equivalent to a non-glob 'FilePath'.
--
-- If it is trivial in this sense then the result is the equivalent constant
-- 'FilePath'. On the other hand if it is not trivial (so could in principle
-- match more than one file) then the result is @Nothing@.
isTrivialFilePathGlob :: FilePathGlob -> Maybe FilePath
isTrivialFilePathGlob (FilePathGlob root pathglob) =
  case root of
    FilePathRelative -> go [] pathglob
    FilePathRoot root' -> go [root'] pathglob
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

-- | Match a 'FilePathGlob' against the file system, starting from a given
-- root directory for relative paths. The results of relative globs are
-- relative to the given root. Matches for absolute globs are absolute.
matchFileGlob :: FilePath -> FilePathGlob -> IO [FilePath]
matchFileGlob relroot (FilePathGlob globroot glob) = do
  root <- getFilePathRootDirectory globroot relroot
  matches <- matchFileGlobRel root glob
  case globroot of
    FilePathRelative -> return matches
    _ -> return (map (root </>) matches)

-- | Match a 'FilePathGlobRel' against the file system, starting from a
-- given root directory. The results are all relative to the given root.
matchFileGlobRel :: FilePath -> FilePathGlobRel -> IO [FilePath]
matchFileGlobRel root glob0 = go glob0 ""
  where
    go (GlobFile glob) dir = do
      entries <- getDirectoryContents (root </> dir)
      let files = filter (matchGlob glob) entries
      return (map (dir </>) files)
    go (GlobDir glob globPath) dir = do
      entries <- getDirectoryContents (root </> dir)
      subdirs <-
        filterM
          ( \subdir ->
              doesDirectoryExist
                (root </> dir </> subdir)
          )
          $ filter (matchGlob glob) entries
      concat <$> traverse (\subdir -> go globPath (dir </> subdir)) subdirs
    go GlobDirTrailing dir = return [dir]

-- | Match a globbing pattern against a file path component
matchGlob :: Glob -> String -> Bool
matchGlob = goStart
  where
    -- From the man page, glob(7):
    --   "If a filename starts with a '.', this character must be
    --    matched explicitly."

    go, goStart :: [GlobPiece] -> String -> Bool

    goStart (WildCard : _) ('.' : _) = False
    goStart (Union globs : rest) cs =
      any
        (\glob -> goStart (glob ++ rest) cs)
        globs
    goStart rest cs = go rest cs

    go [] "" = True
    go (Literal lit : rest) cs
      | Just cs' <- stripPrefix lit cs =
          go rest cs'
      | otherwise = False
    go [WildCard] "" = True
    go (WildCard : rest) (c : cs) = go rest (c : cs) || go (WildCard : rest) cs
    go (Union globs : rest) cs = any (\glob -> go (glob ++ rest) cs) globs
    go [] (_ : _) = False
    go (_ : _) "" = False

------------------------------------------------------------------------------
-- Parsing & printing
--

instance Pretty FilePathGlob where
  pretty (FilePathGlob root pathglob) = pretty root Disp.<> pretty pathglob

instance Parsec FilePathGlob where
  parsec = do
    root <- parsec
    case root of
      FilePathRelative -> FilePathGlob root <$> parsec
      _ -> FilePathGlob root <$> parsec <|> pure (FilePathGlob root GlobDirTrailing)

instance Pretty FilePathRoot where
  pretty FilePathRelative = Disp.empty
  pretty (FilePathRoot root) = Disp.text root
  pretty FilePathHomeDir = Disp.char '~' Disp.<> Disp.char '/'

instance Parsec FilePathRoot where
  parsec = root <|> P.try home <|> P.try drive <|> pure FilePathRelative
    where
      root = FilePathRoot "/" <$ P.char '/'
      home = FilePathHomeDir <$ P.string "~/"
      drive = do
        dr <- P.satisfy $ \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        _ <- P.char ':'
        _ <- P.char '/' <|> P.char '\\'
        return (FilePathRoot (toUpper dr : ":\\"))

instance Pretty FilePathGlobRel where
  pretty (GlobDir glob pathglob) =
    dispGlob glob
      Disp.<> Disp.char '/'
      Disp.<> pretty pathglob
  pretty (GlobFile glob) = dispGlob glob
  pretty GlobDirTrailing = Disp.empty

instance Parsec FilePathGlobRel where
  parsec = parsecPath
    where
      parsecPath :: CabalParsing m => m FilePathGlobRel
      parsecPath = do
        glob <- parsecGlob
        dirSep *> (GlobDir glob <$> parsecPath <|> pure (GlobDir glob GlobDirTrailing)) <|> pure (GlobFile glob)

      dirSep :: CabalParsing m => m ()
      dirSep =
        () <$ P.char '/'
          <|> P.try
            ( do
                _ <- P.char '\\'
                -- check this isn't an escape code
                P.notFollowedBy (P.satisfy isGlobEscapedChar)
            )

dispGlob :: Glob -> Disp.Doc
dispGlob = Disp.hcat . map dispPiece
  where
    dispPiece WildCard = Disp.char '*'
    dispPiece (Literal str) = Disp.text (escape str)
    dispPiece (Union globs) =
      Disp.braces
        ( Disp.hcat
            ( Disp.punctuate
                (Disp.char ',')
                (map dispGlob globs)
            )
        )
    escape [] = []
    escape (c : cs)
      | isGlobEscapedChar c = '\\' : c : escape cs
      | otherwise = c : escape cs

parsecGlob :: CabalParsing m => m Glob
parsecGlob = some parsecPiece
  where
    parsecPiece = P.choice [literal, wildcard, union]

    wildcard = WildCard <$ P.char '*'
    union = Union . toList <$> P.between (P.char '{') (P.char '}') (P.sepByNonEmpty parsecGlob (P.char ','))
    literal = Literal <$> some litchar

    litchar = normal <|> escape

    normal = P.satisfy (\c -> not (isGlobEscapedChar c) && c /= '/' && c /= '\\')
    escape = P.try $ P.char '\\' >> P.satisfy isGlobEscapedChar

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*' = True
isGlobEscapedChar '{' = True
isGlobEscapedChar '}' = True
isGlobEscapedChar ',' = True
isGlobEscapedChar _ = False
