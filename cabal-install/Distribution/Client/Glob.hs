{-# LANGUAGE DeriveGeneric #-}

--TODO: [code cleanup] plausibly much of this module should be merged with
-- similar functionality in Cabal.
module Distribution.Client.Glob
    ( FilePathGlob(..)
    , Glob
    , GlobPiece(..)
    , matchFileGlob
    , matchGlob
    , isTrivialFilePathGlob
    ) where

import           Data.List (stripPrefix)
import           Control.Monad
import           Distribution.Compat.Binary
import           GHC.Generics (Generic)

import           Distribution.Text
import           Distribution.Compat.ReadP (ReadP, (<++), (+++))
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

import           System.FilePath
import           System.Directory


-- | A file path specified by globbing
--
data FilePathGlob
   = GlobDir  !Glob !FilePathGlob
   | GlobFile !Glob
  deriving (Eq, Show, Generic)

-- | A single directory or file component of a globbed path
type Glob = [GlobPiece]

-- | A piece of a globbing pattern
data GlobPiece = WildCard
               | Literal String
               | Union [Glob]
  deriving (Eq, Show, Generic)

instance Binary FilePathGlob
instance Binary GlobPiece


isTrivialFilePathGlob :: FilePathGlob -> Maybe FilePath
isTrivialFilePathGlob = go []
  where
    go paths (GlobDir  [Literal path] globs) = go (path:paths) globs
    go paths (GlobFile [Literal path]) = Just (joinPath (reverse (path:paths)))
    go _ _ = Nothing


------------------------------------------------------------------------------
-- Matching
--

-- | Match a 'FilePathGlob' against the file system, starting from a
-- given root directory. The results are all relative to the given root.
--
matchFileGlob :: FilePath -> FilePathGlob -> IO [FilePath]
matchFileGlob root glob0 = go glob0 ""
  where
    go (GlobFile glob) dir = do
      entries <- getDirectoryContents (root </> dir)
      let files = filter (matchGlob glob) entries
      return (map (dir </>) files)

    go (GlobDir glob globPath) dir = do
      entries <- getDirectoryContents (root </> dir)
      subdirs <- filterM (\subdir -> doesDirectoryExist
                                       (root </> dir </> subdir))
               $ filter (matchGlob glob) entries
      concat <$> mapM (\subdir -> go globPath (dir </> subdir)) subdirs


-- | Match a globbing pattern against a file path component
--
matchGlob :: Glob -> String -> Bool
matchGlob = goStart
  where
    -- From the man page, glob(7):
    --   "If a filename starts with a '.', this character must be
    --    matched explicitly."

    go, goStart :: [GlobPiece] -> String -> Bool

    goStart (WildCard:_) ('.':_)  = False
    goStart (Union globs:rest) cs = any (\glob -> goStart (glob ++ rest) cs)
                                        globs
    goStart rest               cs = go rest cs

    go []                 ""    = True
    go (Literal lit:rest) cs
      | Just cs' <- stripPrefix lit cs
                                = go rest cs'
      | otherwise               = False
    go [WildCard]         ""    = True
    go (WildCard:rest)   (c:cs) = go rest (c:cs) || go (WildCard:rest) cs
    go (Union globs:rest)   cs  = any (\glob -> go (glob ++ rest) cs) globs
    go []                (_:_)  = False
    go (_:_)              ""    = False


------------------------------------------------------------------------------
-- Parsing & printing
--

instance Text FilePathGlob where
  disp (GlobDir  glob pathglob) = dispGlob glob
                          Disp.<> Disp.char '/'
                          Disp.<> disp pathglob
  disp (GlobFile glob)          = dispGlob glob

  parse =
      parseGlob >>= \globpieces ->
      (asDir globpieces <++ asFile globpieces)
    where
      asDir  glob = do _ <- Parse.char '/'
                       globs <- parse
                       return (GlobDir glob globs)
      asFile glob = return (GlobFile glob)


dispGlob :: Glob -> Disp.Doc
dispGlob = Disp.hcat . map dispPiece
  where
    dispPiece WildCard      = Disp.char '*'
    dispPiece (Literal str) = Disp.text (escape str)
    dispPiece (Union globs) = Disp.braces
                                (Disp.hcat (Disp.punctuate
                                             (Disp.char ',')
                                             (map dispGlob globs)))
    escape []               = []
    escape (c:cs)
      | isGlobEscapedChar c = '\\' : c : escape cs
      | otherwise           =        c : escape cs

parseGlob :: ReadP r Glob
parseGlob = Parse.many1 parsePiece
  where
    parsePiece = literal +++ wildcard +++ union

    wildcard = Parse.char '*' >> return WildCard

    union = Parse.between (Parse.char '{') (Parse.char '}') $
              fmap Union (Parse.sepBy1 parseGlob (Parse.char ','))

    literal = Literal `fmap` litchars1

    litchar = normal +++ escape

    normal  = Parse.satisfy (not . isGlobEscapedChar)
    escape  = Parse.char '\\' >> Parse.satisfy isGlobEscapedChar

    litchars1 :: ReadP r [Char]
    litchars1 = liftM2 (:) litchar litchars

    litchars :: ReadP r [Char]
    litchars = litchars1 <++ return []

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*'  = True
isGlobEscapedChar '{'  = True
isGlobEscapedChar '}'  = True
isGlobEscapedChar ','  = True
isGlobEscapedChar '\\' = True
isGlobEscapedChar '/'  = True
isGlobEscapedChar _    = False

