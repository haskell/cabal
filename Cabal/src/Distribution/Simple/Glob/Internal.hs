{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Glob.Internal
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- License     :  BSD3
--                portions Copyright (c) 2007, Galois Inc.
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Internal module for simple file globbing.
-- Please import "Distribution.Simple.Glob" instead.
module Distribution.Simple.Glob.Internal where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec
import Distribution.Pretty
import qualified Text.PrettyPrint as Disp

--------------------------------------------------------------------------------

-- | A filepath specified by globbing.
data Glob
  = -- | @<dirGlob>/<glob>@
    GlobDir !GlobPieces !Glob
  | -- | @**/<glob>@, where @**@ denotes recursively traversing
    -- all directories and matching filenames on <glob>.
    GlobDirRecursive !GlobPieces
  | -- | A file glob.
    GlobFile !GlobPieces
  | -- | Trailing dir; a glob ending in @/@.
    GlobDirTrailing
  deriving (Eq, Show, Generic)

instance Binary Glob
instance Structured Glob

-- | A single directory or file component of a globbed path
type GlobPieces = [GlobPiece]

-- | A piece of a globbing pattern
data GlobPiece
  = -- | A wildcard @*@
    WildCard
  | -- | A literal string @dirABC@
    Literal String
  | -- | A union of patterns, e.g. @dir/{a,*.txt,c}/...@
    Union [GlobPieces]
  deriving (Eq, Show, Generic)

instance Binary GlobPiece
instance Structured GlobPiece

--------------------------------------------------------------------------------
-- Parsing & pretty-printing

instance Pretty Glob where
  pretty (GlobDir glob pathglob) =
    dispGlobPieces glob
      Disp.<> Disp.char '/'
      Disp.<> pretty pathglob
  pretty (GlobDirRecursive glob) =
    Disp.text "**/"
      Disp.<> dispGlobPieces glob
  pretty (GlobFile glob) = dispGlobPieces glob
  pretty GlobDirTrailing = Disp.empty

instance Parsec Glob where
  parsec = parsecPath
    where
      parsecPath :: CabalParsing m => m Glob
      parsecPath = do
        glob <- parsecGlob
        dirSep *> (GlobDir glob <$> parsecPath <|> pure (GlobDir glob GlobDirTrailing)) <|> pure (GlobFile glob)
      -- We could support parsing recursive directory search syntax
      -- @**@ here too, rather than just in 'parseFileGlob'

      dirSep :: CabalParsing m => m ()
      dirSep =
        () <$ P.char '/'
          <|> P.try
            ( do
                _ <- P.char '\\'
                -- check this isn't an escape code
                P.notFollowedBy (P.satisfy isGlobEscapedChar)
            )

      parsecGlob :: CabalParsing m => m GlobPieces
      parsecGlob = some parsecPiece
        where
          parsecPiece = P.choice [literal, wildcard, union]

          wildcard = WildCard <$ P.char '*'
          union = Union . toList <$> P.between (P.char '{') (P.char '}') (P.sepByNonEmpty parsecGlob (P.char ','))
          literal = Literal <$> some litchar

          litchar = normal <|> escape

          normal = P.satisfy (\c -> not (isGlobEscapedChar c) && c /= '/' && c /= '\\')
          escape = P.try $ P.char '\\' >> P.satisfy isGlobEscapedChar

dispGlobPieces :: GlobPieces -> Disp.Doc
dispGlobPieces = Disp.hcat . map dispPiece
  where
    dispPiece WildCard = Disp.char '*'
    dispPiece (Literal str) = Disp.text (escape str)
    dispPiece (Union globs) =
      Disp.braces
        ( Disp.hcat
            ( Disp.punctuate
                (Disp.char ',')
                (map dispGlobPieces globs)
            )
        )
    escape [] = []
    escape (c : cs)
      | isGlobEscapedChar c = '\\' : c : escape cs
      | otherwise = c : escape cs

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*' = True
isGlobEscapedChar '{' = True
isGlobEscapedChar '}' = True
isGlobEscapedChar ',' = True
isGlobEscapedChar _ = False
