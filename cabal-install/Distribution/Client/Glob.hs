{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Glob
    ( GlobAtom(..)
    , Glob (..)
    , globMatches
    ) where

import Data.List (stripPrefix)
import Control.Monad (liftM2)
import Distribution.Compat.Binary
import GHC.Generics (Generic)

import Distribution.Text
import Distribution.Compat.ReadP
import qualified Text.PrettyPrint as Disp


-- | A piece of a globbing pattern
data GlobAtom = WildCard
              | Literal String
              | Union [Glob]
  deriving (Eq, Show, Generic)

instance Binary GlobAtom

-- | A single directory or file component of a globbed path
newtype Glob = Glob [GlobAtom]
  deriving (Eq, Show, Generic)

instance Binary Glob


-- | Test whether a file path component matches a globbing pattern
--
globMatches :: Glob -> String -> Bool
globMatches (Glob atoms) = goStart atoms
  where
    -- From the man page, glob(7):
    --   "If a filename starts with a '.', this character must be
    --    matched explicitly."
    
    go, goStart :: [GlobAtom] -> String -> Bool

    goStart (WildCard:_) ('.':_)  = False
    goStart (Union globs:rest) cs = any (\(Glob glob) ->
                                            goStart (glob ++ rest) cs) globs
    goStart rest               cs = go rest cs

    go []                 ""    = True
    go (Literal lit:rest) cs
      | Just cs' <- stripPrefix lit cs
                                = go rest cs'
      | otherwise               = False
    go [WildCard]         ""    = True
    go (WildCard:rest)   (c:cs) = go rest (c:cs) || go (WildCard:rest) cs
    go (Union globs:rest)   cs  = any (\(Glob glob) ->
                                          go (glob ++ rest) cs) globs
    go []                (_:_)  = False
    go (_:_)              ""    = False

instance Text Glob where
  disp (Glob atoms) = Disp.hcat (map dispAtom atoms) 
    where
      dispAtom WildCard      = Disp.char '*'
      dispAtom (Literal str) = Disp.text (escape str)
      dispAtom (Union globs) = Disp.braces
                                 (Disp.hcat (Disp.punctuate (Disp.char ',')
                                                            (map disp globs)))

      escape []               = []
      escape (c:cs)
        | isGlobEscapedChar c = '\\' : c : escape cs
        | otherwise           =        c : escape cs

  parse = Glob `fmap` many1 globAtom
    where
      globAtom :: ReadP r GlobAtom
      globAtom = literal +++ wildcard +++ union

      wildcard = char '*' >> return WildCard

      union = between (char '{') (char '}')
              (fmap (Union . map Glob) $ sepBy1 (many1 globAtom) (char ','))

      literal = Literal `fmap` many1'
        where
          litchar = normal +++ escape
          
          normal  = satisfy (not . isGlobEscapedChar)
          escape  = char '\\' >> satisfy isGlobEscapedChar

          many1' :: ReadP r [Char]
          many1' = liftM2 (:) litchar many'

          many' :: ReadP r [Char]
          many' = many1' <++ return []

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*'  = True
isGlobEscapedChar '{'  = True
isGlobEscapedChar '}'  = True
isGlobEscapedChar ','  = True
isGlobEscapedChar '\\' = True
isGlobEscapedChar '/'  = True
isGlobEscapedChar _    = False