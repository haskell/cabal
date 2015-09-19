{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Glob
    ( GlobAtom(..)
    , Glob (..)
    , globMatches
    ) where

import Distribution.Compat.ReadP
import Distribution.Text
import Control.Monad (liftM2)
import Data.Binary
import GHC.Generics (Generic)

-- | A piece of a globbing pattern
data GlobAtom = WildCard
              | Literal String
              | Union [Glob]
              deriving (Show, Generic)
instance Binary GlobAtom

-- | A single directory or file component of a globbed path
newtype Glob = Glob [GlobAtom]
             deriving (Show, Generic)
instance Binary Glob

-- | @a `dropPrefix` b@ tests whether @b@ is a prefix of @a@,
-- return @Just@ the remaining portion of @a@ if so.
dropPrefix :: String -> String -> Maybe String
dropPrefix xs     []     = Just xs
dropPrefix (x:xs) (y:ys)
  | x == y               = dropPrefix xs ys
dropPrefix _      _      = Nothing

-- | Test whether a name matches a globbing pattern
globMatches :: Glob -> String -> Bool
globMatches (Glob atoms) = globMatches' atoms

globMatches' :: [GlobAtom] -> String -> Bool
globMatches' [] ""                 = True
globMatches' (Literal lit:rest) s
  | Just s' <- s `dropPrefix` lit = globMatches' rest s'
  | otherwise                     = False
globMatches' [WildCard] ""         = True
globMatches' (WildCard:rest) (x:xs)=
    globMatches' rest (x:xs) || globMatches' (WildCard:rest) xs
globMatches' (Union globs:rest) xs =
    any (\(Glob glob) -> glob `globMatches'` xs) globs && globMatches' rest xs
globMatches' [] (_:_)              = False
globMatches' (_:_) ""              = False

instance Text Glob where
  disp  = error "Glob disp"
  parse = Glob `fmap` many1 globAtom
    where
      wildcard = char '*' >> return WildCard
      union = between (char '{') (char '}')
              (Union . map Glob <$> sepBy1 (many globAtom) (char ','))
      --literal = Literal `fmap` (many1 $ (char '\\' >> choice (map char escaped)) +++ other)
      literal = Literal `fmap` many1'
        where
          escaped = "*!{},"
          other = satisfy (`notElem` escaped)
          many1' :: ReadP r [Char]
          many1' = liftM2 (:) other many'
          many' :: ReadP r [Char]
          many' = many1' <++ return []

      globAtom :: ReadP r GlobAtom
      globAtom = literal +++ wildcard +++ union

-- | Simplify a glob pattern
simplify :: [GlobAtom] -> [GlobAtom]
simplify []                             = []
simplify (WildCard : WildCard : rest)   = simplify (WildCard : rest)
simplify (Literal x : Literal y : rest) = simplify (Literal (x++y) : rest)
simplify (Union globs : rest)           =
    Union (map (\(Glob glob) -> Glob $ simplify glob) globs) : simplify rest
simplify (x : rest)                     = x : simplify rest
