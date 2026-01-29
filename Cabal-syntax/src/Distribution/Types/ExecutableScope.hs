{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Types.ExecutableScope
  ( ExecutableScope (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.Annotation

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

data ExecutableScope
  = ExecutablePublic
  | ExecutablePrivate
  deriving (Generic, Show, Read, Eq, Ord, Data)

instance Pretty ExecutableScope where
  pretty ExecutablePublic = Disp.text "public"
  pretty ExecutablePrivate = Disp.text "private"

instance Markable ExecutableScope
instance ExactPretty ExecutableScope

instance ExactParsec ExecutableScope
instance Parsec ExecutableScope where
  parsec = P.try pub <|> pri
    where
      pub = ExecutablePublic <$ P.string "public"
      pri = ExecutablePrivate <$ P.string "private"

instance Binary ExecutableScope
instance Structured ExecutableScope
instance NFData ExecutableScope where rnf = genericRnf

-- | 'Any' like semigroup, where 'ExecutablePrivate' is 'Any True'
instance Semigroup ExecutableScope where
  ExecutablePublic <> x = x
  x@ExecutablePrivate <> _ = x

-- | 'mempty' = 'ExecutablePublic'
instance Monoid ExecutableScope where
  mempty = ExecutablePublic
  mappend = (<>)
