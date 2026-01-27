{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Types.LibraryVisibility
  ( LibraryVisibility (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.Annotation

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | Multi-lib visibility
--
-- @since 3.0.0.0
data LibraryVisibility
  = -- | Can be used as a dependency for other packages
    LibraryVisibilityPublic
  | -- | Internal library, default
    LibraryVisibilityPrivate
  deriving (Generic, Show, Read, Eq, Ord, Data)

instance Pretty LibraryVisibility where
  pretty LibraryVisibilityPublic = Disp.text "public"
  pretty LibraryVisibilityPrivate = Disp.text "private"

instance Markable LibraryVisibility
instance Prettier LibraryVisibility where
  prettier _ = pretty

instance ExactParsec LibraryVisibility where exactParsec = (mempty,) <$> parsec
instance Parsec LibraryVisibility where
  parsec = do
    name <- P.munch1 isAlpha
    case name of
      "public" -> return LibraryVisibilityPublic
      "private" -> return LibraryVisibilityPrivate
      _ -> fail $ "Unknown visibility: " ++ name

instance Binary LibraryVisibility
instance Structured LibraryVisibility
instance NFData LibraryVisibility where rnf = genericRnf

instance Semigroup LibraryVisibility where
  LibraryVisibilityPrivate <> LibraryVisibilityPrivate = LibraryVisibilityPrivate
  _ <> _ = LibraryVisibilityPublic

instance Monoid LibraryVisibility where
  mempty = LibraryVisibilityPrivate
  mappend = (<>)
