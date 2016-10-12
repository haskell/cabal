{-# LANGUAGE DeriveFunctor #-}
-- | Cabal-like file AST types: 'Field', 'Section' etc
--
-- These types are parametrized by an annotation.
module Distribution.Parsec.Types.Field (
    -- * Cabal file
    Field (..),
    fieldAnn,
    FieldLine (..),
    SectionArg (..),
    sectionArgAnn,
    -- * Name
    Name (..),
    mkName,
    getName,
    nameAnn,
    ) where

import Prelude ()
import Distribution.Compat.Prelude

import Data.ByteString (ByteString)

import qualified Data.Char as Char
import qualified Data.ByteString.Char8 as B

-------------------------------------------------------------------------------
-- Cabal file
-------------------------------------------------------------------------------

data Field ann
    = Field   !(Name ann) [FieldLine ann]
    | Section !(Name ann) [SectionArg ann] [Field ann]
  deriving (Eq, Show, Functor)

fieldAnn :: Field ann -> ann
fieldAnn (Field (Name ann _) _)     = ann
fieldAnn (Section (Name ann _) _ _) = ann

-- | A line of text representing the value of a field from a Cabal file.
-- A field may contain multiple lines.
--
-- /Invariant:/ 'ByteString' has no newlines.
data FieldLine ann  = FieldLine  !ann !ByteString
  deriving (Eq, Show, Functor)

-- | Section arguments, e.g. name of the library
data SectionArg ann
    = SecArgName  !ann !ByteString
      -- ^ identifier
    | SecArgStr   !ann !String
      -- ^ quoted string
    | SecArgNum   !ann !ByteString
      -- ^ integral number
    | SecArgOther !ann !ByteString
      -- ^ everything else, mm. operators (e.g. in if-section conditionals)
  deriving (Eq, Show, Functor)

-- | Extract annotation from 'SectionArg'.
sectionArgAnn :: SectionArg ann -> ann
sectionArgAnn (SecArgName ann _)  = ann
sectionArgAnn (SecArgStr ann _)   = ann
sectionArgAnn (SecArgNum ann _)   = ann
sectionArgAnn (SecArgOther ann _) = ann

-------------------------------------------------------------------------------
-- Name
-------------------------------------------------------------------------------

-- | A field name.
--
-- /Invariant/: 'ByteString' is lower-case ASCII.
data Name ann  = Name       !ann !ByteString
  deriving (Eq, Show, Functor)

mkName :: ann -> ByteString -> Name ann
mkName ann bs = Name ann (B.map Char.toLower bs)

getName :: Name ann -> ByteString
getName (Name _ bs) = bs

nameAnn :: Name ann -> ann
nameAnn (Name ann _) = ann
