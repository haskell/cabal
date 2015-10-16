{-# LANGUAGE DeriveFunctor #-}
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
    -- TODO: reconsider whether we actually need `IfElseBlock`
    | IfElseBlock ann [SectionArg ann] [Field ann] [Field ann]
  deriving (Eq, Show, Functor)

fieldAnn :: Field ann -> ann
fieldAnn (Field (Name ann _) _)     = ann
fieldAnn (Section (Name ann _) _ _) = ann
fieldAnn (IfElseBlock ann _ _ _)    = ann

data FieldLine ann  = FieldLine  !ann !ByteString
  deriving (Eq, Show, Functor)

data SectionArg ann
    = SecArgName  !ann !ByteString
    | SecArgStr   !ann !String
    | SecArgNum   !ann !ByteString
    | SecArgOther !ann !ByteString
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

data Name ann  = Name       !ann !ByteString
  deriving (Eq, Show, Functor)

mkName :: ann -> ByteString -> Name ann
mkName ann bs = Name ann (B.map Char.toLower bs)

getName :: Name a -> ByteString
getName (Name _ bs) = bs

nameAnn :: Name ann -> ann
nameAnn (Name ann _) = ann
