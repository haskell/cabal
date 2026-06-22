{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.Annotation where

import Distribution.Parsec.Position
import Distribution.Types.Trivia
import Distribution.Fields.Field

import qualified Data.ByteString as BS
import Data.Data
import Data.Kind
import Data.List (List)

-- | Toggle whether a GPD component has annotation or not.
data ParsingPhase
  = -- | Concrete syntax tree
    Conc
  | -- | Abstract syntax tree
    Abst
  deriving (Show, Read, Eq, Ord, Data)

type family IfConc (m :: ParsingPhase) (f :: Type -> Type) (a :: Type) where
  IfConc Abst _ a = a
  IfConc Conc f a = f a

-- Type family combinators that can compose and attach concrete syntax informations conditionally.
type AnnotateWith (t :: Type) (m :: ParsingPhase) (a :: Type) = IfConc m (Ann t) a
type Annotate (m :: ParsingPhase) (a :: Type) = AnnotateWith SurroundingText m a

type PreserveGrouping (m :: ParsingPhase) (a :: Type) = IfConc m List a

type AttachWith (t :: Type) (m :: ParsingPhase) (a :: Type) = IfConc m ((,) t) a
type AttachPositions (m :: ParsingPhase) (a :: Type) = AttachWith Positions m a
type AttachPosition (m :: ParsingPhase) (a :: Type) = AttachWith Position m a
type AttachComments (m :: ParsingPhase) (a :: Type) = AttachWith [Comment Position] m a

-- FieldGrammar aliases
-- Note: within each group, there is an associated comments list and item list
type MonoidalFieldAla (m :: ParsingPhase) (a :: Type) =
  PreserveGrouping m
    ( IfConc m ( (,,) [Comment Position] BS.ByteString )
      (AttachPositions m a)
    )

type UniqueField (m :: ParsingPhase) (a :: Type) = IfConc m ((,,) Positions BS.ByteString) a
