{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Types that can be used as modifiers
module Distribution.Types.Modify where

import Distribution.Parsec.Position
import Distribution.Trivia

import Data.Data
import Data.Kind

-- | Toggle whether a GPD component has annotation or not.
data HasAnnotation
  = HasAnn
  | HasNoAnn
  deriving (Show, Read, Eq, Ord, Data)

-- Type family combinators that can compose and attach concrete syntax informations conditionally.

type family AnnotateWith (trivia :: Type) (m :: HasAnnotation) (a :: Type) where
  AnnotateWith t HasNoAnn a = a
  AnnotateWith t HasAnn a = Ann t a

type Annotate (m :: HasAnnotation) (a :: Type) = AnnotateWith SurroundingText m a

type family AttachPos (m :: HasAnnotation) (a :: Type) where
  AttachPos HasAnn a = (Positions, a)
  AttachPos HasNoAnn a = a

type family WithPos (m :: HasAnnotation) where
  WithPos HasAnn = Position
  WithPos HasNoAnn = ()

type family PreserveGrouping (m :: HasAnnotation) (a :: Type) where
  PreserveGrouping HasAnn a = [a]
  PreserveGrouping HasNoAnn a = a
