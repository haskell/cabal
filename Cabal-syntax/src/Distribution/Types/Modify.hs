{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

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

type Annotate (m :: HasAnnotation) (a :: Type) = AnnotateWith SurroundingText m a

type family AnnotateWith (trivia :: Type) (m :: HasAnnotation) (a :: Type) where
  AnnotateWith t HasNoAnn a = a
  AnnotateWith t HasAnn a = Ann t a

type AttachPositions (m :: HasAnnotation) (a :: Type) = AttachWith Positions m a
type AttachPosition (m :: HasAnnotation) (a :: Type) = AttachWith Position m a

type family AttachWith (t :: Type) (m :: HasAnnotation) (a :: Type) where
  AttachWith t HasAnn a = (t, a)
  AttachWith _ HasNoAnn a = a

type family PreserveGrouping (m :: HasAnnotation) (a :: Type) where
  PreserveGrouping HasAnn a = [a]
  PreserveGrouping HasNoAnn a = a
