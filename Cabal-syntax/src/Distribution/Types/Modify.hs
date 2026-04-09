{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Types that can be used as modifiers
module Distribution.Types.Modify where

import Distribution.Trivia

import Data.Data
import Data.Kind

-- | Toggle whether a GPD component has annotation or not.
data HasAnnotation
  = HasAnn
  | HasNoAnn
  deriving (Show, Read, Eq, Ord, Data)

type family Annotate (m :: HasAnnotation) (a :: Type) where
  Annotate HasNoAnn a = a
  Annotate HasAnn a = Ann SurroundingText a

type family AttachPos (m :: HasAnnotation) (a :: Type) where
  AttachPos HasAnn a = (Positions, a)
  AttachPos HasNoAnn a = a

type family PreserveGrouping (m :: HasAnnotation) (a :: Type) where
  PreserveGrouping HasAnn a = [a]
  PreserveGrouping HasNoAnn a = a
