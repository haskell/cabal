{-# LANGUAGE DeriveFunctor #-}

module Distribution.Types.AnnotatedId
  ( AnnotatedId (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Package
import Distribution.Types.ComponentName

-- | An 'AnnotatedId' is a 'ComponentId', 'UnitId', etc.
-- which is annotated with some other useful information
-- that is useful for printing to users, etc.
--
-- Invariant: if ann_id x == ann_id y, then ann_pid x == ann_pid y
-- and ann_cname x == ann_cname y
data AnnotatedId id = AnnotatedId
  { ann_pid :: PackageId
  , ann_cname :: ComponentName
  , ann_id :: id
  }
  deriving (Show, Functor)

instance Eq id => Eq (AnnotatedId id) where
  x == y = ann_id x == ann_id y

instance Ord id => Ord (AnnotatedId id) where
  compare x y = compare (ann_id x) (ann_id y)

instance Package (AnnotatedId id) where
  packageId = ann_pid
