-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Compat.DList
-- Copyright   :  (c) Ben Gamari 2015-2019
-- License     :  BSD3
--
-- Maintainer  :  cabal-dev@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A very simple difference list.
module Distribution.Compat.DList
  ( DList
  , runDList
  , empty
  , singleton
  , fromList
  , toList
  , snoc
  ) where

import Distribution.Compat.Prelude hiding (empty, toList)
import Prelude ()

-- | Difference list.
newtype DList a = DList ([a] -> [a])

runDList :: DList a -> [a]
runDList (DList run) = run []

-- | Make 'DList' containing single element.
singleton :: a -> DList a
singleton a = DList (a :)

-- | @since 3.4.0.0
empty :: DList a
empty = DList id

fromList :: [a] -> DList a
fromList as = DList (as ++)

toList :: DList a -> [a]
toList = runDList

snoc :: DList a -> a -> DList a
snoc xs x = xs <> singleton x

instance Monoid (DList a) where
  mempty = empty
  mappend = (<>)

instance Semigroup (DList a) where
  DList a <> DList b = DList (a . b)
