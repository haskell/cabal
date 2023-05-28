{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Compat.NonEmptySet
  ( NonEmptySet

    -- * Construction
  , singleton

    -- * Insertion
  , insert

    -- * Deletion
  , delete

    -- * Conversions
  , toNonEmpty
  , fromNonEmpty
  , toList
  , toSet

    -- * Query
  , member

    -- * Map
  , map
  ) where

import Prelude (Bool (..), Eq, Maybe (..), Ord (..), Read, Show (..), String, error, otherwise, return, showParen, showString, ($), (++), (.))

import Control.DeepSeq (NFData (..))
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup (..))
import Data.Typeable (Typeable)

import qualified Data.Foldable as F
import qualified Data.Set as Set

import Distribution.Compat.Binary (Binary (..))
import Distribution.Utils.Structured

#if MIN_VERSION_binary(0,6,0)
import Control.Applicative (empty)
#else
import Control.Monad (fail)
#endif

-- | @since 3.4.0.0
newtype NonEmptySet a = NES (Set.Set a)
  deriving (Eq, Ord, Typeable, Data, Read)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Show a => Show (NonEmptySet a) where
  showsPrec d s =
    showParen (d > 10) $
      showString "fromNonEmpty "
        . showsPrec 11 (toNonEmpty s)

{- FOURMOLU_DISABLE -}
instance Binary a => Binary (NonEmptySet a) where
  put (NES s) = put s
  get = do
      xs <- get
      if Set.null xs
#if MIN_VERSION_binary(0,6,0)
      then empty
#else
      then fail "NonEmptySet: empty"
#endif
      else return (NES xs)
{- FOURMOLU_ENABLE -}

instance Structured a => Structured (NonEmptySet a) where
  structure = containerStructure

instance NFData a => NFData (NonEmptySet a) where
  rnf (NES x) = rnf x

-- | Note: there aren't @Monoid@ instance.
instance Ord a => Semigroup (NonEmptySet a) where
  NES x <> NES y = NES (Set.union x y)

instance F.Foldable NonEmptySet where
  foldMap f (NES s) = F.foldMap f s
  foldr f z (NES s) = F.foldr f z s

#if MIN_VERSION_base(4,8,0)
  toList         = toList
  null _         = False
  length (NES s) = F.length s
#endif

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

singleton :: a -> NonEmptySet a
singleton = NES . Set.singleton

-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

insert :: Ord a => a -> NonEmptySet a -> NonEmptySet a
insert x (NES xs) = NES (Set.insert x xs)

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

delete :: Ord a => a -> NonEmptySet a -> Maybe (NonEmptySet a)
delete x (NES xs)
  | Set.null res = Nothing
  | otherwise = Just (NES xs)
  where
    res = Set.delete x xs

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

fromNonEmpty :: Ord a => NonEmpty a -> NonEmptySet a
fromNonEmpty (x :| xs) = NES (Set.fromList (x : xs))

toNonEmpty :: NonEmptySet a -> NonEmpty a
toNonEmpty (NES s) = case Set.toList s of
  [] -> panic "toNonEmpty"
  x : xs -> x :| xs

toList :: NonEmptySet a -> [a]
toList (NES s) = Set.toList s

toSet :: NonEmptySet a -> Set.Set a
toSet (NES s) = s

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

member :: Ord a => a -> NonEmptySet a -> Bool
member x (NES xs) = Set.member x xs

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

{- FOURMOLU_DISABLE -}
map
    :: ( Ord b
#if !MIN_VERSION_containers(0,5,2)
       , Ord a
#endif
       )
    => (a -> b) -> NonEmptySet a -> NonEmptySet b
map f (NES x) = NES (Set.map f x)
{- FOURMOLU_ENABLE -}

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

panic :: String -> a
panic msg = error $ "NonEmptySet invariant violated: " ++ msg
