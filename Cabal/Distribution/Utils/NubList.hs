module Distribution.Utils.NubList
    ( NubList    -- opaque
    , toNubList  -- smart construtor
    , fromNubList
    ) where

import Data.Binary
import Data.List (nub)
import Data.Monoid

import Distribution.Simple.Utils

import qualified Text.Read as R

-- | NubList : A list where every element in the list is unique to the list
-- and the original list order is maintained.
newtype NubList a =
    NubList { fromNubList :: [a] }
    deriving Eq

-- NubList assumes that nub retains the list order while removing duplicate
-- elements (keeping the first occurence). The Data.List.nub documentation does
-- not specifically state that ordering is maintained so we will add a test for
-- that to the test suite.

toNubList :: Ord a => [a] -> NubList a
toNubList list = NubList $ ordNub list

-- | Monoid operations on NubLists.
-- For a valid Monoid instance we need to satistfy the required monoid laws;
-- identity, associativity and closure.
--
-- Identity : by inspection:
--      mempty `mappend` NubList xs == NubList xs `mappend` mempty
--
-- Associativity : by inspection:
--      (NubList xs `mappend` NubList ys) `mappend` NubList zs
--      == NubList xs `mappend` (NubList ys `mappend` NubList zs)
--
-- Closure : appending two lists of type a and removing duplicates obviously
-- does not change the type.

instance Ord a => Monoid (NubList a) where
    mempty = NubList []
    NubList xs `mappend` NubList ys = NubList . nub $ xs ++ ys

instance Show a => Show (NubList a) where
    show (NubList a) = show a

instance (Ord a, Read a) => Read (NubList a) where
    readPrec = R.parens . R.prec 10 $ fmap toNubList R.readPrec

-- Binary instance of NubList is the same as for List. For put, we just pull off
-- constructor and put the list. For get, we get the list and make a NubList
-- out of it using toNubList.
instance (Ord a, Binary a) => Binary (NubList a) where
    put (NubList l) = put l
    get = fmap toNubList get
