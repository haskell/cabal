{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Distribution.Solver.Modular.WeightedPSQ (
    WeightedPSQ
  , fromList
  , toList
  , keys
  , weights
  , isZeroOrOne
  , filter
  , lookup
  , mapWithKey
  , mapWeightsWithKey
  , traverseWithKey
  , union
  , takeUntil
  ) where

import qualified Data.Foldable as F
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (First(..))
import qualified Data.Traversable as T
import Prelude hiding (filter, lookup)

-- | An association list that is sorted by weight.
--
-- Each element has a key ('k'), value ('v'), and weight ('w'). All operations
-- that add elements or modify weights stably sort the elements by weight.
newtype WeightedPSQ w k v = WeightedPSQ (Map w (NonEmpty (k, v)))
  deriving (Eq, Show, Functor, F.Foldable, T.Traversable)

-- | /O(N)/.
filter :: (v -> Bool) -> WeightedPSQ k w v -> WeightedPSQ k w v
filter p (WeightedPSQ xs) =
  WeightedPSQ $ M.mapMaybe (NE.nonEmpty . NE.filter (p . snd)) xs

-- | /O(1)/. Return @True@ if the @WeightedPSQ@ contains zero or one elements.
isZeroOrOne :: WeightedPSQ w k v -> Bool
isZeroOrOne (WeightedPSQ ws) = case M.minView ws of
  Nothing -> True
  Just (_ :| kvs, rest) -> L.null kvs && M.null rest

-- | /O(1)/. Return the elements in order.
toList :: WeightedPSQ w k v -> [(w, k, v)]
toList (WeightedPSQ xs) =
  concatMap (\(w, kvs) -> L.map (\(k, v) -> (w, k, v)) (NE.toList kvs)) $ M.assocs xs

-- | /O(N log N)/.
fromList :: Ord w => [(w, k, v)] -> WeightedPSQ w k v
fromList = WeightedPSQ . M.fromListWith (flip (<>)) . L.map (\(w, k, v) -> (w, (k, v) :| []))

-- | /O(N)/. Return the weights in order.
weights :: WeightedPSQ w k v -> [w]
weights (WeightedPSQ xs) =
  concatMap (\(w, kvs) -> L.replicate (NE.length kvs) w) $ M.assocs xs

-- | /O(N)/. Return the keys in order.
keys :: WeightedPSQ w k v -> [k]
keys (WeightedPSQ xs) = L.map fst $ concatMap NE.toList $ M.elems xs

-- | /O(N)/. Return the value associated with the first occurrence of the give
-- key, if it exists.
lookup :: Eq k => k -> WeightedPSQ w k v -> Maybe v
lookup k (WeightedPSQ xs) = getFirst $ foldMap (First . L.lookup k . NE.toList) xs

-- | /O(N log N)/. Update the weights.
mapWeightsWithKey :: Ord w2
                  => (k -> w1 -> w2)
                  -> WeightedPSQ w1 k v
                  -> WeightedPSQ w2 k v
mapWeightsWithKey f = fromList . L.map (\(w, k, v) -> (f k w, k, v)) . toList

-- | /O(N)/. Update the values.
mapWithKey :: (k -> v1 -> v2) -> WeightedPSQ w k v1 -> WeightedPSQ w k v2
mapWithKey f (WeightedPSQ xs) = WeightedPSQ $
  M.map (NE.map (\(k, v) -> (k, f k v))) xs

-- | /O(N)/. Traverse and update values in some applicative functor.
traverseWithKey
  :: Applicative f
  => (k -> v -> f v')
  -> WeightedPSQ w k v
  -> f (WeightedPSQ w k v')
traverseWithKey f (WeightedPSQ xs) = WeightedPSQ <$>
  traverse (traverse (\(k,v) -> (k,) <$> f k v)) xs

-- | \(O\bigl(M \log\bigl(\frac{N+1}{M+1}\bigr)\bigr), \; M \leq N\).
-- Combine two @WeightedPSQ@s, preserving all
-- elements. Elements from the first @WeightedPSQ@ come before elements in the
-- second when they have the same weight.
union :: Ord w => WeightedPSQ w k v -> WeightedPSQ w k v -> WeightedPSQ w k v
union (WeightedPSQ xs) (WeightedPSQ ys) = WeightedPSQ $ M.unionWith (<>) xs ys

-- | /O(N)/. Return the prefix of values ending with the first element that
-- satisfies p, or all elements if none satisfy p.
takeUntil :: forall w k v. (v -> Bool) -> WeightedPSQ w k v -> WeightedPSQ w k v
takeUntil p (WeightedPSQ xs) = WeightedPSQ $ M.fromDistinctAscList $ go $ M.toAscList xs
  where
    go :: [(w, NonEmpty (k, v))] -> [(w, NonEmpty (k, v))]
    go [] = []
    go ((w, kvs) : rest) = case NE.break (p . snd) kvs of
      (_, []) -> (w, kvs) : go rest
      (pref, x : _) -> [(w, NE.fromList (pref ++ [x]))]
