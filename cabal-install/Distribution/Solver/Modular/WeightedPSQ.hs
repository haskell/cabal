{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Solver.Modular.WeightedPSQ (
    WeightedPSQ
  , filter
  , fromList
  , keys
  , length
  , degree
  , isZeroOrOne
  , lookup
  , mapWeightsWithKey
  , mapWithKey
  , toList
  , union
  , weights
  ) where

-- Association lists that are always sorted by weight.

import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Traversable as T
import Prelude hiding (filter, length, lookup)

import Distribution.Solver.Modular.Degree

newtype WeightedPSQ w k v = WeightedPSQ [(w, k, v)]
  deriving (Eq, Show, Functor, F.Foldable, T.Traversable)

filter :: (v -> Bool) -> WeightedPSQ k w v -> WeightedPSQ k w v
filter p (WeightedPSQ xs) = WeightedPSQ (L.filter (p . triple_3) xs)

length :: WeightedPSQ k w v -> Int
length (WeightedPSQ xs) = L.length xs

degree :: WeightedPSQ w k v -> Degree
degree (WeightedPSQ [])     = ZeroOrOne
degree (WeightedPSQ [_])    = ZeroOrOne
degree (WeightedPSQ [_, _]) = Two
degree (WeightedPSQ _)      = Other

isZeroOrOne :: WeightedPSQ w k v -> Bool
isZeroOrOne (WeightedPSQ [])  = True
isZeroOrOne (WeightedPSQ [_]) = True
isZeroOrOne _                 = False

toList :: WeightedPSQ w k v -> [(w, k, v)]
toList (WeightedPSQ xs) = xs

fromList :: Ord w => [(w, k, v)] -> WeightedPSQ w k v
fromList = WeightedPSQ . L.sortBy (comparing triple_1)

weights :: WeightedPSQ w k v -> [w]
weights (WeightedPSQ xs) = L.map triple_1 xs

keys :: WeightedPSQ w k v -> [k]
keys (WeightedPSQ xs) = L.map triple_2 xs

lookup :: Eq k => k -> WeightedPSQ w k v -> Maybe v
lookup k (WeightedPSQ xs) = triple_3 `fmap` L.find ((k ==) . triple_2) xs

mapWeightsWithKey :: Ord w2
                  => (k -> w1 -> w2)
                  -> WeightedPSQ w1 k v
                  -> WeightedPSQ w2 k v
mapWeightsWithKey f (WeightedPSQ xs) = fromList $
                                       L.map (\ (w, k, v) -> (f k w, k, v)) xs

mapWithKey :: (k -> v1 -> v2) -> WeightedPSQ w k v1 -> WeightedPSQ w k v2
mapWithKey f (WeightedPSQ xs) = WeightedPSQ $
                                L.map (\ (w, k, v) -> (w, k, f k v)) xs

union :: Ord w => WeightedPSQ w k v -> WeightedPSQ w k v -> WeightedPSQ w k v
union (WeightedPSQ xs) (WeightedPSQ ys) = fromList (xs ++ ys)

triple_1 :: (x, y, z) -> x
triple_1 (x, _, _) = x

triple_2 :: (x, y, z) -> y
triple_2 (_, y, _) = y

triple_3 :: (x, y, z) -> z
triple_3 (_, _, z) = z
