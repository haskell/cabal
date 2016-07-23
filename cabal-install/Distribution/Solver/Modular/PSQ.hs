{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Solver.Modular.PSQ
    ( PSQ(..)  -- Unit test needs constructor access
    , casePSQ
    , cons
    , degree
    , delete
    , dminimumBy
    , length
    , lookup
    , filter
    , filterKeys
    , firstOnly
    , fromList
    , isZeroOrOne
    , keys
    , map
    , mapKeys
    , mapWithKey
    , mapWithKeyState
    , maximumBy
    , minimumBy
    , null
    , prefer
    , preferByKeys
    , preferOrElse
    , snoc
    , sortBy
    , sortByKeys
    , splits
    , toList
    , union
    ) where

-- Priority search queues.
--
-- I am not yet sure what exactly is needed. But we need a data structure with
-- key-based lookup that can be sorted. We're using a sequence right now with
-- (inefficiently implemented) lookup, because I think that queue-based
-- operations and sorting turn out to be more efficiency-critical in practice.

import Distribution.Solver.Modular.Degree

import Control.Arrow (first, second)

import qualified Data.Foldable as F
import Data.Function
import qualified Data.List as S
import Data.Ord (comparing)
import Data.Traversable
import Prelude hiding (foldr, length, lookup, filter, null, map)

newtype PSQ k v = PSQ [(k, v)]
  deriving (Eq, Show, Functor, F.Foldable, Traversable) -- Qualified Foldable to avoid issues with FTP

keys :: PSQ k v -> [k]
keys (PSQ xs) = fmap fst xs

lookup :: Eq k => k -> PSQ k v -> Maybe v
lookup k (PSQ xs) = S.lookup k xs

map :: (v1 -> v2) -> PSQ k v1 -> PSQ k v2
map f (PSQ xs) = PSQ (fmap (second f) xs)

mapKeys :: (k1 -> k2) -> PSQ k1 v -> PSQ k2 v
mapKeys f (PSQ xs) = PSQ (fmap (first f) xs)

mapWithKey :: (k -> a -> b) -> PSQ k a -> PSQ k b
mapWithKey f (PSQ xs) = PSQ (fmap (\ (k, v) -> (k, f k v)) xs)

mapWithKeyState :: (s -> k -> a -> (b, s)) -> PSQ k a -> s -> PSQ k b
mapWithKeyState p (PSQ xs) s0 =
  PSQ (F.foldr (\ (k, v) r s -> case p s k v of
                                  (w, n) -> (k, w) : (r n))
               (const []) xs s0)

delete :: Eq k => k -> PSQ k a -> PSQ k a
delete k (PSQ xs) = PSQ (snd (S.partition ((== k) . fst) xs))

fromList :: [(k, a)] -> PSQ k a
fromList = PSQ

cons :: k -> a -> PSQ k a -> PSQ k a
cons k x (PSQ xs) = PSQ ((k, x) : xs)

snoc :: PSQ k a -> k -> a -> PSQ k a
snoc (PSQ xs) k x = PSQ (xs ++ [(k, x)])

casePSQ :: PSQ k a -> r -> (k -> a -> PSQ k a -> r) -> r
casePSQ (PSQ xs) n c =
  case xs of
    []          -> n
    (k, v) : ys -> c k v (PSQ ys)

splits :: PSQ k a -> PSQ k (a, PSQ k a)
splits = go id
  where
    go f xs = casePSQ xs
        (PSQ [])
        (\ k v ys -> cons k (v, f ys) (go (f . cons k v) ys))

sortBy :: (a -> a -> Ordering) -> PSQ k a -> PSQ k a
sortBy cmp (PSQ xs) = PSQ (S.sortBy (cmp `on` snd) xs)

sortByKeys :: (k -> k -> Ordering) -> PSQ k a -> PSQ k a
sortByKeys cmp (PSQ xs) = PSQ (S.sortBy (cmp `on` fst) xs)

-- | Given a measure in form of a pseudo-peano-natural number,
-- determine the approximate minimum. This is designed to stop
-- even traversing the list as soon as we find any element with
-- measure 'ZeroOrOne'.
--
-- Always returns a one-element queue (except if the queue is
-- empty, then we return an empty queue again).
--
dminimumBy :: (a -> Degree) -> PSQ k a -> PSQ k a
dminimumBy _   (PSQ [])       = PSQ []
dminimumBy sel (PSQ (x : xs)) = go (sel (snd x)) x xs
  where
    go ZeroOrOne v _ = PSQ [v]
    go _ v []        = PSQ [v]
    go c v (y : ys)  = case compare c d of
      LT -> go c v ys
      EQ -> go c v ys
      GT -> go d y ys
      where
        d = sel (snd y)

maximumBy :: (k -> Int) -> PSQ k a -> (k, a)
maximumBy sel (PSQ xs) =
  S.minimumBy (flip (comparing (sel . fst))) xs

minimumBy :: (a -> Int) -> PSQ k a -> PSQ k a
minimumBy sel (PSQ xs) =
  PSQ [snd (S.minimumBy (comparing fst) (S.map (\ x -> (sel (snd x), x)) xs))]

-- | Will partition the list according to the predicate. If
-- there is any element that satisfies the precidate, then only
-- the elements satisfying the predicate are returned.
-- Otherwise, the rest is returned.
--
prefer :: (a -> Bool) -> PSQ k a -> PSQ k a
prefer p (PSQ xs) =
  let
    (pro, con) = S.partition (p . snd) xs
  in
    if S.null pro then PSQ con else PSQ pro

-- | Variant of 'prefer' that takes a continuation for the case
-- that there are none of the desired elements.
preferOrElse :: (a -> Bool) -> (PSQ k a -> PSQ k a) -> PSQ k a -> PSQ k a
preferOrElse p k (PSQ xs) =
  let
    (pro, con) = S.partition (p . snd) xs
  in
    if S.null pro then k (PSQ con) else PSQ pro

-- | Variant of 'prefer' that takes a predicate on the keys
-- rather than on the values.
--
preferByKeys :: (k -> Bool) -> PSQ k a -> PSQ k a
preferByKeys p (PSQ xs) =
  let
    (pro, con) = S.partition (p . fst) xs
  in
    if S.null pro then PSQ con else PSQ pro

filterKeys :: (k -> Bool) -> PSQ k a -> PSQ k a
filterKeys p (PSQ xs) = PSQ (S.filter (p . fst) xs)

filter :: (a -> Bool) -> PSQ k a -> PSQ k a
filter p (PSQ xs) = PSQ (S.filter (p . snd) xs)

length :: PSQ k a -> Int
length (PSQ xs) = S.length xs

degree :: PSQ k a -> Degree
degree (PSQ [])     = ZeroOrOne
degree (PSQ [_])    = ZeroOrOne
degree (PSQ [_, _]) = Two
degree (PSQ _)      = Other

null :: PSQ k a -> Bool
null (PSQ xs) = S.null xs

isZeroOrOne :: PSQ k a -> Bool
isZeroOrOne (PSQ [])  = True
isZeroOrOne (PSQ [_]) = True
isZeroOrOne _         = False

firstOnly :: PSQ k a -> PSQ k a
firstOnly (PSQ [])      = PSQ []
firstOnly (PSQ (x : _)) = PSQ [x]

toList :: PSQ k a -> [(k, a)]
toList (PSQ xs) = xs

union :: PSQ k a -> PSQ k a -> PSQ k a
union (PSQ xs) (PSQ ys) = PSQ (xs ++ ys)
