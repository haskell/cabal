module Distribution.Client.Dependency.Modular.PSQ where

-- Priority search queues.
--
-- I am not yet sure what exactly is needed. But we need a datastructure with
-- key-based lookup that can be sorted. We're using a sequence right now with
-- (inefficiently implemented) lookup, because I think that queue-based
-- operations and sorting turn out to be more efficiency-critical in practice.

import Control.Applicative
import Data.Foldable
import Data.Function
import Data.List as S hiding (foldr)
import Data.Traversable
import Prelude hiding (foldr)

newtype PSQ k v = PSQ [(k, v)]
  deriving (Eq, Show)

instance Functor (PSQ k) where
  fmap f (PSQ xs) = PSQ (fmap (\ (k, v) -> (k, f v)) xs)

instance Foldable (PSQ k) where
  foldr op e (PSQ xs) = foldr op e (fmap snd xs)

instance Traversable (PSQ k) where
  traverse f (PSQ xs) = PSQ <$> traverse (\ (k, v) -> (\ x -> (k, x)) <$> f v) xs

keys :: PSQ k v -> [k]
keys (PSQ xs) = fmap fst xs

lookup :: Eq k => k -> PSQ k v -> Maybe v
lookup k (PSQ xs) = S.lookup k xs

map :: (v1 -> v2) -> PSQ k v1 -> PSQ k v2
map f (PSQ xs) = PSQ (fmap (\ (k, v) -> (k, f v)) xs)

mapKeys :: (k1 -> k2) -> PSQ k1 v -> PSQ k2 v
mapKeys f (PSQ xs) = PSQ (fmap (\ (k, v) -> (f k, v)) xs)

mapWithKey :: (k -> a -> b) -> PSQ k a -> PSQ k b
mapWithKey f (PSQ xs) = PSQ (fmap (\ (k, v) -> (k, f k v)) xs)

mapWithKeyState :: (s -> k -> a -> (b, s)) -> PSQ k a -> s -> PSQ k b
mapWithKeyState p (PSQ xs) s0 =
  PSQ (foldr (\ (k, v) r s -> case p s k v of
                                (w, n) -> (k, w) : (r n))
             (const []) xs s0)

delete :: Eq k => k -> PSQ k a -> PSQ k a
delete k (PSQ xs) = PSQ (snd (partition ((== k) . fst) xs))

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

filterKeys :: (k -> Bool) -> PSQ k a -> PSQ k a
filterKeys p (PSQ xs) = PSQ (S.filter (p . fst) xs)

filter :: (a -> Bool) -> PSQ k a -> PSQ k a
filter p (PSQ xs) = PSQ (S.filter (p . snd) xs)

length :: PSQ k a -> Int
length (PSQ xs) = S.length xs

-- | "Lazy length".
--
-- Only approximates the length, but doesn't force the list.
llength :: PSQ k a -> Int
llength (PSQ [])       = 0
llength (PSQ (_:[]))   = 1
llength (PSQ (_:_:[])) = 2
llength (PSQ _)        = 3

null :: PSQ k a -> Bool
null (PSQ xs) = S.null xs

toList :: PSQ k a -> [(k, a)]
toList (PSQ xs) = xs
