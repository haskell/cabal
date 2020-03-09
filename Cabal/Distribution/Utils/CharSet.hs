{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE Safe         #-}
-- | Sets of characters.
--
-- Using this is more efficint than 'RE.Type.Alt':ng individual characters.
module Distribution.Utils.CharSet (
    -- * Set of characters
    CharSet,
    -- * Construction
    empty,
    universe,
    singleton,
    insert,
    union,
    intersection,
    complement,
    difference,
    -- * Query
    size,
    null,
    member,
    -- * Conversions
    fromList,
    toList,
    fromIntervalList,
    toIntervalList,
    -- * Special lists
    alpha,
    alphanum,
    ) where

import Distribution.Compat.Prelude hiding (empty, null, toList)
import Prelude ()

#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as IM
#else
import qualified Data.IntMap as IM
#endif

-- | A set of 'Char's.
--
-- We use range set, which works great with 'Char'.
newtype CharSet = CS { unCS :: IM.IntMap Int }
  deriving (Eq, Ord)

instance IsString CharSet where
    fromString = fromList

instance Show CharSet where
    showsPrec d cs
        | size cs < 20
        = showsPrec d (toList cs)
        | otherwise
        = showParen (d > 10)
        $ showString "CS "
        . showsPrec 11 (unCS cs)

instance Semigroup CharSet where
    (<>) = union

instance Monoid CharSet where
    mempty = empty
    mappend = (<>)

-- | Empty character set.
empty :: CharSet
empty = CS IM.empty

-- | universe
universe :: CharSet
universe = CS $ IM.singleton 0 0x10ffff

-- | Check whether 'CharSet' is 'empty'.
null :: CharSet -> Bool
null (CS cs) = IM.null cs

-- | Size of 'CharSet'
--
-- >>> size $ fromIntervalList [('a','f'), ('0','9')]
-- 16
--
-- >>> length $ toList $ fromIntervalList [('a','f'), ('0','9')]
-- 16
--
size :: CharSet -> Int
size (CS m) = foldl' (\ !acc (lo, hi) -> acc + (hi - lo) + 1) 0 (IM.toList m)

-- | Singleton character set.
singleton :: Char -> CharSet
singleton c = CS (IM.singleton (ord c) (ord c))

-- | Test whether character is in the set.
member :: Char -> CharSet -> Bool
#if MIN_VERSION_containers(0,5,0)
member c (CS m) = case IM.lookupLE i m of
    Nothing      -> False
    Just (_, hi) -> i <= hi
  where
#else
member c (CS m) = go (IM.toList m)
  where
    go [] = False
    go ((x,y):zs) = (x <= i && i <= y) || go zs
#endif
    i = ord c

-- | Insert 'Char' into 'CharSet'.
insert :: Char -> CharSet -> CharSet
insert c (CS m) = normalise (IM.insert (ord c) (ord c) m)

-- | Union of two 'CharSet's.
union :: CharSet -> CharSet -> CharSet
union (CS xs) (CS ys) = normalise (IM.unionWith max xs ys)

-- | Intersection of two 'CharSet's
intersection :: CharSet -> CharSet -> CharSet
intersection (CS xs) (CS ys) = CS $
    IM.fromList (intersectRangeList (IM.toList xs) (IM.toList ys))

-- | Compute the intersection.
intersectRangeList :: Ord a => [(a, a)] -> [(a, a)] -> [(a, a)]
intersectRangeList aset@((x,y):as) bset@((u,v):bs)
   | y < u     = intersectRangeList as bset
   | v < x     = intersectRangeList aset bs
   | y < v     = (max x u, y) : intersectRangeList as bset
   | otherwise = (max x u, v) : intersectRangeList aset bs
intersectRangeList _ [] = []
intersectRangeList [] _ = []

-- | Complement of a CharSet
complement :: CharSet -> CharSet
complement (CS xs) = CS $ IM.fromList $ complementRangeList (IM.toList xs)

-- | Compute the complement intersected with @[x,)@ assuming @x<u@.
complementRangeList' :: Int -> [(Int, Int)] -> [(Int, Int)]
complementRangeList' x ((u,v):s) = (x,pred u) : complementRangeList'' v s
complementRangeList' x []        = [(x,0x10ffff)]

-- | Compute the complement intersected with @(x,)@.
complementRangeList'' :: Int -> [(Int, Int)] -> [(Int, Int)]
complementRangeList'' x s
    | x == 0x10ffff = []
    | otherwise     = complementRangeList' (succ x) s

-- | Compute the complement.
--
-- Note: we treat Ints as codepoints, i.e minBound is 0, and maxBound is 0x10ffff
complementRangeList :: [(Int, Int)] -> [(Int, Int)]
complementRangeList s@((x,y):s')
    | x == 0    = complementRangeList'' y s'
    | otherwise = complementRangeList' 0 s
complementRangeList [] = [(0, 0x10ffff)]

-- | Difference of two 'CharSet's.
difference :: CharSet -> CharSet -> CharSet
difference xs ys = intersection xs (complement ys)

-- | Make 'CharSet' from a list of characters, i.e. 'String'.
fromList :: String -> CharSet
fromList = normalise . foldl' (\ acc c -> IM.insert (ord c) (ord c) acc) IM.empty

-- | Convert 'CharSet' to a list of characters i.e. 'String'.
toList :: CharSet -> String
toList = concatMap (uncurry enumFromTo) . toIntervalList

-- | Convert to interval list
--
-- >>> toIntervalList $ union "01234" "56789"
-- [('0','9')]
--
toIntervalList :: CharSet -> [(Char, Char)]
toIntervalList (CS m) = [ (chr lo, chr hi) | (lo, hi) <- IM.toList m ]

-- | Convert from interval pairs.
--
-- >>> fromIntervalList []
-- ""
--
-- >>> fromIntervalList [('a','f'), ('0','9')]
-- "0123456789abcdef"
--
-- >>> fromIntervalList [('Z','A')]
-- ""
--
fromIntervalList :: [(Char,Char)] -> CharSet
fromIntervalList xs = normalise' $ sortBy (\a b -> compare (fst a) (fst b))
    [ (ord lo, ord hi)
    | (lo, hi) <- xs
    , lo <= hi
    ]

-------------------------------------------------------------------------------
-- Normalisation
-------------------------------------------------------------------------------

normalise :: IM.IntMap Int -> CharSet
normalise = normalise'. IM.toList

normalise' :: [(Int,Int)] -> CharSet
normalise' = CS . IM.fromList . go where
    go :: [(Int,Int)] -> [(Int,Int)]
    go []         = []
    go ((x,y):zs) = go' x y zs

    go' :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
    go' lo hi [] = [(lo, hi)]
    go' lo hi ws0@((u,v):ws)
        | u <= succ hi = go' lo (max v hi) ws
        | otherwise    = (lo,hi) : go ws0

-------------------------------------------------------------------------------
-- Alpha Numeric character list
-------------------------------------------------------------------------------

-- Computing this takes some time,
-- but they are not used in-non testing in Cabal's normal operation.

-- | Note: this set varies depending on @base@ version.
--
alpha :: CharSet
alpha = foldl' (flip insert) empty [ c | c <- [ minBound .. maxBound ], isAlpha c ]
{-# NOINLINE alpha #-}

-- | Note: this set varies depending on @base@ version.
--
alphanum :: CharSet
alphanum = foldl' (flip insert) empty [ c | c <- [ minBound .. maxBound ], isAlphaNum c ]
{-# NOINLINE alphanum #-}
