{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
-- | Sets of characters.
--
-- Using this is more efficient than 'RE.Type.Alt':ng individual characters.
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
    -- * Conversions
    fromList,
    toList,
    toIntervalList,
    -- * Special lists
    alpha,
    alphanum,
    alphanumNotDigit,
    upper,
    ) where

import Data.Char                     (chr, isAlpha, isAlphaNum, isDigit, isUpper, ord)
import Data.List                     (foldl')
import Data.Monoid                   (Monoid (..))
import Data.String                   (IsString (..))
import Data.Semigroup                (Semigroup (..))
import Prelude
       (Bounded (..), Char, Enum (..), Eq (..), Int, Num (..), Ord (..), Show (..), String, (&&), concatMap, flip, not, otherwise, showParen,
       showString, uncurry, ($), (.))

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

-- | Size of 'CharSet'
--
size :: CharSet -> Int
size (CS m) = foldl' (\ !acc (lo, hi) -> acc + (hi - lo) + 1) 0 (IM.toList m)

-- | Singleton character set.
singleton :: Char -> CharSet
singleton c = CS (IM.singleton (ord c) (ord c))

-- | Insert 'Char' into 'CharSet'.
{- FOURMOLU_DISABLE -}
insert :: Char -> CharSet -> CharSet
{- FOURMOLU_ENABLE -}
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
alphanumNotDigit :: CharSet
alphanumNotDigit = foldl' (flip insert) empty [ c | c <- [ minBound .. maxBound ], isAlphaNum c && not (isDigit c) ]
{-# NOINLINE alphanumNotDigit #-}

-- | Note: this set varies depending on @base@ version.
--
alphanum :: CharSet
alphanum = foldl' (flip insert) alphanumNotDigit ['0' .. '9' ]
{-# NOINLINE alphanum #-}

-- | Note: this set varies depending on @base@ version.
--
upper :: CharSet
upper = foldl' (flip insert) empty [ c | c <- [ minBound .. maxBound ], isUpper c ]
{-# NOINLINE upper #-}

-- $setup
-- Use -XOverloadedStrings to avoid the error: Couldn't match type ‘[Char]’ with ‘CharSet’
-- >>> :set -XOverloadedStrings
-- >>> import Prelude (length)
