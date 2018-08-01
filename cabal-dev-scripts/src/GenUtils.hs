{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GenUtils where

import Control.Lens
import Data.Char    (toUpper)
import Data.Maybe   (fromMaybe)
import Data.Text    (Text)

import qualified Data.Algorithm.Diff as Diff
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL

-------------------------------------------------------------------------------
-- License List version
-------------------------------------------------------------------------------

-- | SPDX license list version
data SPDXLicenseListVersion
    = SPDXLicenseListVersion_3_0
    | SPDXLicenseListVersion_3_2
  deriving (Eq, Ord, Show, Enum, Bounded)

allVers :: Set.Set SPDXLicenseListVersion
allVers =  Set.fromList [minBound .. maxBound]

prettyVer :: SPDXLicenseListVersion -> Text
prettyVer SPDXLicenseListVersion_3_2 = "SPDX License List 3.2"
prettyVer SPDXLicenseListVersion_3_0 = "SPDX License List 3.0"

-------------------------------------------------------------------------------
-- Commmons
-------------------------------------------------------------------------------

header :: TL.Text
header = "-- This file is generated. See Makefile's spdx rule"

-------------------------------------------------------------------------------
-- Tools
-------------------------------------------------------------------------------

combine
    :: forall a b tag. (Ord b, Ord tag, Enum tag, Bounded tag)
    => (a -> b)
    -> (tag -> [a])
    -> [(a, Set.Set tag)]
combine f t
    = map addTags
    $ foldr process [] [ minBound .. maxBound ]
  where
    unDiff :: Diff.Diff a -> a
    unDiff (Diff.First a)  = a
    unDiff (Diff.Second a) = a
    unDiff (Diff.Both _ a) = a -- important we prefer latter versions!

    addTags :: a -> (a, Set.Set tag)
    addTags a = (a, fromMaybe Set.empty (Map.lookup (f a) tags))

    process :: tag -> [a] -> [a]
    process tag as = map unDiff $ Diff.getDiffBy (\x y -> f x == f y) (t tag) as

    tags :: Map.Map b (Set.Set tag)
    tags = Map.fromListWith Set.union
        [ (f a, Set.singleton tag)
        | tag <- [ minBound .. maxBound ]
        , a <- t tag
        ]

ordNubOn :: Ord b => (a -> b) -> [a] -> [a]
ordNubOn f = go Set.empty where
    go _    [] = []
    go past (a:as)
        | b `Set.member` past = go past as
        | otherwise           = a :  go (Set.insert b past) as
      where
        b = f a

textShow :: Text -> Text
textShow = T.pack . show

toConstructorName :: Text -> Text
toConstructorName t = t
    & each %~ f
    & ix 0 %~ toUpper
    & special
  where
    f '.' = '_'
    f '-' = '_'
    f '+' = '\''
    f c   = c

    special :: Text -> Text
    special "0BSD"          = "NullBSD"
    special "389_exception" = "DS389_exception"
    special u               = u

mkList :: [Text] -> Text
mkList []     = "    []"
mkList (x:xs) =
    "    [ " <> x <> "\n"
    <> foldMap (\x' -> "    , " <> x' <> "\n") xs
    <> "    ]"
