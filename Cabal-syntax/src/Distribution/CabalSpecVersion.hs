{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.CabalSpecVersion where

import Distribution.Compat.Prelude
import Prelude ()

-- | Different Cabal-the-spec versions.
--
-- We branch based on this at least in the parser.
data CabalSpecVersion
  = -- | this is older than 'CabalSpecV1_2'
    CabalSpecV1_0
  | -- | new syntax (sections)
    CabalSpecV1_2
  | CabalSpecV1_4
  | CabalSpecV1_6
  | CabalSpecV1_8
  | CabalSpecV1_10
  | CabalSpecV1_12
  | -- 1.16 -- 1.14: no changes
    CabalSpecV1_18
  | CabalSpecV1_20
  | CabalSpecV1_22
  | CabalSpecV1_24
  | CabalSpecV2_0
  | CabalSpecV2_2
  | CabalSpecV2_4
  | CabalSpecV3_0
  | -- 3.2: no changes
    CabalSpecV3_4
  | CabalSpecV3_6
  | CabalSpecV3_8
  | -- 3.10: no changes
    CabalSpecV3_12
  | CabalSpecV3_14
  | CabalSpecV3_16
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

instance Binary CabalSpecVersion
instance Structured CabalSpecVersion
instance NFData CabalSpecVersion where rnf = genericRnf

-- | Show cabal spec version, but not the way in the .cabal files
--
-- @since 3.0.0.0
showCabalSpecVersion :: CabalSpecVersion -> String
showCabalSpecVersion CabalSpecV3_16 = "3.16"
showCabalSpecVersion CabalSpecV3_14 = "3.14"
showCabalSpecVersion CabalSpecV3_12 = "3.12"
showCabalSpecVersion CabalSpecV3_8 = "3.8"
showCabalSpecVersion CabalSpecV3_6 = "3.6"
showCabalSpecVersion CabalSpecV3_4 = "3.4"
showCabalSpecVersion CabalSpecV3_0 = "3.0"
showCabalSpecVersion CabalSpecV2_4 = "2.4"
showCabalSpecVersion CabalSpecV2_2 = "2.2"
showCabalSpecVersion CabalSpecV2_0 = "2.0"
showCabalSpecVersion CabalSpecV1_24 = "1.24"
showCabalSpecVersion CabalSpecV1_22 = "1.22"
showCabalSpecVersion CabalSpecV1_20 = "1.20"
showCabalSpecVersion CabalSpecV1_18 = "1.18"
showCabalSpecVersion CabalSpecV1_12 = "1.12"
showCabalSpecVersion CabalSpecV1_10 = "1.10"
showCabalSpecVersion CabalSpecV1_8 = "1.8"
showCabalSpecVersion CabalSpecV1_6 = "1.6"
showCabalSpecVersion CabalSpecV1_4 = "1.4"
showCabalSpecVersion CabalSpecV1_2 = "1.2"
showCabalSpecVersion CabalSpecV1_0 = "1.0"

cabalSpecLatest :: CabalSpecVersion
cabalSpecLatest = CabalSpecV3_16

-- | Parse 'CabalSpecVersion' from version digits.
--
-- It may fail if for recent versions the version is not exact.
cabalSpecFromVersionDigits :: [Int] -> Maybe CabalSpecVersion
cabalSpecFromVersionDigits v
  | v == [3, 16] = Just CabalSpecV3_16
  | v == [3, 14] = Just CabalSpecV3_14
  | v == [3, 12] = Just CabalSpecV3_12
  | v == [3, 8] = Just CabalSpecV3_8
  | v == [3, 6] = Just CabalSpecV3_6
  | v == [3, 4] = Just CabalSpecV3_4
  | v == [3, 0] = Just CabalSpecV3_0
  | v == [2, 4] = Just CabalSpecV2_4
  | v == [2, 2] = Just CabalSpecV2_2
  | v == [2, 0] = Just CabalSpecV2_0
  | v >= [1, 25] = Nothing
  | v >= [1, 23] = Just CabalSpecV1_24
  | v >= [1, 21] = Just CabalSpecV1_22
  | v >= [1, 19] = Just CabalSpecV1_20
  | v >= [1, 17] = Just CabalSpecV1_18
  | v >= [1, 11] = Just CabalSpecV1_12
  | v >= [1, 9] = Just CabalSpecV1_10
  | v >= [1, 7] = Just CabalSpecV1_8
  | v >= [1, 5] = Just CabalSpecV1_6
  | v >= [1, 3] = Just CabalSpecV1_4
  | v >= [1, 1] = Just CabalSpecV1_2
  | otherwise = Just CabalSpecV1_0

-- | @since 3.4.0.0
cabalSpecToVersionDigits :: CabalSpecVersion -> [Int]
cabalSpecToVersionDigits CabalSpecV3_16 = [3, 16]
cabalSpecToVersionDigits CabalSpecV3_14 = [3, 14]
cabalSpecToVersionDigits CabalSpecV3_12 = [3, 12]
cabalSpecToVersionDigits CabalSpecV3_8 = [3, 8]
cabalSpecToVersionDigits CabalSpecV3_6 = [3, 6]
cabalSpecToVersionDigits CabalSpecV3_4 = [3, 4]
cabalSpecToVersionDigits CabalSpecV3_0 = [3, 0]
cabalSpecToVersionDigits CabalSpecV2_4 = [2, 4]
cabalSpecToVersionDigits CabalSpecV2_2 = [2, 2]
cabalSpecToVersionDigits CabalSpecV2_0 = [2, 0]
cabalSpecToVersionDigits CabalSpecV1_24 = [1, 24]
cabalSpecToVersionDigits CabalSpecV1_22 = [1, 22]
cabalSpecToVersionDigits CabalSpecV1_20 = [1, 20]
cabalSpecToVersionDigits CabalSpecV1_18 = [1, 18]
cabalSpecToVersionDigits CabalSpecV1_12 = [1, 12]
cabalSpecToVersionDigits CabalSpecV1_10 = [1, 10]
cabalSpecToVersionDigits CabalSpecV1_8 = [1, 8]
cabalSpecToVersionDigits CabalSpecV1_6 = [1, 6]
cabalSpecToVersionDigits CabalSpecV1_4 = [1, 4]
cabalSpecToVersionDigits CabalSpecV1_2 = [1, 2]
cabalSpecToVersionDigits CabalSpecV1_0 = [1, 0]

-- | What is the minimum Cabal library version which knows how handle
-- this spec version.
--
-- /Note:/ this is a point where we could decouple cabal-spec and Cabal
-- versions, if we ever want that.
--
-- >>> cabalSpecMinimumLibraryVersion CabalSpecV3_0
-- [2,5]
--
-- >>> cabalSpecMinimumLibraryVersion CabalSpecV2_4
-- [2,3]
--
-- @since 3.4.0.0
cabalSpecMinimumLibraryVersion :: CabalSpecVersion -> [Int]
cabalSpecMinimumLibraryVersion CabalSpecV1_0 = [1, 0]
cabalSpecMinimumLibraryVersion csv = case cabalSpecToVersionDigits (pred csv) of
  [x, y] -> [x, y + 1]
  xs -> xs

specHasCommonStanzas :: CabalSpecVersion -> HasCommonStanzas
specHasCommonStanzas v =
  if v >= CabalSpecV2_2
    then HasCommonStanzas
    else NoCommonStanzas

specHasElif :: CabalSpecVersion -> HasElif
specHasElif v =
  if v >= CabalSpecV2_2
    then HasElif
    else NoElif

-------------------------------------------------------------------------------
-- Booleans
-------------------------------------------------------------------------------

-- IDEA: make some kind of tagged booleans?
data HasElif = HasElif | NoElif
  deriving (Eq, Show)

data HasCommonStanzas = HasCommonStanzas | NoCommonStanzas
  deriving (Eq, Show)

data HasGlobstar = HasGlobstar | NoGlobstar
