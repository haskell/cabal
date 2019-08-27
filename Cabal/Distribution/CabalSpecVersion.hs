{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.CabalSpecVersion where

import Prelude ()
import Distribution.Compat.Prelude

-- | Different Cabal-the-spec versions.
--
-- We branch based on this at least in the parser.
--
data CabalSpecVersion
    = CabalSpecV1_0 -- ^ this is older than 'CabalSpecV1_2'
    | CabalSpecV1_2 -- ^ new syntax (sections)
    | CabalSpecV1_4
    | CabalSpecV1_6
    | CabalSpecV1_8
    | CabalSpecV1_10
    | CabalSpecV1_12
    -- 1.16 -- 1.14: no changes
    | CabalSpecV1_18
    | CabalSpecV1_20
    | CabalSpecV1_22
    | CabalSpecV1_24
    | CabalSpecV2_0
    | CabalSpecV2_2
    | CabalSpecV2_4
    | CabalSpecV3_0
    | CabalSpecV3_2
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data, Generic)

-- | Show cabal spec version, but not the way in the .cabal files
--
-- @since 3.0.0.0
showCabalSpecVersion :: CabalSpecVersion -> String
showCabalSpecVersion CabalSpecV3_2  = "3.2"
showCabalSpecVersion CabalSpecV3_0  = "3.0"
showCabalSpecVersion CabalSpecV2_4  = "2.4"
showCabalSpecVersion CabalSpecV2_2  = "2.2"
showCabalSpecVersion CabalSpecV2_0  = "2.0"
showCabalSpecVersion CabalSpecV1_24 = "1.24"
showCabalSpecVersion CabalSpecV1_22 = "1.22"
showCabalSpecVersion CabalSpecV1_20 = "1.20"
showCabalSpecVersion CabalSpecV1_18 = "1.18"
showCabalSpecVersion CabalSpecV1_12 = "1.12"
showCabalSpecVersion CabalSpecV1_10 = "1.10"
showCabalSpecVersion CabalSpecV1_8  = "1.8"
showCabalSpecVersion CabalSpecV1_6  = "1.6"
showCabalSpecVersion CabalSpecV1_4  = "1.4"
showCabalSpecVersion CabalSpecV1_2  = "1.2"
showCabalSpecVersion CabalSpecV1_0  = "1.0"

cabalSpecLatest :: CabalSpecVersion
cabalSpecLatest = CabalSpecV3_2

cabalSpecFromVersionDigits :: [Int] -> CabalSpecVersion
cabalSpecFromVersionDigits v
    | v >= [3,1]  = CabalSpecV3_2
    | v >= [2,5]  = CabalSpecV3_0
    | v >= [2,3]  = CabalSpecV2_4
    | v >= [2,1]  = CabalSpecV2_2
    | v >= [1,25] = CabalSpecV2_0
    | v >= [1,23] = CabalSpecV1_24
    | v >= [1,21] = CabalSpecV1_22
    | v >= [1,19] = CabalSpecV1_20
    | v >= [1,17] = CabalSpecV1_18
    | v >= [1,11] = CabalSpecV1_12
    | v >= [1,9]  = CabalSpecV1_10
    | v >= [1,7]  = CabalSpecV1_8
    | v >= [1,5]  = CabalSpecV1_6
    | v >= [1,3]  = CabalSpecV1_4
    | v >= [1,1]  = CabalSpecV1_2
    | otherwise   = CabalSpecV1_0

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
