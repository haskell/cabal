{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.CabalSpecVersion where

import Prelude ()
import Distribution.Compat.Prelude
import qualified Data.Set as Set

-- | Different Cabal-the-spec versions.
--
-- We branch based on this at least in the parser.
--
data CabalSpecVersion
    = CabalSpecOld
    | CabalSpecV1_22
    | CabalSpecV1_24
    | CabalSpecV2_0
    | CabalSpecV2_2
    | CabalSpecV2_4
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data, Generic)

cabalSpecLatest :: CabalSpecVersion
cabalSpecLatest = CabalSpecV2_4

cabalSpecFeatures :: CabalSpecVersion -> Set.Set CabalFeature
cabalSpecFeatures CabalSpecOld   = Set.empty
cabalSpecFeatures CabalSpecV1_22 = Set.empty
cabalSpecFeatures CabalSpecV1_24 = Set.empty
cabalSpecFeatures CabalSpecV2_0  = Set.empty
cabalSpecFeatures CabalSpecV2_2  = Set.fromList
    [ Elif
    , CommonStanzas
    ]
cabalSpecFeatures CabalSpecV2_4  = Set.fromList
    [ Elif
    , CommonStanzas
    , Globstar
    ]

cabalSpecSupports :: CabalSpecVersion -> [Int] -> Bool
cabalSpecSupports CabalSpecOld v   = v < [1,21]
cabalSpecSupports CabalSpecV1_22 v = v < [1,23]
cabalSpecSupports CabalSpecV1_24 v = v < [1,25]
cabalSpecSupports CabalSpecV2_0 v  = v < [2,1]
cabalSpecSupports CabalSpecV2_2 v  = v < [2,3]
cabalSpecSupports CabalSpecV2_4 _  = True

specHasCommonStanzas :: CabalSpecVersion -> HasCommonStanzas
specHasCommonStanzas CabalSpecV2_2 = HasCommonStanzas
specHasCommonStanzas CabalSpecV2_4 = HasCommonStanzas
specHasCommonStanzas _             = NoCommonStanzas

specHasElif :: CabalSpecVersion -> HasElif
specHasElif CabalSpecV2_2 = HasElif
specHasElif CabalSpecV2_4 = HasElif
specHasElif _             = NoElif

-------------------------------------------------------------------------------
-- Features
-------------------------------------------------------------------------------

data CabalFeature
    = Elif
    | CommonStanzas
    | Globstar
      -- ^ Implemented in #5284. Not actually a change to the parser,
      -- as filename patterns are opaque to it currently.
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data, Generic)

-------------------------------------------------------------------------------
-- Booleans
-------------------------------------------------------------------------------

data HasElif = HasElif | NoElif
  deriving (Eq, Show)

data HasCommonStanzas = HasCommonStanzas | NoCommonStanzas
  deriving (Eq, Show)

data HasGlobstar = HasGlobstar | NoGlobstar
