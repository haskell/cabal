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
    | CabalSpecV20
    | CabalSpecV22
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data, Generic)

cabalSpecLatest :: CabalSpecVersion
cabalSpecLatest = CabalSpecV22

cabalSpecFeatures :: CabalSpecVersion -> Set.Set CabalFeature
cabalSpecFeatures CabalSpecOld = Set.empty
cabalSpecFeatures CabalSpecV20 = Set.empty
cabalSpecFeatures CabalSpecV22 = Set.fromList
    [ Elif
    , CommonStanzas
    ]

cabalSpecSupports :: CabalSpecVersion -> [Int] -> Bool
cabalSpecSupports CabalSpecOld v = v < [1,25]
cabalSpecSupports CabalSpecV20 v = v < [2,1]
cabalSpecSupports CabalSpecV22 _ = True

specHasCommonStanzas :: CabalSpecVersion -> HasCommonStanzas
specHasCommonStanzas CabalSpecV22 = HasCommonStanzas
specHasCommonStanzas _            = NoCommonStanzas

specHasElif :: CabalSpecVersion -> HasElif
specHasElif CabalSpecV22 = HasElif
specHasElif _            = NoElif

-------------------------------------------------------------------------------
-- Features
-------------------------------------------------------------------------------

data CabalFeature
    = Elif
    | CommonStanzas
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data, Generic)

-------------------------------------------------------------------------------
-- Booleans
-------------------------------------------------------------------------------

data HasElif = HasElif | NoElif
  deriving (Eq, Show)

data HasCommonStanzas = HasCommonStanzas | NoCommonStanzas
  deriving (Eq, Show)
