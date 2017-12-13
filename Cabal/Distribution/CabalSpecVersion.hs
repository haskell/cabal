{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Distribution.CabalSpecVersion where

import Distribution.Parsec.Class (Parsec (..), ParsecParser)

-- A class to select how to parse different fields.
class CabalSpecVersion v where
    -- | @v@ can act as own proxy
    cabalSpecVersion   :: v

    -- | Parsec parser according to the spec version
    specParsec         :: Parsec a => v -> ParsecParser a

    -- given a version, whether this spec knows about it's fields
    specKnows              :: v -> [Int] -> Bool

    specHasElif            :: v -> HasElif
    specHasCommonStanzas   :: v -> HasCommonStanzas

data CabalSpecOld = CabalSpecOld
data CabalSpecV20 = CabalSpecV20
data CabalSpecV22 = CabalSpecV22

instance CabalSpecVersion CabalSpecOld where
    cabalSpecVersion       = CabalSpecOld
    specParsec _           = parsec
    specKnows _ vs         = vs < [1,25]
    specHasElif _          = NoElif
    specHasCommonStanzas _ = NoCommonStanzas

instance CabalSpecVersion CabalSpecV20  where
    cabalSpecVersion       = CabalSpecV20
    specParsec _           = parsec
    specKnows _ vs         = vs < [2,1]
    specHasElif _          = NoElif
    specHasCommonStanzas _ = NoCommonStanzas

instance CabalSpecVersion CabalSpecV22  where
    cabalSpecVersion       = CabalSpecV22
    specParsec _           = parsec22
    specKnows _ _          = True
    specHasElif _          = HasElif
    specHasCommonStanzas _ = HasCommonStanzas

type CabalSpecLatest = CabalSpecV22

-------------------------------------------------------------------------------
-- "Booleans"
-------------------------------------------------------------------------------

data HasElif = HasElif | NoElif
  deriving (Eq, Show)

data HasCommonStanzas = HasCommonStanzas | NoCommonStanzas
  deriving (Eq, Show)
