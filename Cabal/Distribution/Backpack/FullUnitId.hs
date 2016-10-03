{-# LANGUAGE DeriveGeneric #-}
module Distribution.Backpack.FullUnitId (
    FullUnitId(..),
    FullDb,
    expandIndefUnitId,
    expandUnitId
) where

import Distribution.Backpack
import Distribution.Package
import Distribution.Compat.Prelude

-- Unlike IndefUnitId, which could direct to a UnitId.
data FullUnitId = FullUnitId ComponentId IndefModuleSubst
    deriving (Show, Generic)

type FullDb = UnitId -> FullUnitId

expandIndefUnitId :: FullDb -> IndefUnitId -> FullUnitId
expandIndefUnitId _db (IndefFullUnitId cid subst)
    = FullUnitId cid subst
expandIndefUnitId db (IndefUnitId uid)
    = expandUnitId db uid

expandUnitId :: FullDb -> UnitId -> FullUnitId
expandUnitId db uid = db uid
