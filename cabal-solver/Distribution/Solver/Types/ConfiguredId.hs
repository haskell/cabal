{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.ConfiguredId
    ( ConfiguredId(..)
    ) where

import Distribution.Compat.Binary (Binary(..))
import GHC.Generics (Generic)
import Distribution.Package (PackageId, UnitId)

-- | A ConfiguredId is a package ID for a configured package.
--
-- Once we configure a source package we know it's UnitId
-- (at least, in principle, even if we have to fake it currently). It is still
-- however useful in lots of places to also know the source ID for the package.
-- We therefore bundle the two.
--
-- An already installed package of course is also "configured" (all it's
-- configuration parameters and dependencies have been specified).
--
-- TODO: I wonder if it would make sense to promote this datatype to Cabal
-- and use it consistently instead of UnitIds?
data ConfiguredId = ConfiguredId {
    confSrcId  :: PackageId
  , confInstId :: UnitId
  }
  deriving (Eq, Generic)

instance Binary ConfiguredId

instance Show ConfiguredId where
  show = show . confSrcId
