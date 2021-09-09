{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Solver.Types.SourcePackage
    ( PackageDescriptionOverride
    , SourcePackage(..)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package
         ( PackageId, Package(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..) )

import Data.ByteString.Lazy (ByteString)

-- | A package description along with the location of the package sources.
--
data SourcePackage loc = SourcePackage
  { srcpkgPackageId     :: PackageId
  , srcpkgDescription   :: GenericPackageDescription
    -- ^ Note, this field is lazy, e.g. when reading in hackage index
    --   we parse only what we need, not whole index.
  , srcpkgSource        :: loc
  , srcpkgDescrOverride :: PackageDescriptionOverride
  }
  deriving (Eq, Show, Generic, Typeable)

instance Binary loc => Binary (SourcePackage loc)
instance Structured loc => Structured (SourcePackage loc)

instance Package (SourcePackage a) where packageId = srcpkgPackageId

-- | We sometimes need to override the .cabal file in the tarball with
-- the newer one from the package index.
type PackageDescriptionOverride = Maybe ByteString
