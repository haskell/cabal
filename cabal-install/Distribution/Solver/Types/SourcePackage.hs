{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.SourcePackage
    ( PackageDescriptionOverride
    , SourcePackage(..)
    ) where

import Distribution.Package
         ( PackageId, Package(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..) )

import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary(..))

-- | A package description along with the location of the package sources.
--
data SourcePackage loc = SourcePackage {
    packageInfoId        :: PackageId,
    packageDescription   :: GenericPackageDescription,
    packageSource        :: loc,
    packageDescrOverride :: PackageDescriptionOverride
  }
  deriving (Eq, Show, Generic)

instance (Binary loc) => Binary (SourcePackage loc)

instance Package (SourcePackage a) where packageId = packageInfoId

-- | We sometimes need to override the .cabal file in the tarball with
-- the newer one from the package index.
type PackageDescriptionOverride = Maybe ByteString
