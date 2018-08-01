module Distribution.SPDX.LicenseListVersion (
    LicenseListVersion (..),
    cabalSpecVersionToSPDXListVersion,
    ) where

import Distribution.CabalSpecVersion

-- | SPDX License List version @Cabal@ is aware of.
data LicenseListVersion
    = LicenseListVersion_3_0
    | LicenseListVersion_3_2
  deriving (Eq, Ord, Show, Enum, Bounded)

cabalSpecVersionToSPDXListVersion :: CabalSpecVersion -> LicenseListVersion
cabalSpecVersionToSPDXListVersion CabalSpecV2_4 = LicenseListVersion_3_2
cabalSpecVersionToSPDXListVersion _             = LicenseListVersion_3_0
