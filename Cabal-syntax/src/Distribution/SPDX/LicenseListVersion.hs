module Distribution.SPDX.LicenseListVersion
  ( LicenseListVersion (..)
  , cabalSpecVersionToSPDXListVersion
  ) where

import Distribution.CabalSpecVersion

-- | SPDX License List version @Cabal@ is aware of.
data LicenseListVersion
  = LicenseListVersion_3_0
  | LicenseListVersion_3_2
  | LicenseListVersion_3_6
  | LicenseListVersion_3_9
  | LicenseListVersion_3_10
  | LicenseListVersion_3_16
  | LicenseListVersion_3_23
  deriving (Eq, Ord, Show, Enum, Bounded)

cabalSpecVersionToSPDXListVersion :: CabalSpecVersion -> LicenseListVersion
cabalSpecVersionToSPDXListVersion CabalSpecV3_12 = LicenseListVersion_3_23
cabalSpecVersionToSPDXListVersion CabalSpecV3_8 = LicenseListVersion_3_16
cabalSpecVersionToSPDXListVersion CabalSpecV3_6 = LicenseListVersion_3_10
cabalSpecVersionToSPDXListVersion CabalSpecV3_4 = LicenseListVersion_3_9
cabalSpecVersionToSPDXListVersion CabalSpecV3_0 = LicenseListVersion_3_6
cabalSpecVersionToSPDXListVersion CabalSpecV2_4 = LicenseListVersion_3_2
cabalSpecVersionToSPDXListVersion _ = LicenseListVersion_3_0
