-- | Provides the version number of @cabal-install@.
module Distribution.Client.Version
  ( cabalInstallVersion
  ) where

import Distribution.Version

import qualified Paths_cabal_install as PackageInfo

-- |
-- This value determines the output of `cabal-install --version`.
cabalInstallVersion :: Version
cabalInstallVersion = mkVersion' PackageInfo.version
