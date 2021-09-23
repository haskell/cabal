-- | Provides the version number of @cabal-install@.

module Distribution.Client.Version
  ( cabalInstallVersion
  ) where

import Distribution.Version

-- TODO: write a test around this. Don't abuse Paths_cabal_install.
--
cabalInstallVersion :: Version
cabalInstallVersion = mkVersion [3,6]
