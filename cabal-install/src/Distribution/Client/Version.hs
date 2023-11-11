-- | Provides the version number of @cabal-install@.

module Distribution.Client.Version
  ( cabalInstallVersion
  ) where

import Distribution.Version

import qualified Paths_cabal_install as PackageInfo

-- |
-- This value determines the output of `cabal-install --version`.
cabalInstallVersion :: Version
<<<<<<< HEAD
cabalInstallVersion = mkVersion [3,10,2,1]
=======
cabalInstallVersion = mkVersion' PackageInfo.version
>>>>>>> 5df009cae (Use Paths_cabal_install for cabal-install version number (#9421))
