module Distribution.Client.Types.ReadyPackage
  ( GenericReadyPackage (..)
  , ReadyPackage
  ) where

import Distribution.Client.Types.ConfiguredPackage (ConfiguredPackage)
import Distribution.Client.Types.GenericReadyPackage (GenericReadyPackage (..))
import Distribution.Client.Types.PackageLocation (UnresolvedPkgLoc)

type ReadyPackage = GenericReadyPackage (ConfiguredPackage UnresolvedPkgLoc)
