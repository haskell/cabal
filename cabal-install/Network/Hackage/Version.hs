module Network.Hackage.Version where

import Data.Version
import Paths_cabal_install (version)

clientVersion :: Version
clientVersion = Paths_cabal_install.version
