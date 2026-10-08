{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Distribution.Solver.Types.InstalledPreference
    ( InstalledPreference(..),
    ) where

import Prelude (Show)
import Data.Binary (Binary)
import GHC.Generics (Generic)

-- | Whether we prefer an installed version of a package or simply the latest
-- version.
--
data InstalledPreference = PreferInstalled | PreferLatest | PreferOldest
  deriving (Show, Generic, Binary)
