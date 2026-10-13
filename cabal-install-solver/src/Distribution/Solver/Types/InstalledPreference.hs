{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Distribution.Solver.Types.InstalledPreference
    ( InstalledPreference(..),
    ) where

import Prelude (Show, Eq)
import Data.Binary (Binary)
import GHC.Generics (Generic)

import Distribution.Utils.Structured

-- | Whether we prefer an installed version of a package or simply the latest
-- version.
--
data InstalledPreference = PreferInstalled | PreferLatest | PreferOldest
  deriving (Show, Eq, Generic, Binary, Structured)
