{-# LANGUAGE DeriveGeneric #-}

module Distribution.Solver.Types.Stage
  ( Stage (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

data Stage
  = -- | -- The system where the build is running
    Build
  | -- | -- The system where the built artifacts will run
    Host
  deriving (Eq, Ord, Read, Show, Generic, Typeable)

instance Binary Stage
instance Structured Stage
