{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint(..)
    , unlabelPackageConstraint
    ) where

import GHC.Generics
import Data.Binary (Binary)

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint
import Distribution.Utils.Structured

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint
   = LabeledPackageConstraint PackageConstraint ConstraintSource
  deriving (Generic, Eq, Binary, Structured)

unlabelPackageConstraint :: LabeledPackageConstraint -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc
