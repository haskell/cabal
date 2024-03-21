module Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint(..)
    , unlabelPackageConstraint
    ) where

import Distribution.Solver.Types.PackageConstraint

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint cs
   = LabeledPackageConstraint PackageConstraint cs

unlabelPackageConstraint :: LabeledPackageConstraint cs -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc
