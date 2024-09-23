module Distribution.Types.DependencySatisfaction
  ( DependencySatisfaction (..)
  ) where

import Distribution.Types.MissingDependencyReason (MissingDependencyReason)

-- | Whether or not a dependency constraint is satisfied.
data DependencySatisfaction
  = -- | The dependency constraint is satisfied.
    Satisfied
  | -- | The dependency constraint is not satisfied.
    --
    -- Includes a reason for explanation.
    Unsatisfied MissingDependencyReason
