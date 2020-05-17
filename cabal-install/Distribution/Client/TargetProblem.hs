module Distribution.Client.TargetProblem
  ( TargetProblem(..),
    TargetProblem',
    TargetProblemCommon(..),
    commonTargetProblem,
    noneEnabledTargetProblem,
    noTargetsProblem,
    customTargetProblem,
  )
where

import Distribution.Client.Compat.Prelude
import Distribution.Client.ProjectPlanning
  ( AvailableTarget,
  )
import Distribution.Client.TargetSelector
  ( TargetSelector, SubComponentTarget,
  )
import Distribution.Package
  (PackageId, PackageName,
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName,
  )
import Distribution.Simple.LocalBuildInfo
  ( ComponentName(..),
  )
import Prelude ()

-- | Target problems that occur during project orchestration.
data TargetProblemCommon
   = TargetNotInProject                   PackageName
   | TargetAvailableInIndex               PackageName

   | TargetComponentNotProjectLocal
     PackageId ComponentName SubComponentTarget

   | TargetComponentNotBuildable
     PackageId ComponentName SubComponentTarget

   | TargetOptionalStanzaDisabledByUser
     PackageId ComponentName SubComponentTarget

   | TargetOptionalStanzaDisabledBySolver
     PackageId ComponentName SubComponentTarget

   | TargetProblemUnknownComponent
     PackageName (Either UnqualComponentName ComponentName)

    -- The target matching stuff only returns packages local to the project,
    -- so these lookups should never fail, but if 'resolveTargets' is called
    -- directly then of course it can.
   | TargetProblemNoSuchPackage           PackageId
   | TargetProblemNoSuchComponent         PackageId ComponentName
  deriving (Eq, Show)



-- | Type alias for a 'TargetProblem' with no user-defined problems/errors.
--
-- Can use the utilities below for reporting/rendering problems.
type TargetProblem' = TargetProblem ()

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's which can be extended
-- with command specific target problems as described by 'e'.
data TargetProblem e
  = CommonProblem TargetProblemCommon
  | -- | The 'TargetSelector' matches benchmarks but none are buildable
    NoneEnabled TargetSelector [AvailableTarget ()]
  | -- | There are no targets at all
    NoTargets TargetSelector
  | -- | A custom target problem
    CustomProblem e
  deriving (Eq, Show)

commonTargetProblem :: TargetProblemCommon -> TargetProblem e
commonTargetProblem = CommonProblem

noneEnabledTargetProblem :: TargetSelector -> [AvailableTarget ()] -> TargetProblem e
noneEnabledTargetProblem = NoneEnabled

noTargetsProblem :: TargetSelector -> TargetProblem e
noTargetsProblem = NoTargets

customTargetProblem :: e -> TargetProblem e
customTargetProblem = CustomProblem
