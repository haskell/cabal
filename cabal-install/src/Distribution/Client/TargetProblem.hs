{-# LANGUAGE DeriveFunctor #-}

module Distribution.Client.TargetProblem
  ( TargetProblem (..)
  , TargetProblem'
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ProjectPlanning (AvailableTarget)
import Distribution.Client.TargetSelector (SubComponentTarget, TargetSelector)
import Distribution.Package (PackageId, PackageName)
import Distribution.Simple.LocalBuildInfo (ComponentName (..))
import Distribution.Types.UnqualComponentName (UnqualComponentName)

-- | Target problems that occur during project orchestration.
data TargetProblem a
  = TargetNotInProject PackageName
  | TargetAvailableInIndex PackageName
  | TargetComponentNotProjectLocal
      PackageId
      ComponentName
      SubComponentTarget
  | TargetComponentNotBuildable
      PackageId
      ComponentName
      SubComponentTarget
  | TargetOptionalStanzaDisabledByUser
      PackageId
      ComponentName
      SubComponentTarget
  | TargetOptionalStanzaDisabledBySolver
      PackageId
      ComponentName
      SubComponentTarget
  | TargetProblemUnknownComponent
      PackageName
      (Either UnqualComponentName ComponentName)
  | -- | The 'TargetSelector' matches component (test/benchmark/...) but none are buildable
    TargetProblemNoneEnabled TargetSelector [AvailableTarget ()]
  | -- | There are no targets at all
    TargetProblemNoTargets TargetSelector
  | -- The target matching stuff only returns packages local to the project,
    -- so these lookups should never fail, but if 'resolveTargets' is called
    -- directly then of course it can.
    TargetProblemNoSuchPackage PackageId
  | TargetProblemNoSuchComponent PackageId ComponentName
  | -- | A custom target problem
    CustomTargetProblem a
  deriving (Eq, Show, Functor)

-- | Type alias for a 'TargetProblem' with no user-defined problems/errors.
--
-- Can use the utilities below for reporting/rendering problems.
type TargetProblem' = TargetProblem Void
