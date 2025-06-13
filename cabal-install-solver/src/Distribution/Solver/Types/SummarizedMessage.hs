module Distribution.Solver.Types.SummarizedMessage
    ( Entry(..)
    , EntryAtLevel(..)
    , SummarizedMessage(..)
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude hiding (fail)

import Distribution.Solver.Modular.Tree
    ( FailReason(..), POption(..) )
import Distribution.Solver.Types.PackagePath ( QPN )
import Distribution.Solver.Modular.Flag ( QSN, QFN )
import Distribution.Solver.Modular.Dependency
    ( ConflictSet, QGoalReason, GoalReason )
import qualified Distribution.Solver.Modular.ConflictSet as CS

data Entry
  = EntryPackageGoal QPN QGoalReason
  | EntryRejectF QFN Bool ConflictSet FailReason
  | EntryRejectS QSN Bool ConflictSet FailReason
  | EntrySkipping (Set CS.Conflict)
  | EntryTryingF QFN Bool
  | EntryTryingP QPN POption
  | EntryTryingNewP QPN POption (GoalReason QPN)
  | EntryTryingS QSN Bool
  | EntryRejectMany QPN [POption] ConflictSet FailReason
  | EntrySkipMany QPN [POption] (Set CS.Conflict)
  | EntryUnknownPackage QPN (GoalReason QPN)
  | EntrySuccess
  | EntryFailure ConflictSet FailReason

data EntryAtLevel = AtLevel Int Entry

data SummarizedMessage = SummarizedMsg EntryAtLevel | ErrorMsg String
