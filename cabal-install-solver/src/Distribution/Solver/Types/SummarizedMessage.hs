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

-- The following types are used to encode log messages from the
-- dependency solver so they can be easily displayed in the module
-- `Distribution.Solver.Modular.Message`.
--
--  These types are an intermediate representation of the solver log.
-- The log is converted from a list of Message to a list of
-- SummarizedMessage to a list of String. Message is very similar to
-- the structure of the search tree but difficult to read, and
-- SummarizedMessage has the structure that is shown to users.

-- Encoding of solver messages.
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

-- Encode the level at which the solver message occurred.
data EntryAtLevel = AtLevel Int Entry

-- Messages from the solver.
data SummarizedMessage = SummarizedMsg EntryAtLevel | StringMsg String
