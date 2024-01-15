{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
module Distribution.Solver.Types.Progress
    ( Progress(..)
    , foldProgress
    , Message(..)
    , Entry(..)
    , EntryMessage(..)
    , SummarizedMessage(..)
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude hiding (fail)

import Distribution.Solver.Modular.Tree
    ( FailReason(..), POption(..) )
import Distribution.Solver.Types.PackagePath ( QPN )
import Distribution.Solver.Modular.Flag ( QSN, QFN )
import Distribution.Solver.Modular.Dependency
    ( ConflictSet, QGoalReason, GoalReason, Goal )
import qualified Distribution.Solver.Modular.ConflictSet as CS

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- result which may be used to indicate progress and\/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done

-- This Functor instance works around a bug in GHC 7.6.3.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/7436#note_66637.
-- The derived functor instance caused a space leak in the solver.
instance Functor (Progress step fail) where
  fmap f (Step s p) = Step s (fmap f p)
  fmap _ (Fail x)   = Fail x
  fmap f (Done r)   = Done (f r)

-- | Consume a 'Progress' calculation. Much like 'foldr' for lists but with two
-- base cases, one for a final result and one for failure.
--
-- Eg to convert into a simple 'Either' result use:
--
-- > foldProgress (flip const) Left Right
--
foldProgress :: (step -> a -> a) -> (fail -> a) -> (done -> a)
             -> Progress step fail done -> a
foldProgress step fail done = fold
  where fold (Step s p) = step s (fold p)
        fold (Fail f)   = fail f
        fold (Done r)   = done r

instance Monad (Progress step fail) where
  return   = pure
  p >>= f  = foldProgress Step Fail f p

instance Applicative (Progress step fail) where
  pure a  = Done a
  p <*> x = foldProgress Step Fail (flip fmap x) p

instance Monoid fail => Alternative (Progress step fail) where
  empty   = Fail mempty
  p <|> q = foldProgress Step (const q) Done p

data Message =
    Enter           -- ^ increase indentation level
  | Leave           -- ^ decrease indentation level
  | TryP QPN POption
  | TryF QFN Bool
  | TryS QSN Bool
  | Next (Goal QPN)
  | Skip (Set CS.Conflict)
  | Success
  | Failure ConflictSet FailReason

data Entry
  = LogPackageGoal QPN QGoalReason
  | LogRejectF QFN Bool ConflictSet FailReason
  | LogRejectS QSN Bool ConflictSet FailReason
  | LogSkipping (Set CS.Conflict)
  | LogTryingF QFN Bool
  | LogTryingP QPN POption (Maybe (GoalReason QPN))
  | LogTryingS QSN Bool
  | LogRejectMany QPN [POption] ConflictSet FailReason
  | LogSkipMany QPN [POption] (Set CS.Conflict)
  | LogUnknownPackage QPN (GoalReason QPN)
  | LogSuccessMsg
  | LogFailureMsg ConflictSet FailReason
  deriving stock (Show, Eq)

data EntryMessage = AtLevel Int Entry
  deriving stock (Show, Eq)

data SummarizedMessage = SummarizedMessage EntryMessage | ErrorMessage String
  deriving stock (Show, Eq)