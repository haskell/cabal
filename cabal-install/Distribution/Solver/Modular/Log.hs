module Distribution.Solver.Modular.Log
    ( logToProgress
    , SolverFailure(..)
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Distribution.Solver.Types.Progress

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.RetryLog

-- | Information about a dependency solver failure.
data SolverFailure =
    ExhaustiveSearch ConflictSet ConflictMap
  | BackjumpLimitReached

-- | Postprocesses a log file. This function discards all log messages and
-- avoids calling 'showMessages' if the log isn't needed (specified by
-- 'keepLog'), for efficiency.
logToProgress :: Bool
              -> RetryLog Message SolverFailure a
              -> Progress String SolverFailure a
logToProgress keepLog lg =
    if keepLog
    then showMessages progress
    else foldProgress (const id) Fail Done progress
  where
    progress = toProgress lg
