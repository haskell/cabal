module Distribution.Solver.Modular.Log
    ( displayLogMessages
    , SolverFailure(..)
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Distribution.Solver.Types.Progress
    ( Progress(Done, Fail), foldProgress )
import Distribution.Solver.Modular.ConflictSet
    ( ConflictMap, ConflictSet )
import Distribution.Solver.Modular.RetryLog
    ( RetryLog, toProgress, fromProgress )
import Distribution.Solver.Modular.Message (Message, summarizeMessages)
import Distribution.Solver.Types.SummarizedMessage
    ( SummarizedMessage(..) )
-- | Information about a dependency solver failure.
data SolverFailure =
    ExhaustiveSearch ConflictSet ConflictMap
  | BackjumpLimitReached

-- | Postprocesses a log file. This function discards all log messages and
-- avoids calling 'showMessages' if the log isn't needed (specified by
-- 'keepLog'), for efficiency.
displayLogMessages :: Bool
                   -> RetryLog Message SolverFailure a
                   -> RetryLog SummarizedMessage SolverFailure a
displayLogMessages keepLog lg = fromProgress $
    if keepLog
    then summarizeMessages progress
    else foldProgress (const id) Fail Done progress
  where
    progress = toProgress lg
