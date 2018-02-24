module Distribution.Solver.Modular.Log
    ( logToProgress
    , SolverFailure(..)
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Distribution.Solver.Types.Progress

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Message
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.RetryLog
import Distribution.Verbosity

-- | Information about a dependency solver failure.
data SolverFailure =
    ExhaustiveSearch ConflictSet ConflictMap
  | BackjumpLimitReached

-- | Postprocesses a log file. When the dependency solver fails to find a
-- solution, the log ends with a SolverFailure and a message describing the
-- failure.
logToProgress :: Verbosity
              -> Maybe Int
              -> RetryLog Message SolverFailure a
              -> Progress String (SolverFailure, String) a
logToProgress verbosity mbj lg =
    showMessages $

    -- Convert the RetryLog to a Progress (with toProgress) as late as possible,
    -- to take advantage of efficient updates at failures.
    toProgress $
    mapFailure (\failure -> (failure, finalErrorMsg failure)) lg
  where
    finalErrorMsg :: SolverFailure -> String
    finalErrorMsg (ExhaustiveSearch cs cm) =
        "After searching the rest of the dependency tree exhaustively, "
        ++ "these were the goals I've had most trouble fulfilling: "
        ++ showCS cm cs
      where
        showCS = if verbosity > normal
                 then CS.showCSWithFrequency
                 else CS.showCSSortedByFrequency
    finalErrorMsg BackjumpLimitReached =
        "Backjump limit reached (" ++ currlimit mbj ++
        "change with --max-backjumps or try to run with --reorder-goals).\n"
      where currlimit (Just n) = "currently " ++ show n ++ ", "
            currlimit Nothing  = ""
