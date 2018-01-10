module Distribution.Solver.Modular.Log
    ( Log
    , logToProgress
    , SolverFailure(..)
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Distribution.Solver.Types.Progress

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.Tree (FailReason(..))
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Verbosity

-- | The 'Log' datatype.
--
-- Represents the progress of a computation lazily.
--
-- Parameterized over the type of actual messages and the final result.
type Log m a = Progress m (ConflictSet, ConflictMap) a

data Exhaustiveness = Exhaustive | BackjumpLimit

-- | Information about a dependency solver failure. It includes an error message
-- and a final conflict set, if available.
data SolverFailure =
    NoSolution ConflictSet String
  | BackjumpLimitReached String

-- | Postprocesses a log file. Takes as an argument a limit on allowed backjumps.
-- If the limit is 'Nothing', then infinitely many backjumps are allowed. If the
-- limit is 'Just 0', backtracking is completely disabled.
logToProgress :: Verbosity -> Maybe Int -> Log Message a -> Progress String SolverFailure a
logToProgress verbosity mbj l =
    let ms = proc mbj l
        mapFailure f = foldProgress Step (Fail . f) Done
    in mapFailure finalError (showMessages (const True) True ms) -- run with backjump limit applied
  where
    -- Proc takes the allowed number of backjumps and a 'Progress' and explores the
    -- messages until the maximum number of backjumps has been reached. It filters out
    -- and ignores repeated backjumps. If proc reaches the backjump limit, it truncates
    -- the 'Progress' and ends it with the last conflict set. Otherwise, it leaves the
    -- original result.
    proc :: Maybe Int -> Log Message b -> Progress Message (Exhaustiveness, ConflictSet, ConflictMap) b
    proc _        (Done x)                          = Done x
    proc _        (Fail (cs, cm))                   = Fail (Exhaustive, cs, cm)
    proc mbj'     (Step x@(Failure cs Backjump) xs@(Step Leave (Step (Failure cs' Backjump) _)))
      | cs == cs'                                   = Step x (proc mbj'           xs) -- repeated backjumps count as one
    proc (Just 0) (Step   (Failure cs Backjump)  _) = Fail (BackjumpLimit, cs, mempty) -- No final conflict map available
    proc (Just n) (Step x@(Failure _  Backjump) xs) = Step x (proc (Just (n - 1)) xs)
    proc mbj'     (Step x                       xs) = Step x (proc mbj'           xs)

    finalError :: (Exhaustiveness, ConflictSet, ConflictMap) -> SolverFailure
    finalError (exh, cs, cm) =
        case exh of
            Exhaustive ->
                NoSolution cs $
                "After searching the rest of the dependency tree exhaustively, "
                ++ "these were the goals I've had most trouble fulfilling: "
                ++ showCS cm cs
              where
                showCS = if verbosity > normal
                         then CS.showCSWithFrequency
                         else CS.showCSSortedByFrequency
            BackjumpLimit ->
                BackjumpLimitReached $
                "Backjump limit reached (" ++ currlimit mbj ++
                "change with --max-backjumps or try to run with --reorder-goals).\n"
              where currlimit (Just n) = "currently " ++ show n ++ ", "
                    currlimit Nothing  = ""
