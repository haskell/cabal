module Distribution.Solver.Modular.Log
    ( Log
    , logToProgress
    ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Data.List as L

import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Progress

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.Tree (FailReason(..))
import qualified Distribution.Solver.Modular.ConflictSet as CS

-- | The 'Log' datatype.
--
-- Represents the progress of a computation lazily.
--
-- Parameterized over the type of actual messages and the final result.
type Log m a = Progress m (ConflictSet QPN, ConflictMap) a

messages :: Progress step fail done -> [step]
messages = foldProgress (:) (const []) (const [])

data Exhaustive = Exhaustive | NotExhaustive

-- | Postprocesses a log file. Takes as an argument a limit on allowed backjumps.
-- If the limit is 'Nothing', then infinitely many backjumps are allowed. If the
-- limit is 'Just 0', backtracking is completely disabled.
logToProgress :: Maybe Int -> Log Message a -> Progress String String a
logToProgress mbj l = let
                        es = proc (Just 0) l -- catch first error (always)
                        ms = useFirstError (proc mbj l)
                      in go es es -- trace for first error
                            (showMessages (const True) True ms) -- run with backjump limit applied
  where
    -- Proc takes the allowed number of backjumps and a 'Progress' and explores the
    -- messages until the maximum number of backjumps has been reached. It filters out
    -- and ignores repeated backjumps. If proc reaches the backjump limit, it truncates
    -- the 'Progress' and ends it with the last conflict set. Otherwise, it leaves the
    -- original result.
    proc :: Maybe Int -> Log Message b -> Progress Message (Exhaustive, ConflictSet QPN, ConflictMap) b
    proc _        (Done x)                          = Done x
    proc _        (Fail (cs, cm))                   = Fail (Exhaustive, cs, cm)
    proc mbj'     (Step x@(Failure cs Backjump) xs@(Step Leave (Step (Failure cs' Backjump) _)))
      | cs == cs'                                   = Step x (proc mbj'           xs) -- repeated backjumps count as one
    proc (Just 0) (Step   (Failure cs Backjump)  _) = Fail (NotExhaustive, cs, mempty) -- No final conflict map available
    proc (Just n) (Step x@(Failure _  Backjump) xs) = Step x (proc (Just (n - 1)) xs)
    proc mbj'     (Step x                       xs) = Step x (proc mbj'           xs)

    -- Sets the conflict set from the first backjump as the final error in case of a
    -- non-exhaustive search.
    useFirstError :: Progress Message (Exhaustive, ConflictSet QPN, ConflictMap) b
                  -> Progress Message (Exhaustive, ConflictSet QPN, ConflictMap) b
    useFirstError = replace Nothing
      where
        replace _       (Done x)                          = Done x
        replace _       (Fail (Exhaustive,    cs, cm))    = Fail (Exhaustive, cs, cm)
        replace cs'     (Fail (NotExhaustive, cs, cm))    = -- Backjump limit not reached.
                                                            -- Prefer first error over later error.
                                                            Fail (NotExhaustive, fromMaybe cs cs', cm)
        replace Nothing (Step x@(Failure cs Backjump) xs) = Step x $ replace (Just cs) xs
        replace cs'     (Step x                       xs) = Step x $ replace cs' xs

    -- The first two arguments are both supposed to be the log up to the first error.
    -- That's the error that will always be printed in case we do not find a solution.
    -- We pass this log twice, because we evaluate it in parallel with the full log,
    -- but we also want to retain the reference to its beginning for when we print it.
    -- This trick prevents a space leak!
    --
    -- The third argument is the full log, ending with either the solution or the
    -- exhaustiveness and first conflict set.
    go :: Progress Message a b
       -> Progress Message a b
       -> Progress String (Exhaustive, ConflictSet QPN, ConflictMap) b
       -> Progress String String b
    go ms (Step _ ns) (Step x xs)           = Step x (go ms ns xs)
    go ms r           (Step x xs)           = Step x (go ms r  xs)
    go ms _           (Fail (exh, cs, cm))  = Fail $
        "Could not resolve dependencies:\n" ++
        unlines (messages $ showMessages (L.foldr (\ v _ -> v `CS.member` cs) True) False ms) ++
        case exh of
            Exhaustive ->
                "Dependency tree exhaustively searched.\n" ++
                "I've had most trouble fulfilling the following goals: "
                ++ CS.showCSWithFrequency cm cs
            NotExhaustive ->
                "Backjump limit reached (" ++ currlimit mbj ++
                "change with --max-backjumps or try to run with --reorder-goals).\n"
              where currlimit (Just n) = "currently " ++ show n ++ ", "
                    currlimit Nothing  = ""
    go _  _           (Done s)              = Done s
