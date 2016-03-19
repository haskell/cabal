module Distribution.Client.Dependency.Modular.Log
    ( Log
    , continueWith
    , failWith
    , logToProgress
    , succeedWith
    , tryWith
    ) where

import Control.Applicative
import Data.List as L
import Data.Maybe (isNothing)

import Distribution.Client.Dependency.Types -- from Cabal

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree (FailReason(..))
import qualified Distribution.Client.Dependency.Modular.ConflictSet as CS

-- | The 'Log' datatype.
--
-- Represents the progress of a computation lazily.
--
-- Parameterized over the type of actual messages and the final result.
type Log m a = Progress m () a

messages :: Progress step fail done -> [step]
messages = foldProgress (:) (const []) (const [])

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
    -- original success result or replaces the original failure with 'Nothing'.
    proc :: Maybe Int -> Progress Message a b -> Progress Message (Maybe (ConflictSet QPN)) b
    proc _        (Done x)                          = Done x
    proc _        (Fail _)                          = Fail Nothing
    proc mbj'     (Step   (Failure cs Backjump) xs@(Step Leave (Step (Failure cs' Backjump) _)))
      | cs == cs'                                   = proc mbj' xs -- repeated backjumps count as one
    proc (Just 0) (Step   (Failure cs Backjump)  _) = Fail (Just cs)
    proc (Just n) (Step x@(Failure _  Backjump) xs) = Step x (proc (Just (n - 1)) xs)
    proc mbj'     (Step x                       xs) = Step x (proc mbj'           xs)

    -- Sets the conflict set from the first backjump as the final error, and records
    -- whether the search was exhaustive.
    useFirstError :: Progress Message (Maybe (ConflictSet QPN)) b
                  -> Progress Message (Bool, Maybe (ConflictSet QPN)) b
    useFirstError = replace Nothing
      where
        replace _       (Done x)                          = Done x
        replace cs'     (Fail cs)                         = -- 'Nothing' means backjump limit not reached.
                                                            -- Prefer first error over later error.
                                                            Fail (isNothing cs, cs' <|> cs)
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
       -> Progress String (Bool, Maybe (ConflictSet QPN)) b
       -> Progress String String b
    go ms (Step _ ns) (Step x xs)           = Step x (go ms ns xs)
    go ms r           (Step x xs)           = Step x (go ms r  xs)
    go ms _           (Fail (exh, Just cs)) = Fail $
                                              "Could not resolve dependencies:\n" ++
                                              unlines (messages $ showMessages (L.foldr (\ v _ -> v `CS.member` cs) True) False ms) ++
                                              (if exh then "Dependency tree exhaustively searched.\n"
                                                      else "Backjump limit reached (" ++ currlimit mbj ++
                                                               "change with --max-backjumps or try to run with --reorder-goals).\n")
                                                  where currlimit (Just n) = "currently " ++ show n ++ ", "
                                                        currlimit Nothing  = ""
    go _  _           (Done s)              = Done s
    go _  _           (Fail (_, Nothing))   = Fail ("Could not resolve dependencies; something strange happened.") -- should not happen

failWith :: step -> fail -> Progress step fail done
failWith s f = Step s (Fail f)

succeedWith :: step -> done -> Progress step fail done
succeedWith s d = Step s (Done d)

continueWith :: step -> Progress step fail done -> Progress step fail done
continueWith = Step

tryWith :: Message
        -> Progress Message fail done
        -> Progress Message fail done
tryWith m = Step m . Step Enter . foldProgress Step (failWith Leave) Done
