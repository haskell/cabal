module Distribution.Client.Dependency.Modular.Log where

import Control.Applicative
import Data.List as L
import Data.Set as S

import Distribution.Client.Dependency.Types -- from Cabal

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree (FailReason(..))

-- | The 'Log' datatype.
--
-- Represents the progress of a computation lazily.
--
-- Parameterized over the type of actual messages and the final result.
type Log m a = Progress m () a

-- | Turns a log into a list of messages paired with a final result. A final result
-- of 'Nothing' indicates failure. A final result of 'Just' indicates success.
-- Keep in mind that forcing the second component of the returned pair will force the
-- entire log.
runLog :: Log m a -> ([m], Maybe a)
runLog (Done x)       = ([], Just x)
runLog (Fail _)       = ([], Nothing)
runLog (Step m p)     = let
                          (ms, r) = runLog p
                        in
                          (m : ms, r)

-- | Postprocesses a log file. Takes as an argument a limit on allowed backjumps.
-- If the limit is 'Nothing', then infinitely many backjumps are allowed. If the
-- limit is 'Just 0', backtracking is completely disabled.
logToProgress :: Maybe Int -> Log Message a -> Progress String String a
logToProgress mbj l = let
                        (ms, s) = runLog l
                        -- 'Nothing' for 's' means search tree exhaustively searched and failed
                        (es, e) = proc 0 ms -- catch first error (always)
                        -- 'Nothing' in 'e' means no backjump found
                        (ns, t) = case mbj of
                                     Nothing -> (ms, Nothing)
                                     Just n  -> proc n ms
                        -- 'Nothing' in 't' means backjump limit not reached
                        -- prefer first error over later error
                        (exh, r) = case t of
                                     -- backjump limit not reached
                                     Nothing -> case s of
                                                  Nothing -> (True, e) -- failed after exhaustive search
                                                  Just _  -> (True, Nothing) -- success
                                     -- backjump limit reached; prefer first error
                                     Just _  -> (False, e) -- failed after backjump limit was reached
                      in go es es -- trace for first error
                            (showMessages (const True) True ns) -- shortened run
                            r s exh
  where
    -- Proc takes the allowed number of backjumps and a list of messages and explores the
    -- message list until the maximum number of backjumps has been reached. The log until
    -- that point as well as whether we have encountered an error or not are returned.
    proc :: Int -> [Message] -> ([Message], Maybe (ConflictSet QPN))
    proc _ []                             = ([], Nothing)
    proc n (   Failure cs Backjump  : xs@(Leave : Failure cs' Backjump : _))
      | cs == cs'                         = proc n xs -- repeated backjumps count as one
    proc 0 (   Failure cs Backjump  : _ ) = ([], Just cs)
    proc n (x@(Failure _  Backjump) : xs) = (\ ~(ys, r) -> (x : ys, r)) (proc (n - 1) xs)
    proc n (x                       : xs) = (\ ~(ys, r) -> (x : ys, r)) (proc  n      xs)

    -- This function takes a lot of arguments. The first two are both supposed to be
    -- the log up to the first error. That's the error that will always be printed in
    -- case we do not find a solution. We pass this log twice, because we evaluate it
    -- in parallel with the full log, but we also want to retain the reference to its
    -- beginning for when we print it. This trick prevents a space leak!
    --
    -- The third argument is the full log, the fifth and six error conditions.
    -- The seventh argument indicates whether the search was exhaustive.
    --
    -- The order of arguments is important! In particular 's' must not be evaluated
    -- unless absolutely necessary. It contains the final result, and if we shortcut
    -- with an error due to backjumping, evaluating 's' would still require traversing
    -- the entire tree.
    go ms (_ : ns) (x : xs) r         s        exh = Step x (go ms ns xs r s exh)
    go ms []       (x : xs) r         s        exh = Step x (go ms [] xs r s exh)
    go ms _        []       (Just cs) _        exh = Fail $
                                                     "Could not resolve dependencies:\n" ++
                                                     unlines (showMessages (L.foldr (\ v _ -> v `S.member` cs) True) False ms) ++
                                                     (if exh then "Dependency tree exhaustively searched.\n"
                                                             else "Backjump limit reached (change with --max-backjumps).\n")
    go _  _        []       _         (Just s) _   = Done s
    go _  _        []       _         _        _   = Fail ("Could not resolve dependencies; something strange happened.") -- should not happen

logToProgress' :: Log Message a -> Progress String String a
logToProgress' l = let
                    (ms, r) = runLog l
                    xs      = showMessages (const True) True ms
                  in go xs r
  where
    go [x]    Nothing  = Fail x
    go []     Nothing  = Fail ""
    go []     (Just r) = Done r
    go (x:xs) r        = Step x (go xs r)


runLogIO :: Log Message a -> IO (Maybe a)
runLogIO x =
  do
    let (ms, r) = runLog x
    putStr (unlines $ showMessages (const True) True ms)
    return r

failWith :: m -> Log m a
failWith m = Step m (Fail ())

succeedWith :: m -> a -> Log m a
succeedWith m x = Step m (Done x)

continueWith :: m -> Log m a -> Log m a
continueWith = Step

tryWith :: Message -> Log Message a -> Log Message a
tryWith m x = Step m (Step Enter x) <|> failWith Leave
