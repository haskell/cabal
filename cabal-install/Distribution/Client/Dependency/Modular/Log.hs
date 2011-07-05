module Distribution.Client.Dependency.Modular.Log where

import Control.Applicative
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
                        (es, e) = proc 0 ms -- catch first error (always)
                        (ns, t) = case mbj of
                                    Nothing -> (ms, Nothing)
                                    Just n  -> proc n ms
                        -- prefer first error over later error
                        r       = case t of
                                    Nothing -> case s of
                                                 Nothing -> e
                                                 Just _  -> Nothing
                                    Just _  -> e
                      in go es -- trace for first error
                            (showMessages (const True) True ns) -- shortened run
                            r s
  where
    proc :: Int -> [Message] -> ([Message], Maybe (ConflictSet QPN))
    proc _ []                             = ([], Nothing)
    proc n (   Failure cs Backjump  : xs@(Leave : Failure cs' Backjump : _))
      | cs == cs'                         = proc n xs -- repeated backjumps count as one
    proc 0 (   Failure cs Backjump  : _ ) = ([], Just cs)
    proc n (x@(Failure _  Backjump) : xs) = (\ ~(ys, r) -> (x : ys, r)) (proc (n - 1) xs)
    proc n (x                       : xs) = (\ ~(ys, r) -> (x : ys, r)) (proc  n      xs)

    -- The order of arguments is important! In particular 's' must not be evaluated
    -- unless absolutely necessary. It contains the final result, and if we shortcut
    -- with an error due to backjumping, evaluating 's' would still require traversing
    -- the entire tree.
    go ms (x : xs) r         s        = Step x (go ms xs r s)
    go ms []       (Just cs) _        = Fail ("Could not resolve dependencies:\n" ++
                                        unlines (showMessages (foldr (\ v _ -> v `S.member` cs) True) False ms))
    go _  []       _         (Just s) = Done s
    go _  []       _         _        = Fail ("Could not resolve dependencies.") -- should not happen

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
