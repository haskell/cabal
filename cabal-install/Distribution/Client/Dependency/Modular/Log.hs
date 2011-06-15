module Distribution.Client.Dependency.Modular.Log where

import Control.Applicative

import Distribution.Client.Dependency.Types -- from Cabal

import Distribution.Client.Dependency.Modular.Message

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

logToProgress :: Log Message a -> Progress String String a
logToProgress l = let
                    (ms, r) = runLog l
                    xs      = showMessages ms
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
    putStr (unlines $ showMessages ms)
    return r

failWith :: m -> Log m a
failWith m = Step m (Fail ())

succeedWith :: m -> a -> Log m a
succeedWith m x = Step m (Done x)

continueWith :: m -> Log m a -> Log m a
continueWith = Step

tryWith :: Message -> Log Message a -> Log Message a
tryWith m x = Step m (Step Enter x) <|> failWith Leave
