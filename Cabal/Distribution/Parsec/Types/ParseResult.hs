{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
-- | A parse result type for parsers from AST to Haskell types.
module Distribution.Parsec.Types.ParseResult (
    ParseResult,
    runParseResult,
    recoverWith,
    parseWarning,
    parseFailure,
    parseFatalFailure,
    parseFatalFailure',
    parseWarnings',
    ) where

import           Prelude ()
import           Distribution.Compat.Prelude
import           Distribution.Parsec.Types.Common
                 (PError (..), PWarnType (..), PWarning (..), Position (..))

-- | A monad with failure and accumulating errors and warnings.
newtype ParseResult a = PR
    { unPR
        :: forall r. PRState
        -> (PRState -> r)       -- failure
        -> (PRState -> a -> r)  -- success
        -> r
    }

data PRState = PRState ![PWarning] ![PError]

emptyPRState :: PRState
emptyPRState = PRState [] []

-- | Destruct a 'ParseResult' into the emitted warnings and errors, and
-- possibly the final result if there were no errors.
runParseResult :: ParseResult a -> ([PWarning], [PError], Maybe a)
runParseResult pr = unPR pr emptyPRState failure success
  where
    failure (PRState warns errs)   = (warns, errs, Nothing)
    success (PRState warns [])   x = (warns, [], Just x)
    -- If there are any errors, don't return the result
    success (PRState warns errs) _ = (warns, errs, Nothing)

instance Functor ParseResult where
    fmap f (PR pr) = PR $ \ !s failure success ->
        pr s failure $ \ !s' a ->
        success s' (f a)

instance Applicative ParseResult where
    pure x = PR $ \ !s _ success ->  success s x
    -- | Make it concat perrors
    (<*>) = ap
{-
    PR a *> PR b = PR $ \s0 -> case a s0 of
        (x, s1) -> case b s1 of
            (y, s2) -> (x *> y, s2)
-}

instance Monad ParseResult where
    return = pure
    (>>) = (*>)

    m >>= k = PR $ \ !s failure success ->
        unPR m s failure $ \ !s' a ->
        unPR (k a) s' failure success

-- | "Recover" the parse result, so we can proceed parsing.
-- 'runParseResult' will still result in 'Nothing', if there are recorded errors.
recoverWith :: ParseResult a -> a -> ParseResult a
recoverWith (PR pr) x = PR $ \ !s _failure success ->
    pr s (\ !s' -> success s' x) success

parseWarning :: Position -> PWarnType -> String -> ParseResult ()
parseWarning pos t msg = PR $ \(PRState warns errs) _failure success ->
    success (PRState (PWarning t pos msg : warns) errs) ()

parseWarnings' :: [PWarning] -> ParseResult ()
parseWarnings' newWarns = PR $ \(PRState warns errs) _failure success ->
    success (PRState (newWarns ++ warns) errs) ()

-- | Add an error, but not fail the parser yet.
--
-- For fatal failure use 'parseFatalFailure'
parseFailure :: Position -> String -> ParseResult ()
parseFailure pos msg = PR $ \(PRState warns errs) _failure success ->
    success (PRState warns (PError pos msg : errs)) ()

-- | Add an fatal error.
parseFatalFailure :: Position -> String -> ParseResult a
parseFatalFailure pos msg = PR $ \(PRState warns errs) failure _success ->
    failure (PRState warns (PError pos msg : errs))

parseFatalFailure' :: ParseResult a
parseFatalFailure' = PR pr
  where
    pr (PRState warns []) failure _success = failure (PRState warns [err])
    pr s                  failure _success = failure s

    err = PError (Position 0 0) "Unknown fatal error"
