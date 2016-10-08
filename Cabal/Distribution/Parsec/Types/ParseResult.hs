{-# LANGUAGE CPP #-}
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

import           Distribution.Compat.Prelude
import           Prelude ()

import           Distribution.Parsec.Types.Common
                 (PError (..), PWarnType (..), PWarning (..), Position (..))

-- | A monad with failure and accumulating errors
newtype ParseResult a = PR { runPR :: PRState -> (Maybe a, PRState) }

data PRState = PRState ![PWarning] ![PError]

emptyPRState :: PRState
emptyPRState = PRState [] []

runParseResult :: ParseResult a -> ([PWarning], [PError], Maybe a)
runParseResult pr = case runPR pr emptyPRState of
    (res, PRState warns errs)
        -- If there are any errors, don't return the result
        | null errs -> (warns, [], res)
        | otherwise -> (warns, errs, Nothing)

instance Functor ParseResult where
    fmap f (PR pr) = PR $ \s -> case pr s of
        (r, s') -> (fmap f r, s')

-- | Note: this is not a Monad!
instance Applicative ParseResult where
    pure x = PR $ \s -> (Just x, s)
    -- | Make it concat perrors
    (<*>) = ap
    PR a *> PR b = PR $ \s0 -> case a s0 of
        (x, s1) -> case b s1 of
            (y, s2) -> (x *> y, s2)

instance Monad ParseResult where
    return = pure
    (>>) = (*>)
    PR m >>= k = PR $ \s -> case m s of
        (Nothing, s') -> (Nothing, s')
        (Just x,  s') -> runPR (k x) s'

-- | "Recover" the parse result, so we can proceed parsing.
-- 'runParseResult' will still result 'Nothing', if there are recorded errors.
recoverWith :: ParseResult a -> a -> ParseResult a
recoverWith (PR f) x = PR $ \s -> case f s of
    (Nothing, s') -> (Just x, s')
    r             -> r

parseWarning :: Position -> PWarnType -> String -> ParseResult ()
parseWarning pos t msg = PR $ \(PRState warns errs) ->
    (Just (), PRState (PWarning t pos msg : warns) errs)

parseWarnings' :: [PWarning] -> ParseResult ()
parseWarnings' newWarns = PR $ \(PRState warns errs) ->
    (Just (), PRState (warns ++ newWarns) errs) 

-- | Add an error, but not fail the parser yet.
parseFailure :: Position -> String -> ParseResult ()
parseFailure pos msg = PR $ \(PRState warns errs) ->
    (Just (), PRState warns (PError pos msg : errs))

parseFatalFailure :: Position -> String -> ParseResult a
parseFatalFailure pos msg = PR $ \(PRState warns errs) ->
    (Nothing, PRState warns (PError pos msg : errs))

parseFatalFailure' :: ParseResult a
parseFatalFailure' = PR f
  where
    f s@(PRState warns errs)
        | null errs = (Nothing, PRState warns [PError (Position 0 0) "Unknown failure"])
        | otherwise = (Nothing, s)
