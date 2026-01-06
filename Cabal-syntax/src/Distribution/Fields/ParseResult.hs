{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | A parse result type for parsers from AST to Haskell types.
module Distribution.Fields.ParseResult
  ( ParseResult
  , runParseResult
  , PSource (..)
  , CabalFileSource (..)
  , recoverWith
  , parseWarning
  , parseWarnings
  , parseFailure
  , parseFatalFailure
  , parseFatalFailure'
  , annotateParseResult
  , mapParseResultAnnotation
  , getCabalSpecVersion
  , setCabalSpecVersion
  , withoutWarnings
  , liftParseResult
  , withSource
  ) where

import Distribution.Compat.Prelude
import Distribution.Parsec.Error (PError (..), PErrorWithSource (..))
import Distribution.Parsec.Position (Position (..), zeroPos)
import Distribution.Parsec.Source
import Distribution.Parsec.Warning
import Distribution.Version (Version)

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import qualified Data.Map as Map

-- | A monad with failure and accumulating errors and warnings.
newtype ParseResult src a = PR
  { unPR
      :: forall r
       . PRState src
      -> PRContext src
      -> (PRState src -> r) -- failure, but we were able to recover a new-style spec-version declaration
      -> (PRState src -> a -> r) -- success
      -> r
  }

data PRContext src = PRContext
  { prContextSource :: PSource src
  -- ^ The file we are parsing, if known. This field is parametric because we
  -- use the same parser for cabal files and project files.
  }

-- Note: we have version here, as we could get any version.
data PRState src = PRState ![PWarningWithSource src] ![PErrorWithSource src] !(Map Namespace [Trivium]) !(Maybe Version)

emptyPRState :: PRState src
emptyPRState = PRState [] [] Map.empty Nothing

-- | Forget 'ParseResult's warnings.
--
-- @since 3.4.0.0
withoutWarnings :: ParseResult src a -> ParseResult src a
withoutWarnings m = PR $ \s ctx failure success ->
  unPR m s ctx failure $ \ !s1 -> success (s1 `withWarningsOf` s)
  where
    withWarningsOf (PRState _ e t1 v) (PRState w _ _ _) = PRState w e t1 v

-- | Destruct a 'ParseResult' into the emitted warnings and either
-- a successful value or
-- list of errors and possibly recovered a spec-version declaration.
runParseResult :: ParseResult src a -> ([PWarningWithSource src], Either (Maybe Version, NonEmpty (PErrorWithSource src)) a)
runParseResult pr = unPR pr emptyPRState initialCtx failure success
  where
    initialCtx = PRContext PUnknownSource

    failure (PRState warns [] t v) =
        let !() = trace (show t) () in
        (warns, Left (v, PErrorWithSource PUnknownSource (PError zeroPos "panic") :| []))
    failure (PRState warns (err : errs) t v) =
        let !() = trace (show t) () in
        (warns, Left (v, err :| errs))

    success (PRState warns [] t _) x =
        let !() = trace (show t) () in
        (warns, Right x)
    -- If there are any errors, don't return the result
    success (PRState warns (err : errs) t v) _ =
        let !() = trace (show t) () in
        (warns, Left (v, err :| errs))

-- | Chain parsing operations that involve 'IO' actions.
liftParseResult :: (a -> IO (ParseResult src b)) -> ParseResult src a -> IO (ParseResult src b)
liftParseResult f pr = unPR pr emptyPRState initialCtx failure success
  where
    initialCtx = PRContext PUnknownSource

    failure s = return $ PR $ \s' _ctx failure' _ -> failure' (concatPRState s s')
    success s a = do
      pr' <- f a
      return $ PR $ \s' ctx failure' success' -> unPR pr' (concatPRState s s') ctx failure' success'
    concatPRState (PRState warnings errors t version) (PRState warnings' errors' _ version') =
      (PRState (warnings ++ warnings') (toList errors ++ errors') t (version <|> version'))

withSource :: src -> ParseResult src a -> ParseResult src a
withSource source (PR pr) = PR $ \s ctx failure success ->
  pr s (ctx{prContextSource = PKnownSource source}) failure success

instance Functor (ParseResult src) where
  fmap f (PR pr) = PR $ \ !s fp failure success ->
    pr s fp failure $ \ !s' a ->
      success s' (f a)
  {-# INLINE fmap #-}

instance Applicative (ParseResult src) where
  pure x = PR $ \ !s _ _ success -> success s x
  {-# INLINE pure #-}

  f <*> x = PR $ \ !s0 fp failure success ->
    unPR f s0 fp failure $ \ !s1 f' ->
      unPR x s1 fp failure $ \ !s2 x' ->
        success s2 (f' x')
  {-# INLINE (<*>) #-}

  x *> y = PR $ \ !s0 fp failure success ->
    unPR x s0 fp failure $ \ !s1 _ ->
      unPR y s1 fp failure success
  {-# INLINE (*>) #-}

  x <* y = PR $ \ !s0 fp failure success ->
    unPR x s0 fp failure $ \ !s1 x' ->
      unPR y s1 fp failure $ \ !s2 _ ->
        success s2 x'
  {-# INLINE (<*) #-}

instance Monad (ParseResult src) where
  return = pure
  (>>) = (*>)

  m >>= k = PR $ \ !s fp failure success ->
    unPR m s fp failure $ \ !s' a ->
      unPR (k a) s' fp failure success
  {-# INLINE (>>=) #-}

-- | "Recover" the parse result, so we can proceed parsing.
-- 'runParseResult' will still result in 'Nothing', if there are recorded errors.
recoverWith :: ParseResult src a -> a -> ParseResult src a
recoverWith (PR pr) x = PR $ \ !s fp _failure success ->
  pr s fp (\ !s' -> success s' x) success

mapParseResultAnnotation :: (Namespace -> Namespace) -> ParseResult src ()
mapParseResultAnnotation f = do
  PR $ \ !(PRState warns errs t0 v) _fp _failure success ->
    success (PRState warns errs (Map.mapKeys f t0) v) ()

annotateParseResult :: Map Namespace [Trivium] -> ParseResult src ()
annotateParseResult t = PR $ \ !(PRState warns errs t0 v) _fp _failure success ->
  success (PRState warns errs (t0 <> t) v) ()

-- | Set cabal spec version.
setCabalSpecVersion :: Maybe Version -> ParseResult src ()
setCabalSpecVersion v = PR $ \(PRState warns errs t _) _fp _failure success ->
  success (PRState warns errs t v) ()

-- | Get cabal spec version.
getCabalSpecVersion :: ParseResult src (Maybe Version)
getCabalSpecVersion = PR $ \s@(PRState _ _ _ v) _fp _failure success ->
  success s v

-- | Add a warning. This doesn't fail the parsing process.
parseWarning :: Position -> PWarnType -> String -> ParseResult src ()
parseWarning pos t msg = PR $ \(PRState warns errs trivia v) ctx _failure success ->
  success (PRState (PWarningWithSource (prContextSource ctx) (PWarning t pos msg) : warns) errs trivia v) ()

-- | Add multiple warnings at once.
parseWarnings :: [PWarning] -> ParseResult src ()
parseWarnings newWarns = PR $ \(PRState warns errs trivia v) ctx _failure success ->
  success (PRState (map (PWarningWithSource (prContextSource ctx)) newWarns ++ warns) errs trivia v) ()

-- | Add an error, but not fail the parser yet.
--
-- For fatal failure use 'parseFatalFailure'
parseFailure :: Position -> String -> ParseResult src ()
parseFailure pos msg = PR $ \(PRState warns errs trivia v) ctx _failure success ->
  success (PRState warns (PErrorWithSource (prContextSource ctx) (PError pos msg) : errs) trivia v) ()

-- | Add an fatal error.
parseFatalFailure :: Position -> String -> ParseResult src a
parseFatalFailure pos msg = PR $ \(PRState warns errs trivia v) ctx failure _success ->
  failure (PRState warns (PErrorWithSource (prContextSource ctx) (PError pos msg) : errs) trivia v)

-- | A 'mzero'.
parseFatalFailure' :: ParseResult src a
parseFatalFailure' = PR pr
  where
    pr (PRState warns [] trivia v) _ctx failure _success = failure (PRState warns [err] trivia v)
    pr s _ctx failure _success = failure s

    err = PErrorWithSource PUnknownSource (PError zeroPos "Unknown fatal error")
