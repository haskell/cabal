{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.ParsecParser where

import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude

import Distribution.Parsec.FieldLineStream (FieldLineStream, fieldLineStreamFromString)
import Distribution.Parsec.Position (Position (..))
import Distribution.Parsec.Warning
import Prelude ()

import Distribution.CabalParsing
import Distribution.PPUserState
import Distribution.Types.AnnotationNamespace

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.MonadFail as Fail
import qualified Text.Parsec as Parsec

import qualified Data.Map as Map

newtype ParsecParser a = PP
  { unPP
      :: CabalSpecVersion
      -> Parsec.Parsec FieldLineStream PPUserState a
  }

liftParsec :: Parsec.Parsec FieldLineStream PPUserState a -> ParsecParser a
liftParsec p = PP $ \_ -> p

instance CabalParsing ParsecParser where
  parsecWarning t w = liftParsec $ do
    spos <- Parsec.getPosition
    Parsec.modifyState $ \(PPUserState warns trivia) ->
      PPUserState
        (PWarning t (Position (Parsec.sourceLine spos) (Parsec.sourceColumn spos)) w : warns)
        trivia
  askCabalSpecVersion = PP pure

  mapAnnotationKeys f = liftParsec $ do
    Parsec.modifyState $ \(PPUserState warns trivia) ->
      PPUserState warns (Map.mapKeys f trivia)

  annotate namespace trivium = liftParsec $ do
    Parsec.modifyState $ \(PPUserState warns trivia) ->
      -- TODO(leana8959): proof of concept
      PPUserState warns (Map.insertWith (<>) namespace [trivium] trivia)

instance Functor ParsecParser where
  fmap f p = PP $ \v -> fmap f (unPP p v)
  {-# INLINE fmap #-}

  x <$ p = PP $ \v -> x <$ unPP p v
  {-# INLINE (<$) #-}

instance Applicative ParsecParser where
  pure = liftParsec . pure
  {-# INLINE pure #-}

  f <*> x = PP $ \v -> unPP f v <*> unPP x v
  {-# INLINE (<*>) #-}
  f *> x = PP $ \v -> unPP f v *> unPP x v
  {-# INLINE (*>) #-}
  f <* x = PP $ \v -> unPP f v <* unPP x v
  {-# INLINE (<*) #-}

instance Alternative ParsecParser where
  empty = liftParsec empty

  a <|> b = PP $ \v -> unPP a v <|> unPP b v
  {-# INLINE (<|>) #-}

  many p = PP $ \v -> many (unPP p v)
  {-# INLINE many #-}

  some p = PP $ \v -> some (unPP p v)
  {-# INLINE some #-}


instance Monad ParsecParser where
  return = pure

  m >>= k = PP $ \v -> unPP m v >>= \x -> unPP (k x) v
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadPlus ParsecParser where
  mzero = empty
  mplus = (<|>)

instance Fail.MonadFail ParsecParser where
  fail = P.unexpected

instance P.Parsing ParsecParser where
  try p = PP $ \v -> P.try (unPP p v)
  p <?> d = PP $ \v -> unPP p v P.<?> d
  skipMany p = PP $ \v -> P.skipMany (unPP p v)
  skipSome p = PP $ \v -> P.skipSome (unPP p v)
  unexpected = liftParsec . P.unexpected
  eof = liftParsec P.eof
  notFollowedBy p = PP $ \v -> P.notFollowedBy (unPP p v)

instance P.CharParsing ParsecParser where
  satisfy = liftParsec . P.satisfy
  char = liftParsec . P.char
  notChar = liftParsec . P.notChar
  anyChar = liftParsec P.anyChar
  string = liftParsec . P.string

-- | Like 'runParsecParser' but lets specify 'CabalSpecVersion' used.
--
-- @since 3.0.0.0
runParsecParser' :: CabalSpecVersion -> ParsecParser a -> FilePath -> FieldLineStream -> Either Parsec.ParseError a
runParsecParser' v p n = Parsec.runParser (unPP p v <* P.eof) emptyPPUserState n

-- | Run 'ParsecParser' with 'cabalSpecLatest'.
runParsecParser :: ParsecParser a -> FilePath -> FieldLineStream -> Either Parsec.ParseError a
runParsecParser = runParsecParser' cabalSpecLatest

-- | Parse a 'String' with given 'ParsecParser' and 'CabalSpecVersion'. Trailing whitespace is accepted.
-- See 'explicitEitherParsec'.
--
-- @since 3.4.0.0
explicitEitherParsec' :: CabalSpecVersion -> ParsecParser a -> String -> Either String a
explicitEitherParsec' spec parser =
  either (Left . show) Right
    . runParsecParser' spec (parser <* P.spaces) "<eitherParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with given 'ParsecParser'. Trailing whitespace is accepted.
explicitEitherParsec :: ParsecParser a -> String -> Either String a
explicitEitherParsec parser =
  either (Left . show) Right
    . runParsecParser (parser <* P.spaces) "<eitherParsec>"
    . fieldLineStreamFromString
