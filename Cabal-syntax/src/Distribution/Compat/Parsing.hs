{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Compat.Parsing
-- Copyright   :  (c) Edward Kmett 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Alternative parser combinators.
--
-- Originally in @parsers@ package.
module Distribution.Compat.Parsing
  ( -- * Parsing Combinators
    choice
  , option
  , optional -- from Control.Applicative, parsec optionMaybe
  , skipOptional -- parsec optional
  , between
  , some -- from Control.Applicative, parsec many1
  , many -- from Control.Applicative
  , sepBy
  , sepByNonEmpty
  , sepEndByNonEmpty
  , sepEndBy
  , endByNonEmpty
  , endBy
  , count
  , chainl
  , chainr
  , chainl1
  , chainr1
  , manyTill

    -- * Parsing Class
  , Parsing (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Applicative (optional, (<**>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Foldable (asum)

import qualified Data.List.NonEmpty as NE
import qualified Text.Parsec as Parsec

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- parser.
choice :: Alternative m => [m a] -> m a
choice = asum
{-# INLINE choice #-}

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- >  priority = option 0 (digitToInt <$> digit)
option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x
{-# INLINE option #-}

-- | @skipOptional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
-- It only fails if @p@ fails after consuming input. It discards the result
-- of @p@. (Plays the role of parsec's optional, which conflicts with Applicative's optional)
skipOptional :: Alternative m => m a -> m ()
skipOptional p = (() <$ p) <|> pure ()
{-# INLINE skipOptional #-}

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- >  braces  = between (symbol "{") (symbol "}")
between :: Applicative m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket
{-# INLINE between #-}

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- >  commaSep p  = p `sepBy` (symbol ",")
sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = toList <$> sepByNonEmpty p sep <|> pure []
{-# INLINE sepBy #-}

-- | @sepByNonEmpty p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a non-empty list of values returned by @p@.
sepByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = (:|) <$> p <*> many (sep *> p)
{-# INLINE sepByNonEmpty #-}

-- | @sepEndByNonEmpty p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a non-empty list of values
-- returned by @p@.
sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, ie. haskell style
-- statements. Returns a list of values returned by @p@.
--
-- >  haskellStatements  = haskellStatement `sepEndBy` semi
sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy p sep = toList <$> sepEndByNonEmpty p sep <|> pure []
{-# INLINE sepEndBy #-}

-- | @endByNonEmpty p sep@ parses /one/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a non-empty list of values returned by @p@.
endByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
endByNonEmpty p sep = NE.some1 (p <* sep)
{-# INLINE endByNonEmpty #-}

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- >   cStatements  = cStatement `endBy` semi
endBy :: Alternative m => m a -> m sep -> m [a]
endBy p sep = many (p <* sep)
{-# INLINE endBy #-}

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of
-- @n@ values returned by @p@.
count :: Applicative m => Int -> m a -> m [a]
count n p
  | n <= 0 = pure []
  | otherwise = sequenceA (replicate n p)
{-# INLINE count #-}

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are no occurrences of @p@, the value @x@ is
-- returned.
chainr :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainr p op x = chainr1 p op <|> pure x
{-# INLINE chainr #-}

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are zero occurrences of @p@, the value @x@ is
-- returned.
chainl :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainl p op x = chainl1 p op <|> pure x
{-# INLINE chainl #-}

-- | @chainl1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. . This parser can for example be used to eliminate left
-- recursion which typically occurs in expression grammars.
--
-- >  expr   = term   `chainl1` addop
-- >  term   = factor `chainl1` mulop
-- >  factor = parens expr <|> integer
-- >
-- >  mulop  = (*) <$ symbol "*"
-- >       <|> div <$ symbol "/"
-- >
-- >  addop  = (+) <$ symbol "+"
-- >       <|> (-) <$ symbol "-"
chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
{-# INLINE chainl1 #-}

-- | @chainr1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@.
chainr1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan
  where
    scan = p <**> rst
    rst = (flip <$> op <*> scan) <|> pure id
{-# INLINE chainr1 #-}

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@.
-- This parser can be used to scan comments:
--
-- >  simpleComment   = do{ string "<!--"
-- >                      ; manyTill anyChar (try (string "-->"))
-- >                      }
--
--    Note the overlapping parsers @anyChar@ and @string \"-->\"@, and
--    therefore the use of the 'try' combinator.
manyTill :: Alternative m => m a -> m end -> m [a]
manyTill p end = go where go = ([] <$ end) <|> ((:) <$> p <*> go)
{-# INLINE manyTill #-}

infixr 0 <?>

-- | Additional functionality needed to describe parsers independent of input type.
class Alternative m => Parsing m where
  -- | Take a parser that may consume input, and on failure, go back to
  -- where we started and fail as if we didn't consume input.
  try :: m a -> m a

  -- | Give a parser a name
  (<?>) :: m a -> String -> m a

  -- | A version of many that discards its input. Specialized because it
  -- can often be implemented more cheaply.
  skipMany :: m a -> m ()
  skipMany p = () <$ many p
  {-# INLINE skipMany #-}

  -- | @skipSome p@ applies the parser @p@ /one/ or more times, skipping
  -- its result. (aka skipMany1 in parsec)
  skipSome :: m a -> m ()
  skipSome p = p *> skipMany p
  {-# INLINE skipSome #-}

  -- | Used to emit an error on an unexpected token
  unexpected :: String -> m a

  -- | This parser only succeeds at the end of the input. This is not a
  -- primitive parser but it is defined using 'notFollowedBy'.
  --
  -- >  eof  = notFollowedBy anyChar <?> "end of input"
  eof :: m ()

  -- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
  -- does not consume any input. This parser can be used to implement the
  -- \'longest match\' rule. For example, when recognizing keywords (for
  -- example @let@), we want to make sure that a keyword is not followed
  -- by a legal identifier character, in which case the keyword is
  -- actually an identifier (for example @lets@). We can program this
  -- behaviour as follows:
  --
  -- >  keywordLet  = try $ string "let" <* notFollowedBy alphaNum
  notFollowedBy :: Show a => m a -> m ()

instance (Parsing m, MonadPlus m) => Parsing (Lazy.StateT s m) where
  try (Lazy.StateT m) = Lazy.StateT $ try . m
  {-# INLINE try #-}
  Lazy.StateT m <?> l = Lazy.StateT $ \s -> m s <?> l
  {-# INLINE (<?>) #-}
  unexpected = lift . unexpected
  {-# INLINE unexpected #-}
  eof = lift eof
  {-# INLINE eof #-}
  notFollowedBy (Lazy.StateT m) = Lazy.StateT $
    \s -> notFollowedBy (fst <$> m s) >> return ((), s)
  {-# INLINE notFollowedBy #-}

instance (Parsing m, MonadPlus m) => Parsing (Strict.StateT s m) where
  try (Strict.StateT m) = Strict.StateT $ try . m
  {-# INLINE try #-}
  Strict.StateT m <?> l = Strict.StateT $ \s -> m s <?> l
  {-# INLINE (<?>) #-}
  unexpected = lift . unexpected
  {-# INLINE unexpected #-}
  eof = lift eof
  {-# INLINE eof #-}
  notFollowedBy (Strict.StateT m) = Strict.StateT $
    \s -> notFollowedBy (fst <$> m s) >> return ((), s)
  {-# INLINE notFollowedBy #-}

instance (Parsing m, MonadPlus m) => Parsing (ReaderT e m) where
  try (ReaderT m) = ReaderT $ try . m
  {-# INLINE try #-}
  ReaderT m <?> l = ReaderT $ \e -> m e <?> l
  {-# INLINE (<?>) #-}
  skipMany (ReaderT m) = ReaderT $ skipMany . m
  {-# INLINE skipMany #-}
  unexpected = lift . unexpected
  {-# INLINE unexpected #-}
  eof = lift eof
  {-# INLINE eof #-}
  notFollowedBy (ReaderT m) = ReaderT $ notFollowedBy . m
  {-# INLINE notFollowedBy #-}

instance (Parsing m, MonadPlus m, Monoid w) => Parsing (Strict.WriterT w m) where
  try (Strict.WriterT m) = Strict.WriterT $ try m
  {-# INLINE try #-}
  Strict.WriterT m <?> l = Strict.WriterT (m <?> l)
  {-# INLINE (<?>) #-}
  unexpected = lift . unexpected
  {-# INLINE unexpected #-}
  eof = lift eof
  {-# INLINE eof #-}
  notFollowedBy (Strict.WriterT m) =
    Strict.WriterT $
      notFollowedBy (fst <$> m) >>= \x -> return (x, mempty)
  {-# INLINE notFollowedBy #-}

instance (Parsing m, MonadPlus m, Monoid w) => Parsing (Lazy.WriterT w m) where
  try (Lazy.WriterT m) = Lazy.WriterT $ try m
  {-# INLINE try #-}
  Lazy.WriterT m <?> l = Lazy.WriterT (m <?> l)
  {-# INLINE (<?>) #-}
  unexpected = lift . unexpected
  {-# INLINE unexpected #-}
  eof = lift eof
  {-# INLINE eof #-}
  notFollowedBy (Lazy.WriterT m) =
    Lazy.WriterT $
      notFollowedBy (fst <$> m) >>= \x -> return (x, mempty)
  {-# INLINE notFollowedBy #-}

instance (Parsing m, MonadPlus m, Monoid w) => Parsing (Lazy.RWST r w s m) where
  try (Lazy.RWST m) = Lazy.RWST $ \r s -> try (m r s)
  {-# INLINE try #-}
  Lazy.RWST m <?> l = Lazy.RWST $ \r s -> m r s <?> l
  {-# INLINE (<?>) #-}
  unexpected = lift . unexpected
  {-# INLINE unexpected #-}
  eof = lift eof
  {-# INLINE eof #-}
  notFollowedBy (Lazy.RWST m) = Lazy.RWST $
    \r s -> notFollowedBy ((\(a, _, _) -> a) <$> m r s) >>= \x -> return (x, s, mempty)
  {-# INLINE notFollowedBy #-}

instance (Parsing m, MonadPlus m, Monoid w) => Parsing (Strict.RWST r w s m) where
  try (Strict.RWST m) = Strict.RWST $ \r s -> try (m r s)
  {-# INLINE try #-}
  Strict.RWST m <?> l = Strict.RWST $ \r s -> m r s <?> l
  {-# INLINE (<?>) #-}
  unexpected = lift . unexpected
  {-# INLINE unexpected #-}
  eof = lift eof
  {-# INLINE eof #-}
  notFollowedBy (Strict.RWST m) = Strict.RWST $
    \r s -> notFollowedBy ((\(a, _, _) -> a) <$> m r s) >>= \x -> return (x, s, mempty)
  {-# INLINE notFollowedBy #-}

instance (Parsing m, Monad m) => Parsing (IdentityT m) where
  try = IdentityT . try . runIdentityT
  {-# INLINE try #-}
  IdentityT m <?> l = IdentityT (m <?> l)
  {-# INLINE (<?>) #-}
  skipMany = IdentityT . skipMany . runIdentityT
  {-# INLINE skipMany #-}
  unexpected = lift . unexpected
  {-# INLINE unexpected #-}
  eof = lift eof
  {-# INLINE eof #-}
  notFollowedBy (IdentityT m) = IdentityT $ notFollowedBy m
  {-# INLINE notFollowedBy #-}

instance (Parsec.Stream s m t, Show t) => Parsing (Parsec.ParsecT s u m) where
  try = Parsec.try
  (<?>) = (Parsec.<?>)
  skipMany = Parsec.skipMany
  skipSome = Parsec.skipMany1
  unexpected = Parsec.unexpected
  eof = Parsec.eof
  notFollowedBy = Parsec.notFollowedBy
