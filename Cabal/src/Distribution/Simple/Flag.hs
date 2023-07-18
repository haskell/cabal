{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Flag
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Defines the 'Flag' type and it's 'Monoid' instance,  see
-- <http://www.haskell.org/pipermail/cabal-devel/2007-December/001509.html>
-- for an explanation.
--
-- Split off from "Distribution.Simple.Setup" to break import cycles.
module Distribution.Simple.Flag
  ( Flag (..)
  , allFlags
  , toFlag
  , fromFlag
  , fromFlagOrDefault
  , flagElim
  , flagToMaybe
  , flagToList
  , maybeToFlag
  , BooleanFlag (..)
  ) where

import Distribution.Compat.Prelude hiding (get)
import Distribution.Compat.Stack
import Distribution.Parsec
import Prelude ()

-- ------------------------------------------------------------

-- * Flag type

-- ------------------------------------------------------------

-- | All flags are monoids, they come in two flavours:
--
-- 1. list flags eg
--
-- > --ghc-option=foo --ghc-option=bar
--
-- gives us all the values ["foo", "bar"]
--
-- 2. singular value flags, eg:
--
-- > --enable-foo --disable-foo
--
-- gives us Just False
-- So this Flag type is for the latter singular kind of flag.
-- Its monoid instance gives us the behaviour where it starts out as
-- 'NoFlag' and later flags override earlier ones.
data Flag a = Flag a | NoFlag deriving (Eq, Generic, Show, Read, Typeable, Foldable, Traversable)

instance Binary a => Binary (Flag a)
instance Structured a => Structured (Flag a)

instance Functor Flag where
  fmap f (Flag x) = Flag (f x)
  fmap _ NoFlag = NoFlag

instance Applicative Flag where
  (Flag x) <*> y = x <$> y
  NoFlag <*> _ = NoFlag
  pure = Flag

instance Monoid (Flag a) where
  mempty = NoFlag
  mappend = (<>)

instance Semigroup (Flag a) where
  _ <> f@(Flag _) = f
  f <> NoFlag = f

instance Bounded a => Bounded (Flag a) where
  minBound = toFlag minBound
  maxBound = toFlag maxBound

instance Enum a => Enum (Flag a) where
  fromEnum = fromEnum . fromFlag
  toEnum = toFlag . toEnum
  enumFrom (Flag a) = map toFlag . enumFrom $ a
  enumFrom _ = []
  enumFromThen (Flag a) (Flag b) = toFlag `map` enumFromThen a b
  enumFromThen _ _ = []
  enumFromTo (Flag a) (Flag b) = toFlag `map` enumFromTo a b
  enumFromTo _ _ = []
  enumFromThenTo (Flag a) (Flag b) (Flag c) = toFlag `map` enumFromThenTo a b c
  enumFromThenTo _ _ _ = []

instance Parsec a => Parsec (Flag a) where
  parsec = parsecFlag

parsecFlag :: (Parsec a, CabalParsing m) => m (Flag a)
parsecFlag = (Flag <$> parsec) <|> pure mempty

toFlag :: a -> Flag a
toFlag = Flag

fromFlag :: WithCallStack (Flag a -> a)
fromFlag (Flag x) = x
fromFlag NoFlag = error "fromFlag NoFlag. Use fromFlagOrDefault"

fromFlagOrDefault :: a -> Flag a -> a
fromFlagOrDefault _ (Flag x) = x
fromFlagOrDefault def NoFlag = def

flagToMaybe :: Flag a -> Maybe a
flagToMaybe (Flag x) = Just x
flagToMaybe NoFlag = Nothing

-- | @since 3.4.0.0
flagElim :: b -> (a -> b) -> Flag a -> b
flagElim n _ NoFlag = n
flagElim _ f (Flag x) = f x

flagToList :: Flag a -> [a]
flagToList (Flag x) = [x]
flagToList NoFlag = []

allFlags :: [Flag Bool] -> Flag Bool
allFlags flags =
  if all (\f -> fromFlagOrDefault False f) flags
    then Flag True
    else NoFlag

maybeToFlag :: Maybe a -> Flag a
maybeToFlag Nothing = NoFlag
maybeToFlag (Just x) = Flag x

-- | Types that represent boolean flags.
class BooleanFlag a where
  asBool :: a -> Bool

instance BooleanFlag Bool where
  asBool = id
