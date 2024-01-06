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
  , mergeListFlag
  , BooleanFlag (..)
  ) where

import Distribution.Compat.Prelude hiding (get)
import Distribution.Compat.Stack
import Prelude ()

-- ------------------------------------------------------------

-- * Flag type

-- ------------------------------------------------------------

-- | All flags are monoids, they come in two flavours:
--
-- 1. list flags eg
--
--   > --ghc-option=foo --ghc-option=bar
--
--   gives us all the values ["foo", "bar"]
--
-- 2. singular value flags, eg:
--
--   > --enable-foo --disable-foo
--
--   gives us Just False
--
-- So, this 'Flag' type is for the latter singular kind of flag.
-- Its monoid instance gives us the behaviour where it starts out as
-- 'NoFlag' and later flags override earlier ones.
--
-- Isomorphic to 'Maybe' a.
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

-- | Wraps a value in 'Flag'.
toFlag :: a -> Flag a
toFlag = Flag

-- | Extracts a value from a 'Flag', and throws an exception on 'NoFlag'.
fromFlag :: WithCallStack (Flag a -> a)
fromFlag (Flag x) = x
fromFlag NoFlag = error "fromFlag NoFlag. Use fromFlagOrDefault"

-- | Extracts a value from a 'Flag', and returns the default value on 'NoFlag'.
fromFlagOrDefault :: a -> Flag a -> a
fromFlagOrDefault _ (Flag x) = x
fromFlagOrDefault def NoFlag = def

-- | Converts a 'Flag' value to a 'Maybe' value.
flagToMaybe :: Flag a -> Maybe a
flagToMaybe (Flag x) = Just x
flagToMaybe NoFlag = Nothing

-- | Pushes a function through a 'Flag' value, and returns a default
-- if the 'Flag' value is 'NoFlag'.
--
-- @since 3.4.0.0
flagElim :: b -> (a -> b) -> Flag a -> b
flagElim n _ NoFlag = n
flagElim _ f (Flag x) = f x

-- | Converts a 'Flag' value to a list.
flagToList :: Flag a -> [a]
flagToList (Flag x) = [x]
flagToList NoFlag = []

-- | Returns 'True' only if every 'Flag' 'Bool' value is Flag True, else 'False'.
allFlags :: [Flag Bool] -> Flag Bool
allFlags flags =
  if all (\f -> fromFlagOrDefault False f) flags
    then Flag True
    else NoFlag

-- | Converts a 'Maybe' value to a 'Flag' value.
maybeToFlag :: Maybe a -> Flag a
maybeToFlag Nothing = NoFlag
maybeToFlag (Just x) = Flag x

-- | Merge the elements of a list 'Flag' with another list 'Flag'.
mergeListFlag :: Flag [a] -> Flag [a] -> Flag [a]
mergeListFlag currentFlags v =
  Flag $ concat (flagToList currentFlags ++ flagToList v)

-- | Types that represent boolean flags.
class BooleanFlag a where
  asBool :: a -> Bool

instance BooleanFlag Bool where
  asBool = id
