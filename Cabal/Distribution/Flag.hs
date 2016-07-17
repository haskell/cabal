{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Flag
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
--                Thomas Tuegel 2016
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A monoid for command-line options such that successive options override
-- their precedents.
--

module Distribution.Flag
    ( Flag(..), toFlag
    , fromFlag, fromFlagOrDefault, flagToMaybe, flagToList
    , allFlags
    ) where

import GHC.Generics             ( Generic )

import Distribution.Compat.Binary ( Binary )
import Distribution.Compat.Semigroup

-- ------------------------------------------------------------
-- * Flag type
-- ------------------------------------------------------------

-- | @Flag@ is a type for command line flags where subsequent
-- flags override their precedents, i.e. such that parsing
--
-- > --enable-foo --disable-foo
--
-- would yield @Flag False@.

data Flag a = Flag a | NoFlag deriving (Eq, Generic, Show, Read)

instance Binary a => Binary (Flag a)

instance Functor Flag where
  fmap f (Flag x) = Flag (f x)
  fmap _ NoFlag  = NoFlag

instance Monoid (Flag a) where
  mempty = NoFlag
  mappend = (<>)

instance Semigroup (Flag a) where
  _ <> f@(Flag _) = f
  f <> NoFlag     = f

instance Bounded a => Bounded (Flag a) where
  minBound = toFlag minBound
  maxBound = toFlag maxBound

instance Enum a => Enum (Flag a) where
  fromEnum = fromEnum . fromFlag
  toEnum   = toFlag   . toEnum
  enumFrom (Flag a) = map toFlag . enumFrom $ a
  enumFrom _        = []
  enumFromThen (Flag a) (Flag b) = toFlag `map` enumFromThen a b
  enumFromThen _        _        = []
  enumFromTo   (Flag a) (Flag b) = toFlag `map` enumFromTo a b
  enumFromTo   _        _        = []
  enumFromThenTo (Flag a) (Flag b) (Flag c) = toFlag `map` enumFromThenTo a b c
  enumFromThenTo _        _        _        = []

toFlag :: a -> Flag a
toFlag = Flag

{-# DEPRECATED fromFlag "Use fromFlagOrDefault instead." #-}
fromFlag :: Flag a -> a
fromFlag (Flag x) = x
fromFlag NoFlag   = error "fromFlag: NoFlag. Use fromFlagOrDefault instead."

fromFlagOrDefault :: a -> Flag a -> a
fromFlagOrDefault _   (Flag x) = x
fromFlagOrDefault def NoFlag   = def

flagToMaybe :: Flag a -> Maybe a
flagToMaybe (Flag x) = Just x
flagToMaybe NoFlag   = Nothing

flagToList :: Flag a -> [a]
flagToList (Flag x) = [x]
flagToList NoFlag   = []

allFlags :: [Flag Bool] -> Flag Bool
allFlags flags = if all (\f -> fromFlagOrDefault False f) flags
                 then Flag True
                 else NoFlag
