{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

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
  ( Flag
  , pattern Flag
  , pattern NoFlag
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

import Data.Monoid (Last (..))
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
type Flag = Last

pattern Flag :: a -> Last a
pattern Flag a = Last (Just a)

pattern NoFlag :: Last a
pattern NoFlag = Last Nothing

{-# COMPLETE Flag, NoFlag #-}

-- | Wraps a value in 'Flag'.
toFlag :: a -> Flag a
toFlag = Flag

-- | Extracts a value from a 'Flag', and throws an exception on 'NoFlag'.
fromFlag :: WithCallStack (Flag a -> a)
fromFlag (Flag x) = x
fromFlag NoFlag = error "fromFlag NoFlag. Use fromFlagOrDefault"

-- | Extracts a value from a 'Flag', and returns the default value on 'NoFlag'.
fromFlagOrDefault :: a -> Flag a -> a
fromFlagOrDefault def = fromMaybe def . getLast

-- | Converts a 'Flag' value to a 'Maybe' value.
flagToMaybe :: Flag a -> Maybe a
flagToMaybe = getLast

-- | Pushes a function through a 'Flag' value, and returns a default
-- if the 'Flag' value is 'NoFlag'.
--
-- @since 3.4.0.0
flagElim :: b -> (a -> b) -> Flag a -> b
flagElim n f = maybe n f . getLast

-- | Converts a 'Flag' value to a list.
flagToList :: Flag a -> [a]
flagToList = maybeToList . getLast

-- | Returns 'True' only if every 'Flag' 'Bool' value is Flag True, else 'False'.
allFlags :: [Flag Bool] -> Flag Bool
allFlags flags =
  if all (\f -> fromFlagOrDefault False f) flags
    then Flag True
    else NoFlag

-- | Converts a 'Maybe' value to a 'Flag' value.
maybeToFlag :: Maybe a -> Flag a
maybeToFlag = Last

-- | Merge the elements of a list 'Flag' with another list 'Flag'.
mergeListFlag :: Flag [a] -> Flag [a] -> Flag [a]
mergeListFlag currentFlags v =
  Flag $ concat (flagToList currentFlags ++ flagToList v)

-- | Types that represent boolean flags.
class BooleanFlag a where
  asBool :: a -> Bool

instance BooleanFlag Bool where
  asBool = id
