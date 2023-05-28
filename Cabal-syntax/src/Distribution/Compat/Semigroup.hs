{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Compatibility layer for "Data.Semigroup"
module Distribution.Compat.Semigroup
  ( Semigroup ((<>))
  , Mon.Monoid (..)
  , All (..)
  , Any (..)
  , First' (..)
  , Last' (..)
  , Option' (..)
  , gmappend
  , gmempty
  ) where

import Data.Typeable (Typeable)
import Distribution.Compat.Binary (Binary)
import Distribution.Utils.Structured (Structured)

import GHC.Generics

-- Data.Semigroup is available since GHC 8.0/base-4.9 in `base`
-- for older GHC/base, it's provided by `semigroups`

import qualified Data.Monoid as Mon
import Data.Semigroup

-- | A copy of 'Data.Semigroup.First'.
newtype First' a = First' {getFirst' :: a}
  deriving (Eq, Ord, Show)

instance Semigroup (First' a) where
  a <> _ = a

-- | A copy of 'Data.Semigroup.Last'.
newtype Last' a = Last' {getLast' :: a}
  deriving (Eq, Ord, Read, Show, Generic, Binary, Typeable)

instance Structured a => Structured (Last' a)

instance Semigroup (Last' a) where
  _ <> b = b

instance Functor Last' where
  fmap f (Last' x) = Last' (f x)

-- | A wrapper around 'Maybe', providing the 'Semigroup' and 'Monoid' instances
-- implemented for 'Maybe' since @base-4.11@.
newtype Option' a = Option' {getOption' :: Maybe a}
  deriving (Eq, Ord, Read, Show, Binary, Generic, Functor, Typeable)

instance Structured a => Structured (Option' a)

instance Semigroup a => Semigroup (Option' a) where
  Option' (Just a) <> Option' (Just b) = Option' (Just (a <> b))
  Option' Nothing <> b = b
  a <> Option' Nothing = a

instance Semigroup a => Monoid (Option' a) where
  mempty = Option' Nothing
  mappend = (<>)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Stolen from Edward Kmett's BSD3-licensed `semigroups` package

-- | Generically generate a 'Semigroup' ('<>') operation for any type
-- implementing 'Generic'. This operation will append two values
-- by point-wise appending their component fields. It is only defined
-- for product types.
--
-- @
-- 'gmappend' a ('gmappend' b c) = 'gmappend' ('gmappend' a b) c
-- @
gmappend :: (Generic a, GSemigroup (Rep a)) => a -> a -> a
gmappend x y = to (gmappend' (from x) (from y))

class GSemigroup f where
  gmappend' :: f p -> f p -> f p

instance Semigroup a => GSemigroup (K1 i a) where
  gmappend' (K1 x) (K1 y) = K1 (x <> y)

instance GSemigroup f => GSemigroup (M1 i c f) where
  gmappend' (M1 x) (M1 y) = M1 (gmappend' x y)

instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
  gmappend' (x1 :*: x2) (y1 :*: y2) = gmappend' x1 y1 :*: gmappend' x2 y2

-- | Generically generate a 'Monoid' 'mempty' for any product-like type
-- implementing 'Generic'.
--
-- It is only defined for product types.
--
-- @
-- 'gmappend' 'gmempty' a = a = 'gmappend' a 'gmempty'
-- @
gmempty :: (Generic a, GMonoid (Rep a)) => a
gmempty = to gmempty'

class GSemigroup f => GMonoid f where
  gmempty' :: f p

instance (Semigroup a, Monoid a) => GMonoid (K1 i a) where
  gmempty' = K1 mempty

instance GMonoid f => GMonoid (M1 i c f) where
  gmempty' = M1 gmempty'

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
  gmempty' = gmempty' :*: gmempty'
