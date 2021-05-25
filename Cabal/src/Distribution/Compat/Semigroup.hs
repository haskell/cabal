{-# LANGUAGE CPP                         #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE InstanceSigs                #-}
{-# LANGUAGE StandaloneKindSignatures    #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}

-- | Compatibility layer for "Data.Semigroup"
module Distribution.Compat.Semigroup
    ( Semigroup((<>))
    , Mon.Monoid(..)
    , All(..)
    , Any(..)

    , First'(..)
    , Last'(..)

    , Option'(..)

    , GenericMonoid(..)
    , gmappend
    , gmempty
    ) where

import Distribution.Compat.Binary (Binary)
import Distribution.Utils.Structured (Structured)
import Data.Typeable (Typeable)

import Data.Kind (Type, Constraint)
import GHC.Generics
-- Data.Semigroup is available since GHC 8.0/base-4.9 in `base`
-- for older GHC/base, it's provided by `semigroups`
import Data.Semigroup
import qualified Data.Monoid as Mon


-- | A copy of 'Data.Semigroup.First'.
newtype First' a = First' { getFirst' :: a }
  deriving (Eq, Ord, Show)

instance Semigroup (First' a) where
  a <> _ = a

-- | A copy of 'Data.Semigroup.Last'.
newtype Last' a = Last' { getLast' :: a }
  deriving (Eq, Ord, Read, Show, Generic, Binary, Typeable)

instance Structured a => Structured (Last' a)

instance Semigroup (Last' a) where
  _ <> b = b

instance Functor Last' where
  fmap f (Last' x) = Last' (f x)

-- | A wrapper around 'Maybe', providing the 'Semigroup' and 'Monoid' instances
-- implemented for 'Maybe' since @base-4.11@.
newtype Option' a = Option' { getOption' :: Maybe a }
  deriving (Eq, Ord, Read, Show, Binary, Generic, Functor, Typeable)

instance Structured a => Structured (Option' a)

instance Semigroup a => Semigroup (Option' a) where
  Option' (Just a) <> Option' (Just b) = Option' (Just (a <> b))
  Option' Nothing  <> b                = b
  a                <> Option' Nothing  = a

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

type  GSemigroup :: (Type -> Type) -> Constraint
class GSemigroup f where
  gmappend' :: f a -> f a -> f a

instance Semigroup a => GSemigroup (K1 i a) where
  gmappend' :: K1 info a b -> K1 info a b -> K1 info a b
  gmappend' = (<>)

instance GSemigroup f => GSemigroup (M1 i c f) where
  gmappend' :: M1 info meta f a -> M1 info meta f a -> M1 info meta f a
  gmappend' (M1 x) (M1 y) = M1 (gmappend' x y)

instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
  gmappend' :: (f :*: g) a -> (f :*: g) a -> (f :*: g) a
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

type  GMonoid :: (Type -> Type) -> Constraint
class GSemigroup f => GMonoid f where
  gmempty' :: f a

instance (Semigroup a, Monoid a) => GMonoid (K1 info a) where
  gmempty' :: K1 info a b
  gmempty' = mempty

instance GMonoid f => GMonoid (M1 info meta f) where
  gmempty' :: M1 info meta f a
  gmempty' = M1 gmempty'

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
  gmempty' :: (f :*: g) a
  gmempty' = gmempty' :*: gmempty'

-- | Generic instance for 'Semigroup' and 'Monoid' defined pointwise
-- for any product-like type implementing 'Generic'.
--
-- Can be used with @-XDerivingVia@
--
-- @
-- {-# Language DerivingVia #-}
--
-- data T = MkT [Int] (Maybe Ordering)
--   deriving
--   stock Generic
--
--   deriving (Semigroup, Monoid)
--   via GenericMonoid T
-- @
type    GenericMonoid :: Type -> Type
newtype GenericMonoid a = GenericMonoid a

instance (Generic a, GSemigroup (Rep a)) => Semigroup (GenericMonoid a) where
  (<>) :: GenericMonoid a -> GenericMonoid a -> GenericMonoid a
  GenericMonoid a <> GenericMonoid b = GenericMonoid (gmappend a b)

instance (Generic a, GMonoid (Rep a)) => Monoid (GenericMonoid a) where
  mempty :: GenericMonoid a
  mempty = GenericMonoid gmempty
