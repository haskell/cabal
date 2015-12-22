{-# LANGUAGE CPP #-}

-- | Compatibility layer for "Data.Semigroup"
module Distribution.Compat.Semigroup
    ( Semigroup((<>))
    , Mon.Monoid(..)
    , All(..)
    , Any(..)
    ) where

#if __GLASGOW_HASKELL__ >= 711
-- Data.Semigroup is available since GHC 8.0/base-4.9
import Data.Semigroup
import qualified Data.Monoid as Mon
#else
-- provide internal simplified non-exposed class for older GHCs
import Data.Monoid as Mon (Monoid(..), All(..), Any(..), Dual(..))

class Semigroup a where
    (<>) :: a -> a -> a

-- several primitive instances
instance Semigroup () where
    _ <> _ = ()

instance Semigroup [a] where
    (<>) = (++)

instance Semigroup a => Semigroup (Dual a) where
    Dual a <> Dual b = Dual (b <> a)

instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

instance Semigroup (Either a b) where
    Left _ <> b = b
    a      <> _ = a

instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> y = y
    GT <> _ = GT

instance Semigroup b => Semigroup (a -> b) where
    f <> g = \a -> f a <> g a

instance Semigroup All where
    All a <> All b = All (a && b)

instance Semigroup Any where
    Any a <> Any b = Any (a || b)

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    (a,b) <> (a',b') = (a<>a',b<>b')

instance (Semigroup a, Semigroup b, Semigroup c)
         => Semigroup (a, b, c) where
    (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (a, b, c, d) where
    (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e)
         => Semigroup (a, b, c, d, e) where
    (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')

#endif
