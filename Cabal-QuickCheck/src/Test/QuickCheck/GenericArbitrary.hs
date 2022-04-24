{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators #-}
module Test.QuickCheck.GenericArbitrary (
    genericArbitrary,
    GArbitrary,
) where

import GHC.Generics
import Test.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>), (<*>))
#endif

-- Generic arbitrary for non-recursive types
genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = fmap to garbitrary

class GArbitrary f where
    garbitrary :: Gen (f ())

class GArbitrarySum f where
    garbitrarySum :: [Gen (f ())]

class GArbitraryProd f where
    garbitraryProd :: Gen (f ())

instance (GArbitrarySum f, i ~ D) => GArbitrary (M1 i c f) where
    garbitrary = fmap M1 (oneof garbitrarySum)

instance (GArbitraryProd f, i ~ C) => GArbitrarySum (M1 i c f) where
    garbitrarySum = [fmap M1 garbitraryProd]

instance (GArbitrarySum f, GArbitrarySum g) => GArbitrarySum (f :+: g) where
    garbitrarySum = map (fmap L1) garbitrarySum ++ map (fmap R1) garbitrarySum

instance (GArbitraryProd f, i ~ S) => GArbitraryProd (M1 i c f) where
    garbitraryProd = fmap M1 garbitraryProd

instance GArbitraryProd U1 where
    garbitraryProd = pure U1

instance (GArbitraryProd f, GArbitraryProd g) => GArbitraryProd (f :*: g) where
    garbitraryProd = (:*:) <$> garbitraryProd <*> garbitraryProd

instance (Arbitrary a) => GArbitraryProd (K1 i a) where
    garbitraryProd = fmap K1 arbitrary
