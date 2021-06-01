{-# LANGUAGE RoleAnnotations #-}
module Data.Map where
type role Map nominal representational
data Map k a = Map
instance Functor (Map k) where
    fmap _ Map = Map
