{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Distribution.Types.CondTree (
    CondTree(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Condition

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [( Condition v
                              , CondTree v c a
                              , Maybe (CondTree v c a))]
    }
    deriving (Show, Eq, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance (Binary v, Binary c, Binary a) => Binary (CondTree v c a)
