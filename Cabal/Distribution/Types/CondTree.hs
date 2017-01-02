{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Distribution.Types.CondTree (
    CondTree(..),
    CondBranch(..),
    condIfThen,
    condIfThenElse,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Condition

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [CondBranch v c a]
    }
    deriving (Show, Eq, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance (Binary v, Binary c, Binary a) => Binary (CondTree v c a)

data CondBranch v c a = CondBranch
    { condBranchCondition :: Condition v
    , condBranchIfTrue    :: CondTree v c a
    , condBranchIfFalse   :: Maybe (CondTree v c a)
    }
    deriving (Show, Eq, Typeable, Data, Generic, Functor, Traversable)

-- This instance is written by hand because GHC 8.0.1/8.0.2 infinite
-- loops when trying to derive it with optimizations.  See
-- https://ghc.haskell.org/trac/ghc/ticket/13056
instance Foldable (CondBranch v c) where
    foldMap f (CondBranch _ c Nothing) = foldMap f c
    foldMap f (CondBranch _ c (Just a)) = foldMap f c `mappend` foldMap f a

instance (Binary v, Binary c, Binary a) => Binary (CondBranch v c a)

condIfThen :: Condition v -> CondTree v c a -> CondBranch v c a
condIfThen c t = CondBranch c t Nothing

condIfThenElse :: Condition v -> CondTree v c a -> CondTree v c a -> CondBranch v c a
condIfThenElse c t e = CondBranch c t (Just e)
