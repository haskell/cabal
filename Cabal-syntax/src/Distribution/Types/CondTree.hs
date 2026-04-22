{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Distribution.Types.CondTree
  ( CondTree (..)
  , CondBranch (..)
  , condIfThen
  , condIfThenElse
  , foldCondTree
  , mapCondTree
  , mapTreeConds
  , mapTreeData
  , traverseCondTreeA
  , traverseCondTreeV
  , traverseCondBranchA
  , traverseCondBranchV
  , extractCondition
  , simplifyCondTree
  , simplifyCondBranch
  , ignoreConditions
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Condition

import Control.Exception
import Data.Kind

import qualified Distribution.Compat.Lens as L

-- | A 'CondTree' is used to represent the conditional structure of
-- a Cabal file, reflecting a syntax element subject to constraints,
-- and then any number of sub-elements which may be enabled subject
-- to some condition.  Both @a@ and @c@ are usually 'Monoid's.
--
-- To be more concrete, consider the following fragment of a @Cabal@
-- file:
--
-- @
-- build-depends: base >= 4.0
-- if flag(extra)
--     build-depends: base >= 4.2
-- @
--
-- One way to represent this is to have @'CondTree' 'ConfVar'
-- ['Dependency'] 'BuildInfo'@.  Here, 'condTreeData' represents
-- the actual fields which are not behind any conditional, while
-- 'condTreeComponents' recursively records any further fields
-- which are behind a conditional.  'condTreeConstraints' records
-- the constraints (in this case, @base >= 4.0@) which would
-- be applied if you use this syntax; in general, this is
-- derived off of 'targetBuildInfo' (perhaps a good refactoring
-- would be to convert this into an opaque type, with a smart
-- constructor that pre-computes the dependencies.)
data CondTree v a = CondNode
  { condTreeData :: a
  , condTreeComponents :: [CondBranch v a]
  }

deriving instance (Show v, Show a) => Show (CondTree v a)
deriving instance (Eq v, Eq a) => Eq (CondTree v a)
deriving instance (Data v, Data a) => Data (CondTree v a)
deriving instance Generic (CondTree v a)

instance Functor (CondTree v) where
  fmap f (CondNode x bs) = CondNode (f x) ((fmap . fmap) f bs)

instance Foldable (CondTree v) where
  foldMap f (CondNode x bs) = f x <> (foldMap . foldMap) f bs

instance Traversable (CondTree v) where
  traverse f (CondNode x bs) = CondNode <$> f x <*> (traverse . traverse) f bs

instance (Binary v, Binary a) => Binary (CondTree v a)
instance (Structured v, Structured a) => Structured (CondTree v a)
instance (NFData v, NFData a) => NFData (CondTree v a) where rnf = genericRnf

instance Semigroup a => Semigroup (CondTree v a) where
  (CondNode a bs) <> (CondNode a' bs') = CondNode (a <> a') (bs <> bs')

instance (Semigroup a, Monoid a) => Monoid (CondTree v a) where
  mappend = (<>)
  mempty = CondNode mempty mempty

-- | A 'CondBranch' represents a conditional branch, e.g., @if
-- flag(foo)@ on some syntax @a@.  It also has an optional false
-- branch.
data CondBranch v a = CondBranch
  { condBranchCondition :: Condition v
  , condBranchIfTrue :: CondTree v a
  , condBranchIfFalse :: Maybe (CondTree v a)
  }
  deriving (Generic)

deriving instance (Show v, Show a) => Show (CondBranch v a)
deriving instance (Eq v, Eq a) => Eq (CondBranch v a)
deriving instance (Data v, Data a) => Data (CondBranch v a)
deriving instance Functor (CondBranch v)
deriving instance Foldable (CondBranch v)
deriving instance Traversable (CondBranch v)

instance (Binary v, Binary a) => Binary (CondBranch v a)
instance (Structured v, Structured a) => Structured (CondBranch v a)
instance (NFData v, NFData a) => NFData (CondBranch v a) where rnf = genericRnf

condIfThen :: Condition v -> CondTree v a -> CondBranch v a
condIfThen c t = CondBranch c t Nothing

condIfThenElse :: Condition v -> CondTree v a -> CondTree v a -> CondBranch v a
condIfThenElse c t e = CondBranch c t (Just e)

mapCondTree
  :: (a -> b)
  -> (Condition v -> Condition w)
  -> CondTree v a
  -> CondTree w b
mapCondTree fa fcnd (CondNode a ifs) =
  CondNode (fa a) (map g ifs)
  where
    g (CondBranch cnd t me) =
      CondBranch
        (fcnd cnd)
        (mapCondTree fa fcnd t)
        (fmap (mapCondTree fa fcnd) me)

mapTreeConds :: (Condition v -> Condition w) -> CondTree v a -> CondTree w a
mapTreeConds f = mapCondTree id f

mapTreeData :: (a -> b) -> CondTree v a -> CondTree v b
mapTreeData f = mapCondTree f id

-- | @@Traversal@@ for the data
traverseCondTreeA :: L.Traversal (CondTree v a) (CondTree v b) a b
traverseCondTreeA f (CondNode a ifs) =
  CondNode
    <$> f a
    <*> traverse (traverseCondBranchA f) ifs

-- | @@Traversal@@ for the variables
traverseCondTreeV :: L.Traversal (CondTree v a) (CondTree w a) v w
traverseCondTreeV f (CondNode a ifs) =
  CondNode a <$> traverse (traverseCondBranchV f) ifs

-- | @@Traversal@@ for the data
traverseCondBranchA :: L.Traversal (CondBranch v a) (CondBranch v b) a b
traverseCondBranchA f (CondBranch cnd t me) =
  CondBranch
    <$> pure cnd
    <*> traverseCondTreeA f t
    <*> traverse (traverseCondTreeA f) me

-- | @@Traversal@@ for the variables
traverseCondBranchV :: L.Traversal (CondBranch v a) (CondBranch w a) v w
traverseCondBranchV f (CondBranch cnd t me) =
  CondBranch
    <$> traverse f cnd
    <*> traverseCondTreeV f t
    <*> traverse (traverseCondTreeV f) me

-- | Extract the condition matched by the given predicate from a cond tree.
--
-- We use this mainly for extracting buildable conditions (see the Note in
-- Distribution.PackageDescription.Configuration), but the function is in fact
-- more general.
extractCondition :: Eq v => (a -> Bool) -> CondTree v a -> Condition v
extractCondition p = go
  where
    go (CondNode x cs)
      | not (p x) = Lit False
      | otherwise = goList cs

    goList [] = Lit True
    goList (CondBranch c t e : cs) =
      let
        ct = go t
        ce = maybe (Lit True) go e
       in
        ((c `cAnd` ct) `cOr` (CNot c `cAnd` ce)) `cAnd` goList cs

-- | Flattens a CondTree using a partial flag assignment. When a condition
-- cannot be evaluated, both branches are ignored.
simplifyCondTree
  :: Semigroup a
  => (v -> Either v Bool)
  -> CondTree v a
  -> a
simplifyCondTree env (CondNode a ifs) =
  foldl (<>) a $ mapMaybe (simplifyCondBranch env) ifs

-- | Realizes a 'CondBranch' using partial flag assignment. When a condition
-- cannot be evaluated, returns 'Nothing'.
simplifyCondBranch
  :: Semigroup a
  => (v -> Either v Bool)
  -> CondBranch v a
  -> Maybe a
simplifyCondBranch env (CondBranch cnd t me) =
  case simplifyCondition cnd env of
    (Lit True, _) -> Just $ simplifyCondTree env t
    (Lit False, _) -> fmap (simplifyCondTree env) me
    _ -> Nothing

-- | Flatten a CondTree.  This will resolve the CondTree by taking all
--  possible paths into account.  Note that since branches represent exclusive
--  choices this may not result in a \"sane\" result.
ignoreConditions :: Semigroup a => CondTree v a -> a
ignoreConditions (CondNode a ifs) = foldl (<>) a $ concatMap f ifs
  where
    f (CondBranch _ t me) =
      ignoreConditions t
        : maybeToList (fmap ignoreConditions me)

-- | Flatten a CondTree. This will traverse the CondTree by taking all
--  possible paths into account, but merging inclusive when two paths
--  may co-exist, and exclusively when the paths are an if/else
foldCondTree :: forall b a v. b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> CondTree v a -> b
foldCondTree e u mergeInclusive mergeExclusive = goTree
  where
    goTree :: CondTree v a -> b
    goTree (CondNode a ifs) = u a `mergeInclusive` foldl goBranch e ifs
    goBranch :: b -> CondBranch v a -> b
    goBranch acc (CondBranch _ t mt) = mergeInclusive acc (maybe (goTree t) (mergeExclusive (goTree t) . goTree) mt)
