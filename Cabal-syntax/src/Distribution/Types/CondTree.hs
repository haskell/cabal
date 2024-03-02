{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Types.CondTree
  ( CondTree (..)
  , CondBranch (..)
  , condIfThen
  , condIfThenElse
  , foldCondTree
  , mapCondTree
  , mapTreeConstrs
  , mapTreeConds
  , mapTreeData
  , traverseCondTreeV
  , traverseCondBranchV
  , traverseCondTreeC
  , traverseCondBranchC
  , extractCondition
  , simplifyCondTree
  , simplifyCondBranch
  , ignoreConditions
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Condition

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
data CondTree v c a = CondNode
  { condTreeData :: a
  , condTreeConstraints :: c
  , condTreeComponents :: [CondBranch v c a]
  }
  deriving (Show, Eq, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance (Binary v, Binary c, Binary a) => Binary (CondTree v c a)
instance (Structured v, Structured c, Structured a) => Structured (CondTree v c a)
instance (NFData v, NFData c, NFData a) => NFData (CondTree v c a) where rnf = genericRnf

instance (Semigroup a, Semigroup c) => Semigroup (CondTree v c a) where
  (CondNode a c bs) <> (CondNode a' c' bs') = CondNode (a <> a') (c <> c') (bs <> bs')

instance (Semigroup a, Semigroup c, Monoid a, Monoid c) => Monoid (CondTree v c a) where
  mappend = (<>)
  mempty = CondNode mempty mempty mempty

-- | A 'CondBranch' represents a conditional branch, e.g., @if
-- flag(foo)@ on some syntax @a@.  It also has an optional false
-- branch.
data CondBranch v c a = CondBranch
  { condBranchCondition :: Condition v
  , condBranchIfTrue :: CondTree v c a
  , condBranchIfFalse :: Maybe (CondTree v c a)
  }
  deriving (Show, Eq, Typeable, Data, Generic, Functor, Traversable)

-- This instance is written by hand because GHC 8.0.1/8.0.2 infinite
-- loops when trying to derive it with optimizations.  See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/13056
instance Foldable (CondBranch v c) where
  foldMap f (CondBranch _ c Nothing) = foldMap f c
  foldMap f (CondBranch _ c (Just a)) = foldMap f c `mappend` foldMap f a

instance (Binary v, Binary c, Binary a) => Binary (CondBranch v c a)
instance (Structured v, Structured c, Structured a) => Structured (CondBranch v c a)
instance (NFData v, NFData c, NFData a) => NFData (CondBranch v c a) where rnf = genericRnf

condIfThen :: Condition v -> CondTree v c a -> CondBranch v c a
condIfThen c t = CondBranch c t Nothing

condIfThenElse :: Condition v -> CondTree v c a -> CondTree v c a -> CondBranch v c a
condIfThenElse c t e = CondBranch c t (Just e)

mapCondTree
  :: (a -> b)
  -> (c -> d)
  -> (Condition v -> Condition w)
  -> CondTree v c a
  -> CondTree w d b
mapCondTree fa fc fcnd (CondNode a c ifs) =
  CondNode (fa a) (fc c) (map g ifs)
  where
    g (CondBranch cnd t me) =
      CondBranch
        (fcnd cnd)
        (mapCondTree fa fc fcnd t)
        (fmap (mapCondTree fa fc fcnd) me)

mapTreeConstrs :: (c -> d) -> CondTree v c a -> CondTree v d a
mapTreeConstrs f = mapCondTree id f id

mapTreeConds :: (Condition v -> Condition w) -> CondTree v c a -> CondTree w c a
mapTreeConds f = mapCondTree id id f

mapTreeData :: (a -> b) -> CondTree v c a -> CondTree v c b
mapTreeData f = mapCondTree f id id

-- | @@Traversal@@ for the variables
traverseCondTreeV :: L.Traversal (CondTree v c a) (CondTree w c a) v w
traverseCondTreeV f (CondNode a c ifs) =
  CondNode a c <$> traverse (traverseCondBranchV f) ifs

-- | @@Traversal@@ for the variables
traverseCondBranchV :: L.Traversal (CondBranch v c a) (CondBranch w c a) v w
traverseCondBranchV f (CondBranch cnd t me) =
  CondBranch
    <$> traverse f cnd
    <*> traverseCondTreeV f t
    <*> traverse (traverseCondTreeV f) me

-- | @@Traversal@@ for the aggregated constraints
traverseCondTreeC :: L.Traversal (CondTree v c a) (CondTree v d a) c d
traverseCondTreeC f (CondNode a c ifs) =
  CondNode a <$> f c <*> traverse (traverseCondBranchC f) ifs

-- | @@Traversal@@ for the aggregated constraints
traverseCondBranchC :: L.Traversal (CondBranch v c a) (CondBranch v d a) c d
traverseCondBranchC f (CondBranch cnd t me) =
  CondBranch cnd
    <$> traverseCondTreeC f t
    <*> traverse (traverseCondTreeC f) me

-- | Extract the condition matched by the given predicate from a cond tree.
--
-- We use this mainly for extracting buildable conditions (see the Note in
-- Distribution.PackageDescription.Configuration), but the function is in fact
-- more general.
extractCondition :: Eq v => (a -> Bool) -> CondTree v c a -> Condition v
extractCondition p = go
  where
    go (CondNode x _ cs)
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
  :: (Semigroup a, Semigroup d)
  => (v -> Either v Bool)
  -> CondTree v d a
  -> (d, a)
simplifyCondTree env (CondNode a d ifs) =
  foldl (<>) (d, a) $ mapMaybe (simplifyCondBranch env) ifs

-- | Realizes a 'CondBranch' using partial flag assignment. When a condition
-- cannot be evaluated, returns 'Nothing'.
simplifyCondBranch
  :: (Semigroup a, Semigroup d)
  => (v -> Either v Bool)
  -> CondBranch v d a
  -> Maybe (d, a)
simplifyCondBranch env (CondBranch cnd t me) =
  case simplifyCondition cnd env of
    (Lit True, _) -> Just $ simplifyCondTree env t
    (Lit False, _) -> fmap (simplifyCondTree env) me
    _ -> Nothing

-- | Flatten a CondTree.  This will resolve the CondTree by taking all
--  possible paths into account.  Note that since branches represent exclusive
--  choices this may not result in a \"sane\" result.
ignoreConditions :: (Semigroup a, Semigroup c) => CondTree v c a -> (a, c)
ignoreConditions (CondNode a c ifs) = foldl (<>) (a, c) $ concatMap f ifs
  where
    f (CondBranch _ t me) =
      ignoreConditions t
        : maybeToList (fmap ignoreConditions me)

-- | Flatten a CondTree. This will traverse the CondTree by taking all
--  possible paths into account, but merging inclusive when two paths
--  may co-exist, and exclusively when the paths are an if/else
foldCondTree :: forall b c a v. b -> ((c, a) -> b) -> (b -> b -> b) -> (b -> b -> b) -> CondTree v c a -> b
foldCondTree e u mergeInclusive mergeExclusive = goTree
  where
    goTree :: CondTree v c a -> b
    goTree (CondNode a c ifs) = u (c, a) `mergeInclusive` foldl goBranch e ifs
    goBranch :: b -> CondBranch v c a -> b
    goBranch acc (CondBranch _ t mt) = mergeInclusive acc (maybe (goTree t) (mergeExclusive (goTree t) . goTree) mt)
