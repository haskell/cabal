-- | Fine-grained package dependencies
--
-- Like many others, this module is meant to be "double-imported":
--
-- > import Distribution.Client.ComponentDeps (
-- >     Component
-- >   , ComponentDep
-- >   , ComponentDeps
-- >   )
-- > import qualified Distribution.Client.ComponentDeps as CD
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Distribution.Client.ComponentDeps (
    -- * Fine-grained package dependencies
    Component(..)
  , ComponentDep
  , ComponentDeps -- opaque
    -- ** Constructing ComponentDeps
  , empty
  , fromList
  , singleton
  , insert
  , fromLibraryDeps
  , fromSetupDeps
  , fromInstalled
    -- ** Deconstructing ComponentDeps
  , toList
  , flatDeps
  , nonSetupDeps
  , libraryDeps
  , setupDeps
  , select
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (fold)

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))
#endif

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Component of a package
data Component =
    ComponentLib
  | ComponentExe   String
  | ComponentTest  String
  | ComponentBench String
  | ComponentSetup
  deriving (Show, Eq, Ord)

-- | Dependency for a single component
type ComponentDep a = (Component, a)

-- | Fine-grained dependencies for a package
newtype ComponentDeps a = ComponentDeps { unComponentDeps :: Map Component a }
  deriving (Show, Functor, Eq, Ord)

instance Monoid a => Monoid (ComponentDeps a) where
  mempty =
    ComponentDeps Map.empty
  (ComponentDeps d) `mappend` (ComponentDeps d') =
    ComponentDeps (Map.unionWith mappend d d')

instance Foldable ComponentDeps where
  foldMap f = foldMap f . unComponentDeps

instance Traversable ComponentDeps where
  traverse f = fmap ComponentDeps . traverse f . unComponentDeps

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: ComponentDeps a
empty = ComponentDeps $ Map.empty

fromList :: Monoid a => [ComponentDep a] -> ComponentDeps a
fromList = ComponentDeps . Map.fromListWith mappend

singleton :: Component -> a -> ComponentDeps a
singleton comp = ComponentDeps . Map.singleton comp

insert :: Monoid a => Component -> a -> ComponentDeps a -> ComponentDeps a
insert comp a = ComponentDeps . Map.alter aux comp . unComponentDeps
  where
    aux Nothing   = Just a
    aux (Just a') = Just $ a `mappend` a'

-- | ComponentDeps containing library dependencies only
fromLibraryDeps :: a -> ComponentDeps a
fromLibraryDeps = singleton ComponentLib

-- | ComponentDeps containing setup dependencies only
fromSetupDeps :: a -> ComponentDeps a
fromSetupDeps = singleton ComponentSetup

-- | ComponentDeps for installed packages
--
-- We assume that installed packages only record their library dependencies
fromInstalled :: a -> ComponentDeps a
fromInstalled = fromLibraryDeps

{-------------------------------------------------------------------------------
  Deconstruction
-------------------------------------------------------------------------------}

toList :: ComponentDeps a -> [ComponentDep a]
toList = Map.toList . unComponentDeps

-- | All dependencies of a package
--
-- This is just a synonym for 'fold', but perhaps a use of 'flatDeps' is more
-- obvious than a use of 'fold', and moreover this avoids introducing lots of
-- @#ifdef@s for 7.10 just for the use of 'fold'.
flatDeps :: Monoid a => ComponentDeps a -> a
flatDeps = fold

-- | All dependencies except the setup dependencies
--
-- Prior to the introduction of setup dependencies (TODO: Version? 1.23) this
-- would have been _all_ dependencies
nonSetupDeps :: Monoid a => ComponentDeps a -> a
nonSetupDeps = select (/= ComponentSetup)

-- | Library dependencies proper only
libraryDeps :: Monoid a => ComponentDeps a -> a
libraryDeps = select (== ComponentLib)

-- | Setup dependencies
setupDeps :: Monoid a => ComponentDeps a -> a
setupDeps = select (== ComponentSetup)

-- | Select dependencies satisfying a given predicate
select :: Monoid a => (Component -> Bool) -> ComponentDeps a -> a
select p = foldMap snd . filter (p . fst) . toList
