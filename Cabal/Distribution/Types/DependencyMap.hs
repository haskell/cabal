{-# LANGUAGE CPP #-}

#ifdef MIN_VERSION_containers
#if MIN_VERSION_containers(0,5,0)
#define MIN_VERSION_containers_0_5_0
#endif
#endif

#ifndef MIN_VERSION_containers
#if __GLASGOW_HASKELL__ >= 706
#define MIN_VERSION_containers_0_5_0
#endif
#endif

module Distribution.Types.DependencyMap (
    DependencyMap(..),
    toDepMap,
    fromDepMap,
    lookupDepMap,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Dependency
import Distribution.Types.PackageName
import Distribution.Version

#ifdef MIN_VERSION_containers_0_5_0
import qualified Data.Map.Lazy as Map
#else
import qualified Data.Map as Map
#endif

-- | A map of dependencies.  Newtyped since the default monoid instance is not
--   appropriate.  The monoid instance uses 'intersectVersionRanges'.
newtype DependencyMap = DependencyMap { unDependencyMap :: Map PackageName VersionRange }
  deriving (Show, Read)

instance Monoid DependencyMap where
    mempty = DependencyMap Map.empty
    mappend = (<>)

instance Semigroup DependencyMap where
    (DependencyMap a) <> (DependencyMap b) =
        DependencyMap (Map.unionWith intersectVersionRanges a b)

toDepMap :: [Dependency] -> DependencyMap
toDepMap ds =
  DependencyMap $ Map.fromListWith intersectVersionRanges [ (p,vr) | Dependency p vr <- ds ]

fromDepMap :: DependencyMap -> [Dependency]
fromDepMap m = [ Dependency p vr | (p,vr) <- Map.toList (unDependencyMap m) ]

lookupDepMap :: DependencyMap -> PackageName -> Maybe VersionRange
lookupDepMap (DependencyMap m) pn = Map.lookup pn m
