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

module Distribution.Types.LibDependencyMap
  ( LibDependencyMap
  , toLibDepMap
  , fromLibDepMap
  , lookupLibDepMap
  , discardLibNames
  , constrainBy
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.PackageName
import Distribution.Types.LibDependency
import Distribution.Types.DependencyMap
import Distribution.Types.UnqualComponentName
import Distribution.Version

#ifdef MIN_VERSION_containers_0_5_0
import qualified Data.Map.Lazy as Map
#else
import qualified Data.Map as Map
#endif
import qualified Data.Set as Set

-- | A map of dependencies.  Newtyped since the default monoid instance is not
--   appropriate.  The monoid instance uses 'intersectVersionRanges'.
newtype LibDependencyMap = LibDependencyMap {
    unLibDependencyMap :: Map PackageName ( Set.Set (Maybe UnqualComponentName)
                                          , VersionRange )
  }
  deriving (Show, Read)

instance Monoid LibDependencyMap where
    mempty = LibDependencyMap Map.empty
    mappend = (<>)

instance Semigroup LibDependencyMap where
    (LibDependencyMap a) <> (LibDependencyMap b) =
        LibDependencyMap (Map.unionWith combineValue a b)

combineValue :: (Set.Set (Maybe UnqualComponentName), VersionRange)
             -> (Set.Set (Maybe UnqualComponentName), VersionRange)
             -> (Set.Set (Maybe UnqualComponentName), VersionRange)
combineValue (cs0, vr0) (cs1, vr1) = ( Set.union cs0 cs1
                                     , intersectVersionRanges vr0 vr1 )

toLibDepMap :: [LibDependency] -> LibDependencyMap
toLibDepMap ds = LibDependencyMap $ Map.fromListWith
  combineValue
  [ (p, (Set.singleton l, vr)) | LibDependency p l vr <- ds ]

fromLibDepMap :: LibDependencyMap -> [LibDependency]
fromLibDepMap m = [ LibDependency p l vr
                  | (p, (ls, vr)) <- Map.toList (unLibDependencyMap m)
                  , l <- Set.toList ls ]

lookupLibDepMap :: LibDependencyMap
                -> PackageName
                -> Maybe (Set.Set (Maybe UnqualComponentName), VersionRange)
lookupLibDepMap (LibDependencyMap m) pn = Map.lookup pn m

discardLibNames :: LibDependencyMap -> DependencyMap
discardLibNames = DependencyMap . fmap (\(_, vr) -> vr) . unLibDependencyMap

-- Apply extra constraints to a dependency map.
-- Combines dependencies where the result will only contain keys from the left
-- (first) map.  If a key also exists in the right map, both constraints will
-- be intersected.
constrainBy :: LibDependencyMap  -- ^ Input map
            -> DependencyMap  -- ^ Extra constraints
            -> LibDependencyMap
constrainBy left extra = LibDependencyMap $
  fold tightenConstraint (unLibDependencyMap left)
                         (unDependencyMap extra)
  where tightenConstraint n v l =
            case Map.lookup n l of
              Nothing -> l
              Just entry -> Map.insert n (combineValue entry (Set.empty, v)) l
        fold =
#ifdef MIN_VERSION_containers_0_5_0
          Map.foldrWithKey
#else
          Map.foldWithKey
#endif
