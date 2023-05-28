module Distribution.Types.DependencyMap
  ( DependencyMap
  , toDepMap
  , fromDepMap
  , constrainBy
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Dependency
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint
import Distribution.Version

import qualified Data.Map.Lazy as Map

-- | A map of dependencies.  Newtyped since the default monoid instance is not
--   appropriate.  The monoid instance uses 'intersectVersionRanges'.
newtype DependencyMap = DependencyMap {unDependencyMap :: Map PackageName (VersionRange, NonEmptySet LibraryName)}
  deriving (Show, Read, Eq)

instance Monoid DependencyMap where
  mempty = DependencyMap Map.empty
  mappend = (<>)

instance Semigroup DependencyMap where
  (DependencyMap a) <> (DependencyMap b) =
    DependencyMap (Map.unionWith intersectVersionRangesAndJoinComponents a b)

intersectVersionRangesAndJoinComponents
  :: (VersionRange, NonEmptySet LibraryName)
  -> (VersionRange, NonEmptySet LibraryName)
  -> (VersionRange, NonEmptySet LibraryName)
intersectVersionRangesAndJoinComponents (va, ca) (vb, cb) =
  (intersectVersionRanges va vb, ca <> cb)

toDepMap :: [Dependency] -> DependencyMap
toDepMap ds =
  DependencyMap $ Map.fromListWith intersectVersionRangesAndJoinComponents [(p, (vr, cs)) | Dependency p vr cs <- ds]

fromDepMap :: DependencyMap -> [Dependency]
fromDepMap m = [Dependency p vr cs | (p, (vr, cs)) <- Map.toList (unDependencyMap m)]

-- Apply extra constraints to a dependency map.
-- Combines dependencies where the result will only contain keys from the left
-- (first) map.  If a key also exists in the right map, both constraints will
-- be intersected.
constrainBy
  :: DependencyMap
  -> [PackageVersionConstraint]
  -> DependencyMap
constrainBy = foldl' tightenConstraint
  where
    tightenConstraint (DependencyMap l) (PackageVersionConstraint pn vr) = DependencyMap $
      case Map.lookup pn l of
        Nothing -> l
        Just (vr', cs) -> Map.insert pn (intersectVersionRanges vr' vr, cs) l
