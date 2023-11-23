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
newtype DependencyMap = DependencyMap {unDependencyMap :: Map (PackageName, IsPrivate) (VersionRange, NonEmptySet LibraryName)}
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

toDepMap :: Dependencies -> DependencyMap
toDepMap ds =
  DependencyMap $
    Map.fromListWith
      intersectVersionRangesAndJoinComponents
      ( [((p, Public), (vr, cs)) | Dependency p vr cs <- publicDependencies ds]
          ++ [((p, Private pn), (vr, cs)) | PrivateDependency pn pds <- privateDependencies ds, Dependency p vr cs <- pds]
      )

fromDepMap :: DependencyMap -> Dependencies
fromDepMap m =
  Dependencies
    [Dependency p vr cs | ((p, Public), (vr, cs)) <- Map.toList (unDependencyMap m)]
    [PrivateDependency alias deps | (alias, deps) <- Map.toList priv_deps]
  where
    priv_deps = Map.fromListWith (++) [(sn, [Dependency p vr cs]) | ((p, (Private sn)), (vr, cs)) <- Map.toList (unDependencyMap m)]

-- Apply extra constraints to a dependency map.
-- Combines dependencies where the result will only contain keys from the left
-- (first) map.  If a key also exists in the right map, both constraints will
-- be intersected.
constrainBy
  :: DependencyMap
  -> [(IsPrivate, PackageVersionConstraint)]
  -> DependencyMap
constrainBy = foldl' tightenConstraint
  where
    tightenConstraint (DependencyMap l) (ip, PackageVersionConstraint pn vr) = DependencyMap $
      case Map.lookup (pn, ip) l of
        Nothing -> l
        Just (vr', cs) -> Map.insert (pn, ip) (intersectVersionRanges vr' vr, cs) l
