-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency.TopDown.Constraints
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A set of satisfiable dependencies (package version constraints).
-----------------------------------------------------------------------------
module Distribution.Client.Dependency.TopDown.Constraints (
  Constraints,
  empty,
  choices,
  isPaired,

  constrain,
  Satisfiable(..),
  conflicting,
  ) where

import Distribution.Client.Dependency.TopDown.Types
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Client.PackageIndex (PackageIndex)
import Distribution.Package
         ( PackageName, PackageIdentifier(..)
         , Package(packageId), packageName, packageVersion
         , PackageFixedDeps(depends)
         , Dependency(Dependency) )
import Distribution.Version
         ( Version, withinRange )
import Distribution.Client.Utils
         ( mergeBy, MergeResult(..) )

import Data.List
         ( foldl' )
import Data.Monoid
         ( Monoid(mempty) )
import Data.Maybe
         ( catMaybes )
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Exception
         ( assert )

-- | A set of constraints on package versions. For each package name we record
-- what other packages depends on it and what constraints they impose on the
-- version of the package.
--
data (Package installed, Package available)
  => Constraints installed available reason
   = Constraints

       -- Remaining available choices
       (PackageIndex (InstalledOrAvailable installed available))

       -- Paired choices
       (Map PackageName (Version, Version))

       -- Choices that we have excluded for some reason
       -- usually by applying constraints
       (PackageIndex (ExcludedPackage PackageIdentifier reason))

       -- Purely for the invariant, we keep a copy of the original index
       (PackageIndex (InstalledOrAvailable installed available))


data ExcludedPackage pkg reason
   = ExcludedPackage pkg [reason] -- reasons for excluding just the available
                         [reason] -- reasons for excluding installed and avail

instance Package pkg => Package (ExcludedPackage pkg reason) where
  packageId (ExcludedPackage p _ _) = packageId p

-- | There is a conservation of packages property. Packages are never gained or
-- lost, they just transfer from the remaining pot to the excluded pot.
--
invariant :: (Package installed, Package available)
          => Constraints installed available a -> Bool
invariant (Constraints available _ excluded original) = all check merged
  where
    merged = mergeBy (\a b -> packageId a `compare` mergedPackageId b)
                     (PackageIndex.allPackages original)
                     (mergeBy (\a b -> packageId a `compare` packageId b)
                              (PackageIndex.allPackages available)
                              (PackageIndex.allPackages excluded))
      where
        mergedPackageId (OnlyInLeft  p  ) = packageId p
        mergedPackageId (OnlyInRight   p) = packageId p
        mergedPackageId (InBoth      p _) = packageId p

    check (InBoth (InstalledOnly _) cur) = case cur of
      -- If the package was originally installed only then
      -- now it's either still remaining as installed only
      -- or it has been excluded in which case we excluded both
      -- installed and available since it was only installed
      OnlyInLeft  (InstalledOnly _)            -> True
      OnlyInRight (ExcludedPackage _ [] (_:_)) -> True
      _                                        -> False

    check (InBoth (AvailableOnly _) cur) = case cur of
      -- If the package was originally available only then
      -- now it's either still remaining as available only
      -- or it has been excluded in which case we excluded both
      -- installed and available since it was only available
      OnlyInLeft  (AvailableOnly   _)          -> True
      OnlyInRight (ExcludedPackage _ [] (_:_)) -> True
      _                                        -> True

    -- If the package was originally installed and available
    -- then there are three cases.
    check (InBoth (InstalledAndAvailable _ _) cur) = case cur of
      -- We can have both remaining:
      OnlyInLeft                    (InstalledAndAvailable _ _)  -> True
      -- both excluded, in particular it can have had the available excluded
      -- and later had both excluded so we do not mind if the available excluded
      -- is empty or non-empty.
      OnlyInRight                   (ExcludedPackage _ _  (_:_)) -> True
      -- the installed remaining and the available excluded:
      InBoth      (InstalledOnly _) (ExcludedPackage _ (_:_) []) -> True
      _                                                          -> False

    check _ = False

-- | An update to the constraints can move packages between the two piles
-- but not gain or loose packages.
transitionsTo :: (Package installed, Package available)
              => Constraints installed available a
              -> Constraints installed available a -> Bool
transitionsTo constraints @(Constraints available  _ excluded  _)
              constraints'@(Constraints available' _ excluded' _) =
     invariant constraints && invariant constraints'
  && null availableGained  && null excludedLost
  && map packageId availableLost == map packageId excludedGained

  where
    availableLost   = foldr lost [] availableChange where
      lost (OnlyInLeft  pkg)          rest = pkg : rest
      lost (InBoth (InstalledAndAvailable _ pkg)
                   (InstalledOnly _)) rest = AvailableOnly pkg : rest
      lost _                          rest = rest
    availableGained = [ pkg | OnlyInRight pkg <- availableChange ]
    excludedLost    = [ pkg | OnlyInLeft  pkg <- excludedChange  ]
    excludedGained  = [ pkg | OnlyInRight pkg <- excludedChange  ]
                   ++ [ pkg | InBoth (ExcludedPackage _ (_:_) [])
                                 pkg@(ExcludedPackage _ (_:_) (_:_))
                                              <- excludedChange  ]
    availableChange = mergeBy (\a b -> packageId a `compare` packageId b)
                              (PackageIndex.allPackages available)
                              (PackageIndex.allPackages available')
    excludedChange  = mergeBy (\a b -> packageId a `compare` packageId b)
                              (PackageIndex.allPackages excluded)
                              (PackageIndex.allPackages excluded')

-- | We construct 'Constraints' with an initial 'PackageIndex' of all the
-- packages available.
--
empty :: (PackageFixedDeps installed, Package available)
      => PackageIndex installed
      -> PackageIndex available
      -> Constraints installed available reason
empty installed available = Constraints pkgs pairs mempty pkgs
  where
    pkgs = PackageIndex.fromList
         . map toInstalledOrAvailable
         $ mergeBy (\a b -> packageId a `compare` packageId b)
                   (PackageIndex.allPackages installed)
                   (PackageIndex.allPackages available)
    toInstalledOrAvailable (OnlyInLeft  i  ) = InstalledOnly         i
    toInstalledOrAvailable (OnlyInRight   a) = AvailableOnly           a
    toInstalledOrAvailable (InBoth      i a) = InstalledAndAvailable i a

    -- pick up cases like base-3 and 4 where one version depends on the other:
    pairs = Map.fromList
      [ (name, (packageVersion pkgid1, packageVersion pkgid2))
      | [pkg1, pkg2] <- PackageIndex.allPackagesByName installed
      , let name   = packageName pkg1
            pkgid1 = packageId pkg1
            pkgid2 = packageId pkg2
      ,    any ((pkgid1==) . packageId) (depends pkg2)
        || any ((pkgid2==) . packageId) (depends pkg1) ]

-- | The package choices that are still available.
--
choices :: (Package installed, Package available)
        => Constraints installed available reason
        -> PackageIndex (InstalledOrAvailable installed available)
choices (Constraints available _ _ _) = available

isPaired :: (Package installed, Package available)
         => Constraints installed available reason
         -> PackageIdentifier -> Maybe PackageIdentifier
isPaired (Constraints _ pairs _ _) (PackageIdentifier name version) =
  case Map.lookup name pairs of
    Just (v1, v2)
      | version == v1 -> Just (PackageIdentifier name v2)
      | version == v2 -> Just (PackageIdentifier name v1)
    _                 -> Nothing

data Satisfiable constraints discarded reason
       = Satisfiable constraints discarded
       | Unsatisfiable
       | ConflictsWith [(PackageIdentifier, [reason])]

constrain :: (Package installed, Package available)
          => TaggedDependency
          -> reason
          -> Constraints installed available reason
          -> Satisfiable (Constraints installed available reason)
                         [PackageIdentifier] reason
constrain (TaggedDependency installedConstraint (Dependency name versionRange))
          reason constraints@(Constraints available paired excluded original)

  | not anyRemaining
  = if null conflicts then Unsatisfiable
                      else ConflictsWith conflicts

  | otherwise
  = let constraints' = Constraints available' paired excluded' original
     in assert (constraints `transitionsTo` constraints') $
        Satisfiable constraints' (map packageId newExcluded)

  where
  -- This tells us if any packages would remain at all for this package name if
  -- we applied this constraint. This amounts to checking if any package
  -- satisfies the given constraint, including version range and installation
  -- status.
  --
  anyRemaining = any satisfiesConstraint availableChoices

  conflicts = [ (packageId pkg, reasonsAvail ++ reasonsAll)
              | ExcludedPackage pkg reasonsAvail reasonsAll <- excludedChoices
              , satisfiesVersionConstraint pkg ]

  -- Applying this constraint may involve deleting some choices for this
  -- package name, or restricting which install states are available.
  available' = updateAvailable available
  updateAvailable = flip (foldl' (flip update)) availableChoices where
    update pkg | not (satisfiesVersionConstraint pkg)
               = PackageIndex.deletePackageId (packageId pkg)
    update _   | installedConstraint == NoInstalledConstraint
               = id
    update pkg = case pkg of
      InstalledOnly         _   -> id
      AvailableOnly           _ -> PackageIndex.deletePackageId (packageId pkg)
      InstalledAndAvailable i _ -> PackageIndex.insert (InstalledOnly i)

  -- Applying the constraint means adding exclusions for the packages that
  -- we're just freshly excluding, ie the ones we're removing from available.
  excluded' = foldl' (flip PackageIndex.insert) excluded
                (newExcluded ++ oldExcluded)

  newExcluded = catMaybes (map exclude availableChoices) where
    exclude pkg
      | not (satisfiesVersionConstraint pkg)
      = Just (ExcludedPackage pkgid [] [reason])
      | installedConstraint == NoInstalledConstraint
      = Nothing
      | otherwise = case pkg of
      InstalledOnly         _   -> Nothing
      AvailableOnly           _ -> Just (ExcludedPackage pkgid [] [reason])
      InstalledAndAvailable _ _ ->
        case PackageIndex.lookupPackageId excluded pkgid of
          Just (ExcludedPackage _ avail both)
                  -> Just (ExcludedPackage pkgid (reason:avail) both)
          Nothing -> Just (ExcludedPackage pkgid [reason] [])
      where pkgid = packageId pkg

  -- Additionally we have to add extra exclusions for any already-excluded
  -- packages that happen to be covered by the (inverse of the) constraint.
  oldExcluded = catMaybes (map exclude excludedChoices) where
    exclude (ExcludedPackage pkgid avail both)
      -- if it doesn't satisfy the version constraint then we exclude the
      -- package as a whole, the available or the installed instances or both.
      | not (satisfiesVersionConstraint pkgid)
      = Just (ExcludedPackage pkgid avail (reason:both))
      -- if on the other hand it does satisfy the constraint and we were also
      -- constraining to just the installed version then we exclude just the
      -- available instance.
      | installedConstraint == InstalledConstraint
      = Just (ExcludedPackage pkgid (reason:avail) both)
      | otherwise = Nothing

  -- util definitions
  availableChoices = PackageIndex.lookupPackageName available name
  excludedChoices  = PackageIndex.lookupPackageName excluded  name

  satisfiesConstraint pkg = satisfiesVersionConstraint pkg
                         && satisfiesInstallStateConstraint pkg

  satisfiesVersionConstraint :: Package pkg => pkg -> Bool
  satisfiesVersionConstraint = case Map.lookup name paired of
    Nothing       -> \pkg ->
      packageVersion pkg `withinRange` versionRange
    Just (v1, v2) -> \pkg -> case packageVersion pkg of
      v | v == v1
       || v == v2   -> v1 `withinRange` versionRange
                    || v2 `withinRange` versionRange
        | otherwise -> v `withinRange` versionRange

  satisfiesInstallStateConstraint = case installedConstraint of
    NoInstalledConstraint -> \_   -> True
    InstalledConstraint   -> \pkg -> case pkg of
      AvailableOnly _             -> False
      _                           -> True

conflicting :: (Package installed, Package available)
            => Constraints installed available reason
            -> Dependency
            -> [(PackageIdentifier, [reason])]
conflicting (Constraints _ _ excluded _) dep =
  [ (pkgid, reasonsAvail ++ reasonsAll) --TODO
  | ExcludedPackage pkgid reasonsAvail reasonsAll <-
      PackageIndex.lookupDependency excluded dep ]
