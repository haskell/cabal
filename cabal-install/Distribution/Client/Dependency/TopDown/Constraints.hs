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
  
  constrain,
  Satisfiable(..),
  conflicting,
  ) where

import Distribution.Client.Dependency.TopDown.Types
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Package
         ( PackageIdentifier, Package(packageId), packageVersion
         , Dependency(Dependency) )
import Distribution.Version
         ( withinRange )
import Distribution.Client.Utils
         ( mergeBy, MergeResult(..) )

import Data.List
         ( foldl' )
import Data.Monoid
         ( Monoid(mempty) )
import Data.Maybe
         ( catMaybes )
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
       
       -- Choices that we have excluded for some reason
       -- usually by applying constraints
       (PackageIndex (ExcludedPackage PackageIdentifier reason))

data ExcludedPackage pkg reason
   = ExcludedPackage pkg [reason] -- reasons for excluding just the available
                         [reason] -- reasons for excluding installed and avail

instance Package pkg => Package (ExcludedPackage pkg reason) where
  packageId (ExcludedPackage p _ _) = packageId p

-- | The intersection between the two indexes is empty
invariant :: (Package installed, Package available)
          => Constraints installed available a -> Bool
invariant (Constraints available excluded) =
  all (uncurry ok) [ (a, e) | InBoth a e <- merged ]
  where
    merged = mergeBy (\a b -> packageId a `compare` packageId b)
                     (PackageIndex.allPackages available)
                     (PackageIndex.allPackages excluded)
    ok (InstalledOnly _) (ExcludedPackage _ _ []) = True
    ok _                 _                        = False

-- | An update to the constraints can move packages between the two piles
-- but not gain or loose packages.
transitionsTo :: (Package installed, Package available)
              => Constraints installed available a
              -> Constraints installed available a -> Bool
transitionsTo constraints @(Constraints available  excluded )
              constraints'@(Constraints available' excluded') =
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
    availableChange = mergeBy (\a b -> packageId a `compare` packageId b)
                              (PackageIndex.allPackages available)
                              (PackageIndex.allPackages available')
    excludedChange  = mergeBy (\a b -> packageId a `compare` packageId b)
                              (PackageIndex.allPackages excluded)
                              (PackageIndex.allPackages excluded')

-- | We construct 'Constraints' with an initial 'PackageIndex' of all the
-- packages available.
--
empty :: (Package installed, Package available)
      => PackageIndex installed
      -> PackageIndex available
      -> Constraints installed available reason
empty installed available = Constraints pkgs mempty
  where
    pkgs = PackageIndex.fromList
         . map toInstalledOrAvailable
         $ mergeBy (\a b -> packageId a `compare` packageId b)
                   (PackageIndex.allPackages installed)
                   (PackageIndex.allPackages available)
    toInstalledOrAvailable (OnlyInLeft  i  ) = InstalledOnly         i
    toInstalledOrAvailable (OnlyInRight   a) = AvailableOnly           a
    toInstalledOrAvailable (InBoth      i a) = InstalledAndAvailable i a

-- | The package choices that are still available.
--
choices :: (Package installed, Package available)
        => Constraints installed available reason
        -> PackageIndex (InstalledOrAvailable installed available)
choices (Constraints available _) = available

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
          reason constraints@(Constraints available excluded)

  | not anyRemaining
  = if null conflicts then Unsatisfiable
                      else ConflictsWith conflicts

  | otherwise 
  = let constraints' = Constraints available' excluded'
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
      AvailableOnly           _ -> error "impossible" -- PackageIndex.deletePackageId (packageId pkg)
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
      AvailableOnly           _ -> Just (ExcludedPackage pkgid [reason] [])
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

  satisfiesVersionConstraint pkg =
    packageVersion pkg `withinRange` versionRange

  satisfiesInstallStateConstraint = case installedConstraint of
    NoInstalledConstraint -> \_   -> True
    InstalledConstraint   -> \pkg -> case pkg of
      AvailableOnly _             -> False
      _                           -> True

conflicting :: (Package installed, Package available)
            => Constraints installed available reason
            -> Dependency
            -> [(PackageIdentifier, [reason])]
conflicting (Constraints _ excluded) dep =
  [ (pkgid, reasonsAvail ++ reasonsAll) --TODO
  | ExcludedPackage pkgid reasonsAvail reasonsAll <-
      PackageIndex.lookupDependency excluded dep ]
