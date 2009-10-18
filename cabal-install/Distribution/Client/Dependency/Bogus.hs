-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency.Bogus
-- Copyright   :  (c) David Himmelstrup 2005, Bjorn Bringert 2007
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A dependency resolver for when we do not know what packages are installed.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency.Bogus (
    bogusResolver
  ) where

import Distribution.Client.Types
         ( AvailablePackage(..), ConfiguredPackage(..) )
import Distribution.Client.Dependency.Types
         ( DependencyResolver, Progress(..)
         , PackageConstraint(..), PackagePreferences(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Package
         ( PackageName, PackageIdentifier(..), Dependency(..)
         , Package(..), packageVersion )
import Distribution.PackageDescription
         ( GenericPackageDescription(..), CondTree(..), FlagAssignment )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Version
         ( VersionRange, anyVersion, intersectVersionRanges, withinRange )
import Distribution.Simple.Utils
         ( comparing )
import Distribution.Text
         ( display )

import Data.List
         ( maximumBy )
import Data.Maybe
         ( fromMaybe )
import qualified Data.Map as Map

-- | This resolver thinks that every package is already installed.
--
-- We need this for hugs and nhc98 which do not track installed packages.
-- We just pretend that everything is installed and hope for the best.
--
bogusResolver :: DependencyResolver
bogusResolver platform comp _ available
              preferences constraints targets =
    resolveFromAvailable []
      (combineConstraints preferences constraints targets)
  where
    resolveFromAvailable chosen [] = Done chosen
    resolveFromAvailable chosen ((name, verConstraint, flags, verPref): deps) =
      case latestAvailableSatisfying available name verConstraint verPref of
        Nothing  -> Fail ("Unresolved dependency: " ++ display dep)
        Just apkg@(AvailablePackage _ pkg _) ->
          case finalizePackageDescription flags none platform comp [] pkg of
            Right (_, flags') -> Step msg (resolveFromAvailable chosen' deps)
              where
                msg     = "selecting " ++ display (packageId pkg)
                cpkg    = fudgeChosenPackage apkg flags'
                chosen' = InstallPlan.Configured cpkg : chosen
            _ -> error "bogusResolver: impossible happened"
          where
            none :: Dependency -> Bool
            none = const True
      where
        dep = Dependency name verConstraint

fudgeChosenPackage :: AvailablePackage -> FlagAssignment -> ConfiguredPackage
fudgeChosenPackage (AvailablePackage pkgid pkg source) flags =
  ConfiguredPackage (AvailablePackage pkgid (stripDependencies pkg) source)
                    flags ([] :: [PackageIdentifier]) -- empty list of deps
  where
    -- | Pretend that a package has no dependencies. Go through the
    -- 'GenericPackageDescription' and strip them all out.
    --
    stripDependencies :: GenericPackageDescription -> GenericPackageDescription
    stripDependencies gpkg = gpkg {
        condLibrary     = fmap stripDeps (condLibrary gpkg),
        condExecutables = [ (name, stripDeps tree)
                          | (name, tree) <- condExecutables gpkg ]
      }
    stripDeps :: CondTree v [Dependency] a -> CondTree v [Dependency] a
    stripDeps = mapTreeConstrs (const [])

    mapTreeConstrs :: (c -> c) -> CondTree v c a -> CondTree v c a
    mapTreeConstrs f (CondNode a c ifs) = CondNode a (f c) (map g ifs)
      where
        g (cnd, t, me) = (cnd, mapTreeConstrs f t, fmap (mapTreeConstrs f) me)

combineConstraints :: (PackageName -> PackagePreferences)
                   -> [PackageConstraint]
                   -> [PackageName]
                   -> [(PackageName, VersionRange, FlagAssignment, VersionRange)]
combineConstraints preferences constraints targets =
  [ (name, ver, flags, pref)
  | name <- targets
  , let ver   = fromMaybe anyVersion (Map.lookup name versionConstraints)
        flags = fromMaybe []         (Map.lookup name flagsConstraints)
        PackagePreferences pref _ = preferences name ]
  where
    versionConstraints = Map.fromListWith intersectVersionRanges
      [ (name, versionRange)
      | PackageVersionConstraint name versionRange <- constraints ]

    flagsConstraints =  Map.fromListWith (++)
      [ (name, flags)
      | PackageFlagsConstraint name flags <- constraints ]

-- | Gets the best available package satisfying a dependency.
--
latestAvailableSatisfying :: PackageIndex AvailablePackage
                          -> PackageName -> VersionRange -> VersionRange
                          -> Maybe AvailablePackage
latestAvailableSatisfying index name versionConstraint versionPreference =
  case PackageIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy best pkgs)
  where
    dep  = Dependency name versionConstraint
    best = comparing (\p -> (isPreferred p, packageVersion p))
    isPreferred p = packageVersion p `withinRange` versionPreference
