-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Dependency
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
module Hackage.Dependency.Bogus (
    bogusResolver
  ) where

import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..)
         , ConfiguredPackage(..) )
import Hackage.Dependency.Types
         ( DependencyResolver, Progress(..) )
import Distribution.Package
         ( PackageIdentifier(..), Dependency(..), Package(..) )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription)
import Distribution.Simple.Utils (comparing)
import Hackage.Utils
         ( showDependencies )

import Data.List (maximumBy)

-- | This resolver thinks that every package is already installed.
--
-- We need this for hugs and nhc98 which do not track installed packages.
-- We just pretend that everything is installed and hope for the best.
--
bogusResolver :: DependencyResolver a
bogusResolver os arch comp _ available _ deps =
  case unzipEithers (map resolveFromAvailable deps) of
    (ok, [])      -> Done ok
    (_ , missing) -> Fail $ "Unresolved dependencies: "
                         ++ showDependencies missing
  where
    resolveFromAvailable (UnresolvedDependency dep flags) =
      case latestAvailableSatisfying available dep of
        Nothing  -> Right dep
        Just apkg@(AvailablePackage _ pkg _) ->
          case finalizePackageDescription flags none os arch comp [] pkg of
            Right (_, flags') -> Left $ InstallPlan.Configured $
                                   ConfiguredPackage apkg flags' []
            --TODO: we have to add PreExisting deps of pkg, otherwise
            -- the install plan verifier will say we're missing deps.
            _ -> error "bogusResolver: impossible happened"
          where
            none :: Maybe (PackageIndex PackageIdentifier)
            none = Nothing

-- | Gets the latest available package satisfying a dependency.
latestAvailableSatisfying :: PackageIndex AvailablePackage
                          -> Dependency
                          -> Maybe AvailablePackage
latestAvailableSatisfying index dep =
  case PackageIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing (pkgVersion . packageId)) pkgs)

unzipEithers :: [Either a b] -> ([a], [b])
unzipEithers = foldr (flip consEither) ([], [])
  where consEither ~(ls,rs) = either (\l -> (l:ls,rs)) (\r -> (ls,r:rs))
