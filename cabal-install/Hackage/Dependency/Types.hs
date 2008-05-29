-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Dependency.Types
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Common types for dependency resolution.
-----------------------------------------------------------------------------
module Hackage.Dependency.Types (
    DependencyResolver,
    Progress(..),
    foldProgress,
  ) where

import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..) )
import qualified Hackage.InstallPlan as InstallPlan

import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Simple.PackageIndex
         ( PackageIndex )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( OS, Arch )

import Prelude hiding (fail)

-- | A dependency resolver is a function that works out an installation plan
-- given the set of installed and available packages and a set of deps to
-- solve for.
--
-- The reason for this interface is because there are dozens of approaches to
-- solving the package dependency problem and we want to make it easy to swap
-- in alternatives.
--
type DependencyResolver a = OS
                         -> Arch
                         -> CompilerId
                         -> PackageIndex InstalledPackageInfo
                         -> PackageIndex AvailablePackage
                         -> [UnresolvedDependency]
                         -> Progress String String [InstallPlan.PlanPackage a]

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- retult which may be used to indicate progress and/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done

-- | Consume a 'Progres' calculation. Much like 'foldr' for lists but with
-- two base cases, one for a final result and one for failure.
--
-- Eg to convert into a simple 'Either' result use:
--
-- > foldProgress (flip const) Left Right
--
foldProgress :: (step -> a -> a) -> (fail -> a) -> (done -> a)
             -> Progress step fail done -> a
foldProgress step fail done = fold
  where fold (Step s p) = step s (fold p)
        fold (Fail f)   = fail f
        fold (Done r)   = done r

instance Functor (Progress step failure) where
  fmap f = foldProgress Step Fail (Done . f)
