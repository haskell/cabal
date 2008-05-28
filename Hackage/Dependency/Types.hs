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
  ) where

import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..) )
import qualified Hackage.InstallPlan as InstallPlan

import Distribution.Package
         ( Dependency )
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
                         -> Either [Dependency] [InstallPlan.PlanPackage a]
