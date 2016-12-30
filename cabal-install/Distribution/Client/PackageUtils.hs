-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PackageUtils
-- Copyright   :  (c) Duncan Coutts 2010
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Various package description utils that should be in the Cabal lib
-----------------------------------------------------------------------------
module Distribution.Client.PackageUtils (
    externalBuildDepends,
  ) where

import Distribution.Package
         ( packageName )
import Distribution.Types.ComponentRequestedSpec
         ( ComponentRequestedSpec )
import Distribution.Types.Dependency
import Distribution.Types.LibDependency
import Distribution.PackageDescription
         ( PackageDescription(..),  enabledBuildDepends )

-- | The list of dependencies that refer to external packages rather than
-- internal package components.
--
-- External deps should not be on a sub-lib, and internal deps should have a
-- compatable version range with the current package (or none at all), but Cabal
-- enforces these invariants so we need not worry about them.
externalBuildDepends :: PackageDescription -> ComponentRequestedSpec -> [Dependency]
externalBuildDepends pkg spec = [ libDependencyToDependency dep
                                | dep <- enabledBuildDepends pkg spec
                                , libDepPackageName dep /= packageName pkg ]
