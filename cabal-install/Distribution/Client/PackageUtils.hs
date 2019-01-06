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

import Distribution.Package                      (packageName, packageVersion)
import Distribution.PackageDescription
       (PackageDescription (..), enabledBuildDepends, libName)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec)
import Distribution.Types.Dependency
import Distribution.Types.LibraryName
import Distribution.Types.UnqualComponentName
import Distribution.Version                      (isAnyVersion, withinRange)

-- | The list of dependencies that refer to external packages
-- rather than internal package components.
--
externalBuildDepends :: PackageDescription -> ComponentRequestedSpec -> [Dependency]
externalBuildDepends pkg spec = filter (not . internal) (enabledBuildDepends pkg spec)
  where
    -- True if this dependency is an internal one (depends on a library
    -- defined in the same package).
    internal (Dependency depName versionRange _) =
           (depName == packageName pkg &&
            packageVersion pkg `withinRange` versionRange) ||
           (LSubLibName (packageNameToUnqualComponentName depName) `elem` map libName (subLibraries pkg) &&
            isAnyVersion versionRange)
