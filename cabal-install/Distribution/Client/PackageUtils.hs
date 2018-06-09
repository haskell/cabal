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
         ( packageVersion, packageName )
import Distribution.Types.ComponentRequestedSpec
         ( ComponentRequestedSpec )
import Distribution.Types.Dependency
import Distribution.Types.UnqualComponentName
import Distribution.PackageDescription
         ( PackageDescription(..), libName, enabledBuildDepends )
import Distribution.Version
         ( withinRange, isAnyVersion )

-- | The list of dependencies that refer to external packages
-- rather than internal package components.
--
externalBuildDepends :: PackageDescription -> ComponentRequestedSpec -> [Dependency]
externalBuildDepends pkg spec = filter (not . internal) (enabledBuildDepends pkg spec)
  where
    -- True if this dependency is an internal one (depends on a library
    -- defined in the same package).
    -- TODO With the new syntax, this is not good.
    -- A dep on an internal lib should be something like
    -- "build-depends: thispkgname {internallib1,internallib2}"
    -- and not "build-depends: internallib" (how it's done now),
    -- as it's incoherent with the new syntax and requires that
    -- packagaName->UnqualComponentName conversion.
    -- Can we correct this?
    internal (Dependency depName versionRange _) =
           (depName == packageName pkg &&
            packageVersion pkg `withinRange` versionRange) ||
           (Just (packageNameToUnqualComponentName depName) `elem` map libName (subLibraries pkg) &&
            isAnyVersion versionRange)
