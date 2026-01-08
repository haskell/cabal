{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Dependency.Pretty
  ( Dependency (..)
  , mkDependency
  , depPkgName
  , depVerRange
  , depLibraries
  , simplifyDependency
  , mainLibSet
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Dependency.Internal
import Distribution.Types.VersionRange (isAnyVersionLight)
import Distribution.Version (VersionRange, anyVersion, simplifyVersionRange)

import Distribution.CabalSpecVersion
import Distribution.Compat.CharParsing (char, spaces)
import Distribution.Compat.Parsing (between, option)
import Distribution.Pretty
import Distribution.Types.LibraryName.Internal
import Distribution.Types.LibraryName.Pretty
import Distribution.Types.PackageName.Internal
import Distribution.Types.PackageName.Pretty
import Distribution.Types.VersionRange.Pretty
import Distribution.Types.UnqualComponentName.Internal

import qualified Distribution.Compat.NonEmptySet as NES
import qualified Text.PrettyPrint as PP

-- |
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion mainLibSet
-- "pkg"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.insert (LSubLibName $ mkUnqualComponentName "sublib") mainLibSet
-- "pkg:{pkg,sublib}"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.singleton (LSubLibName $ mkUnqualComponentName "sublib")
-- "pkg:sublib"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.insert (LSubLibName $ mkUnqualComponentName "sublib-b") $ NES.singleton (LSubLibName $ mkUnqualComponentName "sublib-a")
-- "pkg:{sublib-a,sublib-b}"
instance Pretty Dependency where
  pretty (Dependency name ver sublibs) = prettyLibraryNames name (NES.toNonEmpty sublibs) <+> pver
    where
      -- TODO: change to isAnyVersion after #6736
      pver
        | isAnyVersionLight ver = PP.empty
        | otherwise = pretty ver
