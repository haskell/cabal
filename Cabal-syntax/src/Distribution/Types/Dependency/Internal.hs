{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Dependency.Internal
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

import Distribution.Types.VersionRange (isAnyVersionLight)
import Distribution.Version (VersionRange, anyVersion, simplifyVersionRange)

import Distribution.CabalSpecVersion
import Distribution.Compat.CharParsing (char, spaces)
import Distribution.Compat.Parsing (between, option)
import Distribution.Pretty
import Distribution.Types.LibraryName.Internal
import Distribution.Types.PackageName.Internal
import Distribution.Types.UnqualComponentName.Internal

import qualified Distribution.Compat.NonEmptySet as NES
import qualified Text.PrettyPrint as PP

-- | Describes a dependency on a source package (API)
--
-- /Invariant:/ package name does not appear as 'LSubLibName' in
-- set of library names.
data Dependency
  = -- | The set of libraries required from the package.
    -- Only the selected libraries will be built.
    -- It does not affect the cabal-install solver yet.
    Dependency
      PackageName
      VersionRange
      (NonEmptySet LibraryName)
  deriving (Generic, Read, Show, Eq, Ord, Data)

depPkgName :: Dependency -> PackageName
depPkgName (Dependency pn _ _) = pn

depVerRange :: Dependency -> VersionRange
depVerRange (Dependency _ vr _) = vr

depLibraries :: Dependency -> NonEmptySet LibraryName
depLibraries (Dependency _ _ cs) = cs

-- | Smart constructor of 'Dependency'.
--
-- If 'PackageName' is appears as 'LSubLibName' in a set of sublibraries,
-- it is automatically converted to 'LMainLibName'.
--
-- @since 3.4.0.0
mkDependency :: PackageName -> VersionRange -> NonEmptySet LibraryName -> Dependency
mkDependency pn vr lb = Dependency pn vr (NES.map conv lb)
  where
    pn' = packageNameToUnqualComponentName pn

    conv l@LMainLibName = l
    conv l@(LSubLibName ln)
      | ln == pn' = LMainLibName
      | otherwise = l

instance Binary Dependency
instance Structured Dependency
instance NFData Dependency where rnf = genericRnf

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


-- | Library set with main library.
--
-- @since 3.4.0.0
mainLibSet :: NonEmptySet LibraryName
mainLibSet = NES.singleton LMainLibName

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range comps) =
  Dependency name (simplifyVersionRange range) comps
