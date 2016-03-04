{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Package
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Defines a package identifier along with a parser and pretty printer for it.
-- 'PackageIdentifier's consist of a name and an exact version. It also defines
-- a 'Dependency' data type. A dependency is a package name and a version
-- range, like @\"foo >= 1.2 && < 2\"@.

module Distribution.Package (
        -- * Package ids
        PackageName(..),
        PackageIdentifier(..),
        PackageId,

        -- * Package keys/installed package IDs (used for linker symbols)
        ComponentId(..),
        UnitId(..),
        mkUnitId,
        mkLegacyUnitId,
        getHSLibraryName,
        InstalledPackageId, -- backwards compat

        -- * ABI hash
        AbiHash(..),

        -- * Package source dependencies
        Dependency(..),
        thisPackageVersion,
        notThisPackageVersion,
        simplifyDependency,

        -- * Package classes
        Package(..), packageName, packageVersion,
        HasUnitId(..),
        installedPackageId,
        PackageInstalled(..),
  ) where

import Distribution.Version
         ( Version(..), VersionRange, anyVersion, thisVersion
         , notThisVersion, simplifyVersionRange )

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Compat.ReadP
import Distribution.Compat.Binary
import Distribution.Text

import Control.DeepSeq (NFData(..))
import qualified Data.Char as Char
    ( isDigit, isAlphaNum, )
import Data.Data ( Data )
import Data.List ( intercalate )
import Data.Typeable ( Typeable )
import GHC.Generics (Generic)
import Text.PrettyPrint ((<>), (<+>), text)

newtype PackageName = PackageName { unPackageName :: String }
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageName

instance Text PackageName where
  disp (PackageName n) = Disp.text n
  parse = do
    ns <- Parse.sepBy1 component (Parse.char '-')
    return (PackageName (intercalate "-" ns))
    where
      component = do
        cs <- Parse.munch1 Char.isAlphaNum
        if all Char.isDigit cs then Parse.pfail else return cs
        -- each component must contain an alphabetic character, to avoid
        -- ambiguity in identifiers like foo-1 (the 1 is the version number).

instance NFData PackageName where
    rnf (PackageName pkg) = rnf pkg

-- | Type alias so we can use the shorter name PackageId.
type PackageId = PackageIdentifier

-- | The name and version of a package.
data PackageIdentifier
    = PackageIdentifier {
        pkgName    :: PackageName, -- ^The name of this package, eg. foo
        pkgVersion :: Version -- ^the version of this package, eg 1.2
     }
     deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageIdentifier

instance Text PackageIdentifier where
  disp (PackageIdentifier n v) = case v of
    Version [] _ -> disp n -- if no version, don't show version.
    _            -> disp n <> Disp.char '-' <> disp v

  parse = do
    n <- parse
    v <- (Parse.char '-' >> parse) <++ return (Version [] [])
    return (PackageIdentifier n v)

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version

-- ------------------------------------------------------------
-- * Component Source Hash
-- ------------------------------------------------------------

-- | A 'ComponentId' uniquely identifies the transitive source
-- code closure of a component.  For non-Backpack components, it also
-- serves as the basis for install paths, symbols, etc.
--
data ComponentId
    = ComponentId String
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

{-# DEPRECATED InstalledPackageId "Use UnitId instead" #-}
type InstalledPackageId = UnitId

instance Binary ComponentId

instance Text ComponentId where
  disp (ComponentId str) = text str

  parse = ComponentId `fmap` Parse.munch1 abi_char
   where abi_char c = Char.isAlphaNum c || c `elem` "-_."

instance NFData ComponentId where
    rnf (ComponentId pk) = rnf pk

-- | Returns library name prefixed with HS, suitable for filenames
getHSLibraryName :: UnitId -> String
getHSLibraryName (SimpleUnitId (ComponentId s)) = "HS" ++ s

-- | For now, there is no distinction between component IDs
-- and unit IDs in Cabal.
newtype UnitId = SimpleUnitId ComponentId
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data, Binary, Text, NFData)

-- | Makes a simple-style UnitId from a string.
mkUnitId :: String -> UnitId
mkUnitId = SimpleUnitId . ComponentId

-- | Make an old-style UnitId from a package identifier
mkLegacyUnitId :: PackageId -> UnitId
mkLegacyUnitId = SimpleUnitId . ComponentId . display

-- ------------------------------------------------------------
-- * Package source dependencies
-- ------------------------------------------------------------

-- | Describes a dependency on a source package (API)
--
data Dependency = Dependency PackageName VersionRange
                  deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary Dependency

instance Text Dependency where
  disp (Dependency name ver) =
    disp name <+> disp ver

  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return (Dependency name ver)

thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) =
  Dependency n (thisVersion v)

notThisPackageVersion :: PackageIdentifier -> Dependency
notThisPackageVersion (PackageIdentifier n v) =
  Dependency n (notThisVersion v)

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range) =
  Dependency name (simplifyVersionRange range)

-- | Class of things that have a 'PackageIdentifier'
--
-- Types in this class are all notions of a package. This allows us to have
-- different types for the different phases that packages go though, from
-- simple name\/id, package description, configured or installed packages.
--
-- Not all kinds of packages can be uniquely identified by a
-- 'PackageIdentifier'. In particular, installed packages cannot, there may be
-- many installed instances of the same source package.
--
class Package pkg where
  packageId :: pkg -> PackageIdentifier

packageName    :: Package pkg => pkg -> PackageName
packageName     = pkgName    . packageId

packageVersion :: Package pkg => pkg -> Version
packageVersion  = pkgVersion . packageId

instance Package PackageIdentifier where
  packageId = id

-- | Packages that have an installed package ID
class Package pkg => HasUnitId pkg where
  installedUnitId :: pkg -> UnitId

{-# DEPRECATED installedPackageId "Use installedUnitId instead" #-}
-- | Compatibility wrapper for Cabal pre-1.24.
installedPackageId :: HasUnitId pkg => pkg -> UnitId
installedPackageId = installedUnitId

-- | Class of installed packages.
--
-- The primary data type which is an instance of this package is
-- 'InstalledPackageInfo', but when we are doing install plans in Cabal install
-- we may have other, installed package-like things which contain more metadata.
-- Installed packages have exact dependencies 'installedDepends'.
class (HasUnitId pkg) => PackageInstalled pkg where
  installedDepends :: pkg -> [UnitId]

-- -----------------------------------------------------------------------------
-- ABI hash

newtype AbiHash = AbiHash String
    deriving (Eq, Show, Read, Generic)
instance Binary AbiHash

instance Text AbiHash where
    disp (AbiHash abi) = Disp.text abi
    parse = fmap AbiHash (Parse.munch Char.isAlphaNum)
