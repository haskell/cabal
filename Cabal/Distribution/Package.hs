{-# LANGUAGE DeriveDataTypeable #-}
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

        -- * Installed package identifiers
        InstalledPackageId(..),

        -- * Package source dependencies
        Dependency(..),
        thisPackageVersion,
        notThisPackageVersion,
        simplifyDependency,

        -- * Package classes
        Package(..), packageName, packageVersion,
        PackageFixedDeps(..),
  ) where

import Distribution.Version
         ( Version(..), VersionRange, anyVersion, thisVersion
         , notThisVersion, simplifyVersionRange )

import Distribution.Text (Text(..))
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((<++))
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>), (<+>), text)
import Control.DeepSeq (NFData(..))
import qualified Data.Char as Char ( isDigit, isAlphaNum )
import Data.List ( intercalate )
import Data.Data ( Data )
import Data.Typeable ( Typeable )

newtype PackageName = PackageName String
    deriving (Read, Show, Eq, Ord, Typeable, Data)

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
     deriving (Read, Show, Eq, Ord, Typeable, Data)

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
-- * Installed Package Ids
-- ------------------------------------------------------------

-- | An InstalledPackageId uniquely identifies an instance of an installed
-- package.  There can be at most one package with a given 'InstalledPackageId'
-- in a package database, or overlay of databases.
--
newtype InstalledPackageId = InstalledPackageId String
 deriving (Read,Show,Eq,Ord,Typeable,Data)

instance Text InstalledPackageId where
  disp (InstalledPackageId str) = text str

  parse = InstalledPackageId `fmap` Parse.munch1 abi_char
   where abi_char c = Char.isAlphaNum c || c `elem` ":-_."

-- ------------------------------------------------------------
-- * Package source dependencies
-- ------------------------------------------------------------

-- | Describes a dependency on a source package (API)
--
data Dependency = Dependency PackageName VersionRange
                  deriving (Read, Show, Eq, Typeable, Data)

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

-- | Subclass of packages that have specific versioned dependencies.
--
-- So for example a not-yet-configured package has dependencies on version
-- ranges, not specific versions. A configured or an already installed package
-- depends on exact versions. Some operations or data structures (like
--  dependency graphs) only make sense on this subclass of package types.
--
class Package pkg => PackageFixedDeps pkg where
  depends :: pkg -> [PackageIdentifier]
