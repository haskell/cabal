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
        PackageName, unPackageName, mkPackageName,
        PackageIdentifier(..),
        PackageId,

        -- * Package keys/installed package IDs (used for linker symbols)
        ComponentId, unComponentId, mkComponentId,
        UnitId(..),
        mkUnitId,
        newSimpleUnitId,
        mkLegacyUnitId,
        getHSLibraryName,
        InstalledPackageId, -- backwards compat

        -- * Modules
        Module(..),

        -- * ABI hash
        AbiHash, unAbiHash, mkAbiHash,

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

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import Distribution.Version
         ( Version, VersionRange, anyVersion, thisVersion
         , notThisVersion, simplifyVersionRange
         , nullVersion )

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Compat.ReadP
import Distribution.Text
import Distribution.ModuleName

import Text.PrettyPrint ((<+>), text)

-- | A package name.
--
-- Use 'mkPackageName' and 'unPackageName' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0
newtype PackageName = PackageName ShortText
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Convert 'PackageName' to 'String'
unPackageName :: PackageName -> String
unPackageName (PackageName s) = fromShortText s

-- | Construct a 'PackageName' from a 'String'
--
-- 'mkPackageName' is the inverse to 'unPackageName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 2.0
mkPackageName :: String -> PackageName
mkPackageName = PackageName . toShortText

instance Binary PackageName

instance Text PackageName where
  disp = Disp.text . unPackageName
  parse = do
    ns <- Parse.sepBy1 component (Parse.char '-')
    return (mkPackageName (intercalate "-" ns))
    where
      component = do
        cs <- Parse.munch1 isAlphaNum
        if all isDigit cs then Parse.pfail else return cs
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
  disp (PackageIdentifier n v)
    | v == nullVersion = disp n -- if no version, don't show version.
    | otherwise        = disp n <<>> Disp.char '-' <<>> disp v

  parse = do
    n <- parse
    v <- (Parse.char '-' >> parse) <++ return nullVersion
    return (PackageIdentifier n v)

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version

-- | A module identity uniquely identifies a Haskell module by
-- qualifying a 'ModuleName' with the 'UnitId' which defined
-- it.  This type distinguishes between two packages
-- which provide a module with the same name, or a module
-- from the same package compiled with different dependencies.
-- There are a few cases where Cabal needs to know about
-- module identities, e.g., when writing out reexported modules in
-- the 'InstalledPackageInfo'.
data Module =
      Module UnitId ModuleName
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary Module

instance Text Module where
    disp (Module uid mod_name) =
        disp uid <<>> Disp.text ":" <<>> disp mod_name
    parse = do
        uid <- parse
        _ <- Parse.char ':'
        mod_name <- parse
        return (Module uid mod_name)

instance NFData Module where
    rnf (Module uid mod_name) = rnf uid `seq` rnf mod_name

-- | A 'ComponentId' uniquely identifies the transitive source
-- code closure of a component (i.e. libraries, executables).
--
-- For non-Backpack components, this corresponds one to one with
-- the 'UnitId', which serves as the basis for install paths,
-- linker symbols, etc.
--
-- Use 'mkComponentId' and 'unComponentId' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0
newtype ComponentId = ComponentId ShortText
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Construct a 'ComponentId' from a 'String'
--
-- 'mkComponentId' is the inverse to 'unComponentId'
--
-- Note: No validations are performed to ensure that the resulting
-- 'ComponentId' is valid
--
-- @since 2.0
mkComponentId :: String -> ComponentId
mkComponentId = ComponentId . toShortText

-- | Convert 'ComponentId' to 'String'
--
-- @since 2.0
unComponentId :: ComponentId -> String
unComponentId (ComponentId s) = fromShortText s

{-# DEPRECATED InstalledPackageId "Use UnitId instead" #-}
type InstalledPackageId = UnitId

instance Binary ComponentId

instance Text ComponentId where
  disp = text . unComponentId

  parse = mkComponentId `fmap` Parse.munch1 abi_char
   where abi_char c = isAlphaNum c || c `elem` "-_."

instance NFData ComponentId where
    rnf = rnf . unComponentId

-- | Returns library name prefixed with HS, suitable for filenames
getHSLibraryName :: UnitId -> String
getHSLibraryName uid = "HS" ++ display uid

-- | A unit identifier identifies a (possibly instantiated)
-- package/component that can be installed the installed package
-- database.  There are several types of components that can be
-- installed:
--
--  * A traditional library with no holes, so that 'unitIdHash'
--    is @Nothing@.  In the absence of Backpack, 'UnitId'
--    is the same as a 'ComponentId'.
--
--  * An indefinite, Backpack library with holes.  In this case,
--    'unitIdHash' is still @Nothing@, but in the install,
--    there are only interfaces, no compiled objects.
--
--  * An instantiated Backpack library with all the holes
--    filled in.  'unitIdHash' is a @Just@ a hash of the
--    instantiating mapping.
--
-- A unit is a component plus the additional information on how the
-- holes are filled in. Thus there is a one to many relationship: for a
-- particular component there are many different ways of filling in the
-- holes, and each different combination is a unit (and has a separate
-- 'UnitId').
--
-- 'UnitId' is distinct from 'IndefUnitId', in that it is always
-- installed, whereas 'IndefUnitId' are intermediate unit identities
-- that arise during mixin linking, and don't necessarily correspond
-- to any actually installed unit.  Since the mapping is not actually
-- recorded in a 'UnitId', you can't actually substitute over them
-- (but you can substitute over 'IndefUnitId').  See also
-- "Distribution.Backpack.FullUnitId" for a mechanism for expanding an
-- instantiated 'UnitId' to retrieve its mapping.
--
data UnitId
    = UnitId {
        unitIdComponentId :: ComponentId,
        unitIdHash        :: Maybe String
    }
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary UnitId

instance NFData UnitId where
    rnf (UnitId cid str) = rnf cid `seq` rnf str

instance Text UnitId where
    disp (UnitId cid Nothing)     = disp cid
    disp (UnitId cid (Just hash)) = disp cid <<>> text "+" <<>> text hash
    parse = parseUnitId <++ parseSimpleUnitId
      where
        parseUnitId = do cid <- parse
                         _ <- Parse.char '+'
                         hash <- Parse.munch1 isAlphaNum
                         return (UnitId cid (Just hash))
        parseSimpleUnitId = fmap newSimpleUnitId parse

-- | Create a unit identity with no associated hash directly
-- from a 'ComponentId'.
newSimpleUnitId :: ComponentId -> UnitId
newSimpleUnitId cid =
    UnitId {
        unitIdComponentId = cid,
        unitIdHash = Nothing
    }

-- | Makes a simple-style UnitId from a string.
mkUnitId :: String -> UnitId
mkUnitId = newSimpleUnitId . mkComponentId

-- | Make an old-style UnitId from a package identifier
mkLegacyUnitId :: PackageId -> UnitId
mkLegacyUnitId = newSimpleUnitId . mkComponentId . display

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

instance NFData Dependency where rnf = genericRnf

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

-- | Packages that have an installed unit ID
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

-- | ABI Hashes
--
-- Use 'mkAbiHash' and 'unAbiHash' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0
newtype AbiHash = AbiHash ShortText
    deriving (Eq, Show, Read, Generic)

-- | Construct a 'AbiHash' from a 'String'
--
-- 'mkAbiHash' is the inverse to 'unAbiHash'
--
-- Note: No validations are performed to ensure that the resulting
-- 'AbiHash' is valid
--
-- @since 2.0
unAbiHash :: AbiHash -> String
unAbiHash (AbiHash h) = fromShortText h

-- | Convert 'AbiHash' to 'String'
--
-- @since 2.0
mkAbiHash :: String -> AbiHash
mkAbiHash = AbiHash . toShortText

instance Binary AbiHash

instance Text AbiHash where
    disp = Disp.text . unAbiHash
    parse = fmap mkAbiHash (Parse.munch isAlphaNum)
