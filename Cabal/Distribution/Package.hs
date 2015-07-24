{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

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

        -- * Package keys (used for linker symbols)
        PackageKey(..),
        mkPackageKey,
        packageKeyHash,
        packageKeyLibraryName,

        -- * Library name (used for install path, package key)
        LibraryName(..),
        emptyLibraryName,
        getHSLibraryName,

        -- * Package source dependencies
        Dependency(..),
        thisPackageVersion,
        notThisPackageVersion,
        simplifyDependency,

        -- * Package classes
        Package(..), packageName, packageVersion,
        HasInstalledPackageId(..),
        PackageInstalled(..),
  ) where

import Distribution.Version
         ( Version(..), VersionRange, anyVersion, thisVersion
         , notThisVersion, simplifyVersionRange )

import Distribution.Text (Text(..), display)
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((<++))
import qualified Text.PrettyPrint as Disp

import Control.DeepSeq (NFData(..))
import Distribution.Compat.Binary (Binary)
import qualified Data.Char as Char
    ( isDigit, isAlphaNum, isUpper, isLower, ord, chr )
import Data.Data ( Data )
import Data.List ( intercalate, foldl', sort )
import Data.Typeable ( Typeable )
import Data.Word ( Word64 )
import GHC.Fingerprint ( Fingerprint(..), fingerprintString )
import GHC.Generics (Generic)
import Numeric ( showIntAtBase )
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
-- * Installed Package Ids
-- ------------------------------------------------------------

-- | An InstalledPackageId uniquely identifies an instance of an installed
-- package.  There can be at most one package with a given 'InstalledPackageId'
-- in a package database, or overlay of databases.
--
newtype InstalledPackageId = InstalledPackageId String
 deriving (Generic, Read,Show,Eq,Ord,Typeable,Data)

instance Binary InstalledPackageId

instance Text InstalledPackageId where
  disp (InstalledPackageId str) = text str

  parse = InstalledPackageId `fmap` Parse.munch1 abi_char
   where abi_char c = Char.isAlphaNum c || c `elem` "-_."

-- ------------------------------------------------------------
-- * Package Keys
-- ------------------------------------------------------------

-- | A 'PackageKey' is the notion of "package ID" which is visible to the
-- compiler. Why is this not a 'PackageId'? The 'PackageId' is a user-visible
-- concept written explicity in Cabal files; on the other hand, a 'PackageKey'
-- may contain, for example, information about the transitive dependency
-- tree of a package.  Why is this not an 'InstalledPackageId'?  A 'PackageKey'
-- should be stable so that we can incrementally recompile after a source edit;
-- however, an 'InstalledPackageId' may change even with source.
--
-- Package keys may be generated either by Cabal or GHC.  In particular,
-- ordinary, "old-style" packages which don't use Backpack features can
-- have their package keys generated directly by Cabal and coincide with
-- 'LibraryName's.  However, Backpack keys are generated by GHC may exhibit
-- more variation than a 'LibraryName'.
--
data PackageKey
    -- | Modern package key which is a hash of the PackageId and the transitive
    -- dependency key.  It's manually inlined here so we can get the instances
    -- we need.  There's an optional prefix for compatibility with GHC 7.10.
    = PackageKey (Maybe String) {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
    -- | Old-style package key which is just a 'PackageId'.  Required because
    -- old versions of GHC assume that the 'sourcePackageId' recorded for an
    -- installed package coincides with the package key it was compiled with.
    | OldPackageKey !PackageId
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageKey

-- | Convenience function which converts a fingerprint into a new-style package
-- key.
fingerprintPackageKey :: Fingerprint -> PackageKey
fingerprintPackageKey (Fingerprint a b) = PackageKey Nothing a b

-- | Generates a 'PackageKey' from a 'PackageId', sorted package keys of the
-- immediate dependencies.
mkPackageKey :: Bool -- are modern style package keys supported?
             -> PackageId
             -> [LibraryName] -- dependencies
             -> PackageKey
mkPackageKey True pid deps =
    fingerprintPackageKey . fingerprintString $
        display pid ++ "\n" ++
        concat [ display dep ++ "\n" | dep <- sort deps ]
mkPackageKey False pid _ = OldPackageKey pid

-- The base-62 code is based off of 'locators'
-- ((c) Operational Dynamics Consulting, BSD3 licensed)

-- Note: Instead of base-62 encoding a single 128-bit integer
-- (ceil(21.49) characters), we'll base-62 a pair of 64-bit integers
-- (2 * ceil(10.75) characters).  Luckily for us, it's the same number of
-- characters!  In the long term, this should go in GHC.Fingerprint,
-- but not now...

-- | Size of a 64-bit word when written as a base-62 string
word64Base62Len :: Int
word64Base62Len = 11

-- | Converts a 64-bit word into a base-62 string
toBase62 :: Word64 -> String
toBase62 w = pad ++ str
  where
    pad = replicate len '0'
    len = word64Base62Len - length str -- 11 == ceil(64 / lg 62)
    str = showIntAtBase 62 represent w ""
    represent :: Int -> Char
    represent x
        | x < 10 = Char.chr (48 + x)
        | x < 36 = Char.chr (65 + x - 10)
        | x < 62 = Char.chr (97 + x - 36)
        | otherwise = error ("represent (base 62): impossible!")

-- | Parses a base-62 string into a 64-bit word
fromBase62 :: String -> Word64
fromBase62 ss = foldl' multiply 0 ss
  where
    value :: Char -> Int
    value c
        | Char.isDigit c = Char.ord c - 48
        | Char.isUpper c = Char.ord c - 65 + 10
        | Char.isLower c = Char.ord c - 97 + 36
        | otherwise = error ("value (base 62): impossible!")

    multiply :: Word64 -> Char -> Word64
    multiply acc c = acc * 62 + (fromIntegral $ value c)

-- | Parses a base-62 string into a fingerprint.
readBase62Fingerprint :: String -> Fingerprint
readBase62Fingerprint s = Fingerprint w1 w2
 where (s1,s2) = splitAt word64Base62Len s
       w1 = fromBase62 s1
       w2 = fromBase62 (take word64Base62Len s2)

-- | Compute the hash (without a prefix) of a package key.  In GHC 7.12
-- this is equivalent to display.
packageKeyHash :: PackageKey -> String
packageKeyHash (PackageKey _ w1 w2) = toBase62 w1 ++ toBase62 w2
packageKeyHash (OldPackageKey pid) = display pid

-- | Legacy function for GHC 7.10 to compute a LibraryName based on
-- the package key.
packageKeyLibraryName :: PackageId -> PackageKey -> LibraryName
packageKeyLibraryName pid (PackageKey _ w1 w2) =
  LibraryName (display pid ++ "-" ++ toBase62 w1 ++ toBase62 w2)
packageKeyLibraryName _ (OldPackageKey pid) = LibraryName (display pid)

instance Text PackageKey where
  disp (PackageKey mb_prefix w1 w2)
    = maybe Disp.empty (\r -> Disp.text r <> Disp.char '_') mb_prefix <>
      Disp.text (toBase62 w1) <> Disp.text (toBase62 w2)
  disp (OldPackageKey pid) = disp pid

  parse = parseNewWithAnnot <++ parseNew <++ parseOld
    where parseNew = do
            fmap (fingerprintPackageKey . readBase62Fingerprint)
                . Parse.count (word64Base62Len * 2)
                $ Parse.satisfy Char.isAlphaNum
          parseNewWithAnnot = do
            -- this is ignored
            prefix <- Parse.munch1 (\c -> Char.isAlphaNum c || c `elem` "-")
            _ <- Parse.char '_' -- if we use '-' it's ambiguous
            PackageKey _ w1 w2 <- parseNew
            return (PackageKey (Just prefix) w1 w2)
          parseOld = do pid <- parse
                        return (OldPackageKey pid)

instance NFData PackageKey where
    rnf (PackageKey mb _ _) = rnf mb
    rnf (OldPackageKey pid) = rnf pid

-- ------------------------------------------------------------
-- * Library names
-- ------------------------------------------------------------

-- | A library name consists of not only a source package
-- id ('PackageId') but also the library names of all textual
-- dependencies; thus, a library name uniquely identifies an
-- installed package up to the dependency resolution done by Cabal.
-- Create using 'packageKeyLibraryName'.  Library names are opaque,
-- Cabal-defined strings.
newtype LibraryName
    = LibraryName String
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary LibraryName

-- | Default library name for when it is not known.
emptyLibraryName :: LibraryName
emptyLibraryName = LibraryName ""

-- | Returns library name prefixed with HS, suitable for filenames
getHSLibraryName :: LibraryName -> String
getHSLibraryName (LibraryName s) = "HS" ++ s

instance Text LibraryName where
    disp (LibraryName s) = Disp.text s
    parse = LibraryName `fmap` Parse.munch1 hash_char
        where hash_char c = Char.isAlphaNum c || c `elem` "-_."

instance NFData LibraryName where
    rnf (LibraryName s) = rnf s

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
class Package pkg => HasInstalledPackageId pkg where
  installedPackageId :: pkg -> InstalledPackageId

-- | Class of installed packages.
--
-- The primary data type which is an instance of this package is
-- 'InstalledPackageInfo', but when we are doing install plans in Cabal install
-- we may have other, installed package-like things which contain more metadata.
-- Installed packages have exact dependencies 'installedDepends'.
class HasInstalledPackageId pkg => PackageInstalled pkg where
  installedDepends :: pkg -> [InstalledPackageId]
