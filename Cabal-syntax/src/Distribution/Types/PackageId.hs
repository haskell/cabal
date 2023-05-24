{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.PackageId
  ( PackageIdentifier (..)
  , PackageId
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec (Parsec (..), simpleParsec)
import Distribution.Pretty
import Distribution.Types.PackageName
import Distribution.Version (Version, nullVersion)

import qualified Data.List.NonEmpty as NE
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | Type alias so we can use the shorter name PackageId.
type PackageId = PackageIdentifier

-- | The name and version of a package.
data PackageIdentifier = PackageIdentifier
  { pkgName :: PackageName
  -- ^ The name of this package, eg. foo
  , pkgVersion :: Version
  -- ^ the version of this package, eg 1.2
  }
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageIdentifier
instance Structured PackageIdentifier

instance Pretty PackageIdentifier where
  pretty (PackageIdentifier n v)
    | v == nullVersion = pretty n -- if no version, don't show version.
    | otherwise = pretty n <<>> Disp.char '-' <<>> pretty v

-- |
--
-- >>> simpleParsec "foo-bar-0" :: Maybe PackageIdentifier
-- Just (PackageIdentifier {pkgName = PackageName "foo-bar", pkgVersion = mkVersion [0]})
--
-- >>> simpleParsec "foo-bar" :: Maybe PackageIdentifier
-- Just (PackageIdentifier {pkgName = PackageName "foo-bar", pkgVersion = mkVersion []})
--
-- /Note:/ Stricter than 'Text' instance
--
-- >>> simpleParsec "foo-bar-0-0" :: Maybe PackageIdentifier
-- Nothing
--
-- >>> simpleParsec "foo-bar.0" :: Maybe PackageIdentifier
-- Nothing
--
-- >>> simpleParsec "foo-bar.4-2" :: Maybe PackageIdentifier
-- Nothing
--
-- >>> simpleParsec "1.2.3" :: Maybe PackageIdentifier
-- Nothing
instance Parsec PackageIdentifier where
  parsec = do
    xs' <- P.sepByNonEmpty component (P.char '-')
    (v, xs) <- case simpleParsec (NE.last xs') of
      Nothing -> return (nullVersion, toList xs') -- all components are version
      Just v -> return (v, NE.init xs')
    if not (null xs) && all (\c -> all (/= '.') c && not (all isDigit c)) xs
      then return $ PackageIdentifier (mkPackageName (intercalate "-" xs)) v
      else fail "all digits or a dot in a portion of package name"
    where
      component = P.munch1 (\c -> isAlphaNum c || c == '.')

instance NFData PackageIdentifier where
  rnf (PackageIdentifier name version) = rnf name `seq` rnf version
