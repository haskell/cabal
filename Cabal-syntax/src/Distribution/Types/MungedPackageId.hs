{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.MungedPackageId
  ( MungedPackageId (..)
  , computeCompatPackageId
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.LibraryName
import Distribution.Types.MungedPackageName
import Distribution.Types.PackageId
import Distribution.Version (Version, nullVersion)

import qualified Text.PrettyPrint as Disp

-- | A simple pair of a 'MungedPackageName' and 'Version'. 'MungedPackageName' is to
-- 'MungedPackageId' as 'PackageName' is to 'PackageId'. See 'MungedPackageName' for more
-- info.
data MungedPackageId = MungedPackageId
  { mungedName :: MungedPackageName
  -- ^ The combined package and component name. see documentation for
  -- 'MungedPackageName'.
  , mungedVersion :: Version
  -- ^ The version of this package / component, eg 1.2
  }
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary MungedPackageId
instance Structured MungedPackageId

-- |
--
-- >>> prettyShow $ MungedPackageId (MungedPackageName "servant" LMainLibName) (mkVersion [1,2,3])
-- "servant-1.2.3"
--
-- >>> prettyShow $ MungedPackageId (MungedPackageName "servant" (LSubLibName "lackey")) (mkVersion [0,1,2])
-- "z-servant-z-lackey-0.1.2"
instance Pretty MungedPackageId where
  pretty (MungedPackageId n v)
    | v == nullVersion = pretty n -- if no version, don't show version.
    | otherwise = pretty n <<>> Disp.char '-' <<>> pretty v

-- |
--
-- >>> simpleParsec "foo-bar-0" :: Maybe MungedPackageId
-- Just (MungedPackageId {mungedName = MungedPackageName (PackageName "foo-bar") LMainLibName, mungedVersion = mkVersion [0]})
--
-- >>> simpleParsec "foo-bar" :: Maybe MungedPackageId
-- Just (MungedPackageId {mungedName = MungedPackageName (PackageName "foo-bar") LMainLibName, mungedVersion = mkVersion []})
--
-- >>> simpleParsec "z-foo-bar-z-baz-0" :: Maybe MungedPackageId
-- Just (MungedPackageId {mungedName = MungedPackageName (PackageName "foo-bar") (LSubLibName (UnqualComponentName "baz")), mungedVersion = mkVersion [0]})
--
-- >>> simpleParsec "foo-bar-0-0" :: Maybe MungedPackageId
-- Nothing
--
-- >>> simpleParsec "foo-bar.0" :: Maybe MungedPackageId
-- Nothing
--
-- >>> simpleParsec "foo-bar.4-2" :: Maybe MungedPackageId
-- Nothing
instance Parsec MungedPackageId where
  parsec = do
    PackageIdentifier pn v <- parsec
    return $ MungedPackageId (decodeCompatPackageName pn) v

instance NFData MungedPackageId where
  rnf (MungedPackageId name version) = rnf name `seq` rnf version

computeCompatPackageId :: PackageId -> LibraryName -> MungedPackageId
computeCompatPackageId (PackageIdentifier pn vr) ln =
  MungedPackageId (MungedPackageName pn ln) vr

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> import Distribution.Types.Version
