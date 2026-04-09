{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Distribution.Types.PackageName
  ( PackageName
  , PackageNameAnn
  , PackageNameWith (..)
  , unannotatePackageName
  , unPackageName
  , mkPackageName
  , unPackageNameST
  , mkPackageNameST
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Trivia
import qualified Text.PrettyPrint as Disp

import Data.Kind
import Distribution.Types.Modify (Annotate)
import qualified Distribution.Types.Modify as Mod

-- | A package name.
--
-- Use 'mkPackageName' and 'unPackageName' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
type PackageName = PackageNameWith Mod.HasNoAnn
type PackageNameAnn = PackageNameWith Mod.HasAnn

newtype PackageNameWith (m :: Mod.HasAnnotation) = PackageName (Annotate m ShortText)
  deriving (Generic)

deriving instance Show PackageName
deriving instance Read PackageName
deriving instance Eq PackageName
deriving instance Ord PackageName
deriving instance Data PackageName

deriving instance Show PackageNameAnn
deriving instance Read PackageNameAnn
deriving instance Eq PackageNameAnn
deriving instance Ord PackageNameAnn
deriving instance Data PackageNameAnn

unannotatePackageName :: PackageNameWith Mod.HasAnn -> PackageName
unannotatePackageName (PackageName pname) = PackageName (unAnn pname)

-- | Convert 'PackageName' to 'String'
unPackageName :: PackageName -> String
unPackageName (PackageName s) = fromShortText s

-- | @since 3.4.0.0
unPackageNameST :: PackageName -> ShortText
unPackageNameST (PackageName s) = s

-- | Construct a 'PackageName' from a 'String'
--
-- 'mkPackageName' is the inverse to 'unPackageName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 2.0.0.2
mkPackageName :: String -> PackageName
mkPackageName = PackageName . toShortText

-- | Construct a 'PackageName' from a 'ShortText'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 3.4.0.0
mkPackageNameST :: ShortText -> PackageName
mkPackageNameST = PackageName

-- | 'mkPackageName'
--
-- @since 2.0.0.2
instance IsString PackageName where
  fromString = mkPackageName

instance Binary PackageName
instance Structured PackageName

instance Pretty PackageName where
  pretty = Disp.text . unPackageName

instance Pretty PackageNameAnn where
  pretty (PackageName (Ann t x)) = applyTriviaDoc t $ Disp.text $ fromShortText x

instance Parsec PackageName where
  parsec = mkPackageName <$> parsecUnqualComponentName

instance Parsec (PackageNameWith Mod.HasAnn) where
  parsec = PackageName . Ann mempty . toShortText <$> parsecUnqualComponentName

instance NFData PackageName where
  rnf (PackageName pkg) = rnf pkg
