{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.PkgconfigName
  ( PkgconfigName, unPkgconfigName, mkPkgconfigName
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import qualified Text.PrettyPrint as Disp
import Distribution.Compat.ReadP
import Distribution.Text

-- | A pkg-config library name
--
-- This is parsed as any valid argument to the pkg-config utility.
--
-- @since 2.0
newtype PkgconfigName = PkgconfigName ShortText
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Convert 'PkgconfigName' to 'String'
--
-- @since 2.0
unPkgconfigName :: PkgconfigName -> String
unPkgconfigName (PkgconfigName s) = fromShortText s

-- | Construct a 'PkgconfigName' from a 'String'
--
-- 'mkPkgconfigName' is the inverse to 'unPkgconfigName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PkgconfigName' is valid
--
-- @since 2.0
mkPkgconfigName :: String -> PkgconfigName
mkPkgconfigName = PkgconfigName . toShortText

-- | 'mkPkgconfigName'
--
-- @since 2.0
instance IsString PkgconfigName where
    fromString = mkPkgconfigName

instance Binary PkgconfigName

-- pkg-config allows versions and other letters in package names, eg
-- "gtk+-2.0" is a valid pkg-config package _name_.  It then has a package
-- version number like 2.10.13
instance Text PkgconfigName where
  disp = Disp.text . unPkgconfigName
  parse = mkPkgconfigName
          <$> munch1 (\c -> isAlphaNum c || c `elem` "+-._")

instance NFData PkgconfigName where
    rnf (PkgconfigName pkg) = rnf pkg
