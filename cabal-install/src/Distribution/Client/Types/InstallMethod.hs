{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.InstallMethod where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

data InstallMethod
  = InstallMethodCopy
  | InstallMethodSymlink
  deriving (Eq, Show, Generic, Bounded, Enum)

instance Binary InstallMethod
instance Structured InstallMethod

-- | Last
instance Semigroup InstallMethod where
  _ <> x = x

instance Parsec InstallMethod where
  parsec = do
    name <- P.munch1 isAlpha
    case name of
      "copy" -> pure InstallMethodCopy
      "symlink" -> pure InstallMethodSymlink
      _ -> P.unexpected $ "InstallMethod: " ++ name

instance Pretty InstallMethod where
  pretty InstallMethodCopy = PP.text "copy"
  pretty InstallMethodSymlink = PP.text "symlink"
