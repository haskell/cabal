{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.LegacyExeDependency
  ( LegacyExeDependency(..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.FieldGrammar.Described
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Version                (VersionRange, anyVersion)

import qualified Distribution.Compat.CharParsing as P
import           Text.PrettyPrint                (text, (<+>))

-- | Describes a legacy `build-tools`-style dependency on an executable
--
-- It is "legacy" because we do not know what the build-tool referred to. It
-- could refer to a pkg-config executable (PkgconfigName), or an internal
-- executable (UnqualComponentName). Thus the name is stringly typed.
--
-- @since 2.0.0.2
data LegacyExeDependency = LegacyExeDependency
                           String
                           VersionRange
                         deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary LegacyExeDependency
instance Structured LegacyExeDependency
instance NFData LegacyExeDependency where rnf = genericRnf

instance Pretty LegacyExeDependency where
  pretty (LegacyExeDependency name ver) =
    text name <+> pretty ver

instance Parsec LegacyExeDependency where
    parsec = do
        name <- parsecMaybeQuoted nameP
        P.spaces
        verRange <- parsecMaybeQuoted parsec <|> pure anyVersion
        pure $ LegacyExeDependency name verRange
      where
        nameP = intercalate "-" <$> toList <$> P.sepByNonEmpty component (P.char '-')
        component = do
            cs <- P.munch1 (\c -> isAlphaNum c || c == '+' || c == '_')
            if all isDigit cs then fail "invalid component" else return cs

instance Described LegacyExeDependency where
    describe _ = RETodo
