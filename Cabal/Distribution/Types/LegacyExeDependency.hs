{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.LegacyExeDependency
  ( LegacyExeDependency(..)
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version ( VersionRange, anyVersion )

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
import Distribution.Text

import Text.PrettyPrint ((<+>), text)

import Distribution.ParseUtils (parseMaybeQuoted)

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
instance NFData LegacyExeDependency where rnf = genericRnf

instance Text LegacyExeDependency where
  disp (LegacyExeDependency name ver) =
    text name <+> disp ver

  parse = do name <- parseMaybeQuoted parseBuildToolName
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return $ LegacyExeDependency name ver
    where
      -- like parsePackageName but accepts symbols in components
      parseBuildToolName :: Parse.ReadP r String
      parseBuildToolName = do ns <- sepBy1 component (Parse.char '-')
                              return (intercalate "-" ns)
        where component = do
                cs <- munch1 (\c -> isAlphaNum c || c == '+' || c == '_')
                if all isDigit cs then pfail else return cs
