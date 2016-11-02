-- | *Dependency Text instances moved from Distribution.Package
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Package.TextClass () where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.ParseUtils
import Distribution.Version (anyVersion)

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Compat.ReadP
import Distribution.Text

import Text.PrettyPrint ((<+>))


instance Text Dependency where
  disp (Dependency name ver) =
    disp name <+> disp ver

  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return (Dependency name ver)

instance Text LegacyExeDependency where
  disp (LegacyExeDependency name ver) =
    Disp.text name <+> disp ver

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

instance Text PkgconfigDependency where
  disp (PkgconfigDependency name ver) =
    disp name <+> disp ver

  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return $ PkgconfigDependency name ver
