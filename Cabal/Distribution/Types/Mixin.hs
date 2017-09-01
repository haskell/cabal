{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Mixin (
    Mixin(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Text.PrettyPrint ((<+>))
import Distribution.Compat.ReadP
import Distribution.Pretty
import Distribution.Text

import Distribution.Types.PackageName
import Distribution.Types.IncludeRenaming

data Mixin = Mixin { mixinPackageName :: PackageName
                   , mixinIncludeRenaming :: IncludeRenaming }
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance Binary Mixin

instance Pretty Mixin where
    pretty (Mixin pkg_name incl) = pretty pkg_name <+> pretty incl

instance Text Mixin where
    parse = do
        pkg_name <- parse
        skipSpaces
        incl <- parse
        return (Mixin pkg_name incl)
