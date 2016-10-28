{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Mixin (
    Mixin(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Text.PrettyPrint ((<+>))
import Distribution.Compat.ReadP
import Distribution.Text

import Distribution.Package
import Distribution.Types.IncludeRenaming

data Mixin = Mixin { mixinPackageName :: PackageName
                   , mixinIncludeRenaming :: IncludeRenaming }
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance Binary Mixin

instance Text Mixin where
    disp (Mixin pkg_name incl) =
        disp pkg_name <+> disp incl

    parse = do
        pkg_name <- parse
        skipSpaces
        incl <- parse
        return (Mixin pkg_name incl)
