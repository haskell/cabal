{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Mixin (
    Mixin(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Text.PrettyPrint ((<+>), colon)
import Distribution.Compat.ReadP
import Distribution.Text

import Distribution.Package
import Distribution.Types.IncludeRenaming
import Distribution.Types.UnqualComponentName

data Mixin = Mixin { mixinPackageName :: PackageName
                   , mixinLibraryName :: Maybe UnqualComponentName
                   , mixinIncludeRenaming :: IncludeRenaming }
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance Binary Mixin

instance Text Mixin where
    disp (Mixin pkg_name Nothing incl) =
        disp pkg_name <+> disp incl
    disp (Mixin pkg_name (Just lib_name) incl) =
        disp pkg_name <<>> colon <<>> disp lib_name <+> disp incl

    parse = do
        pkg_name <- parse
        mb_lib_name <- option Nothing $ do
            _ <- char ':'
            fmap Just parse
        skipSpaces
        incl <- parse
        return (Mixin pkg_name mb_lib_name incl)
