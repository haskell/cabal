{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.Mixin (
    Mixin(..),
) where

import Distribution.Compat.Prelude
import Prelude ()

import Text.PrettyPrint ((<+>), colon)
import Distribution.Compat.ReadP
import Distribution.Text

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Types.IncludeRenaming
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.ReadP  as Parse

data Mixin = Mixin { mixinPackageName :: PackageName
                   , mixinLibraryName :: Maybe UnqualComponentName
                   , mixinIncludeRenaming :: IncludeRenaming }
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance Binary Mixin

instance NFData Mixin where rnf = genericRnf

instance Pretty Mixin where
    pretty (Mixin pkg_name Nothing incl) =
        pretty pkg_name <+> pretty incl
    pretty (Mixin pkg_name (Just lib_name) incl) =
        pretty pkg_name <<>> colon <<>> pretty lib_name <+> pretty incl

instance Parsec Mixin where
    parsec = do
        mod_name <- parsec
        mb_lib_name <- P.option Nothing $ do
            _ <- P.char ':'
            fmap Just parsec
        P.spaces
        incl <- parsec
        return (Mixin mod_name mb_lib_name incl)

instance Text Mixin where
    parse = do
        pkg_name <- parse
        Parse.skipSpaces
        mb_lib_name <- option Nothing $ do
            _ <- char ':'
            fmap Just parse
        Parse.skipSpaces
        incl <- parse
        return (Mixin pkg_name mb_lib_name incl)
