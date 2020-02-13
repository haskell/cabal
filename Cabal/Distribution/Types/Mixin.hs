{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.Mixin (
    Mixin(..),
) where

import Distribution.Compat.Prelude
import Prelude ()

import Text.PrettyPrint ((<+>))

import Distribution.FieldGrammar.Described
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.IncludeRenaming
import Distribution.Types.PackageName

import qualified Distribution.Compat.CharParsing as P

data Mixin = Mixin { mixinPackageName :: PackageName
                   , mixinIncludeRenaming :: IncludeRenaming }
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance Binary Mixin
instance Structured Mixin

instance NFData Mixin where rnf = genericRnf

instance Pretty Mixin where
    pretty (Mixin pkg_name incl) = pretty pkg_name <+> pretty incl

instance Parsec Mixin where
    parsec = do
        mod_name <- parsec
        P.spaces
        incl <- parsec
        return (Mixin mod_name incl)

instance Described Mixin where
    describe _ = RETodo
