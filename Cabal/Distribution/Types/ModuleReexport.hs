{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.ModuleReexport (
    ModuleReexport(..)
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Parsec
import Distribution.Pretty
import Distribution.FieldGrammar.Described
import Distribution.Types.PackageName

import qualified Distribution.Compat.CharParsing as P
import           Text.PrettyPrint           ((<+>))
import qualified Text.PrettyPrint           as Disp

-- -----------------------------------------------------------------------------
-- Module re-exports

data ModuleReexport = ModuleReexport {
       moduleReexportOriginalPackage :: Maybe PackageName,
       moduleReexportOriginalName    :: ModuleName,
       moduleReexportName            :: ModuleName
    }
    deriving (Eq, Generic, Read, Show, Typeable, Data)

instance Binary ModuleReexport
instance Structured ModuleReexport
instance NFData ModuleReexport where rnf = genericRnf

instance Pretty ModuleReexport where
    pretty (ModuleReexport mpkgname origname newname) =
          maybe Disp.empty (\pkgname -> pretty pkgname <<>> Disp.char ':') mpkgname
       <<>> pretty origname
      <+> if newname == origname
            then Disp.empty
            else Disp.text "as" <+> pretty newname

instance Parsec ModuleReexport where
    parsec = do
        mpkgname <- P.optional (P.try $ parsec <* P.char ':')
        origname <- parsec
        newname  <- P.option origname $ P.try $ do
            P.spaces
            _ <- P.string "as"
            P.spaces
            parsec
        return (ModuleReexport mpkgname origname newname)

instance Described ModuleReexport where
    describe _ = RETodo
