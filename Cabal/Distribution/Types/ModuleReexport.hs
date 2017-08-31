{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ModuleReexport (
    ModuleReexport(..)
) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Distribution.Compat.ReadP as Parse
import Distribution.ModuleName
import Distribution.Pretty
import Distribution.Text
import Distribution.Types.PackageName

import Text.PrettyPrint as Disp

-- -----------------------------------------------------------------------------
-- Module re-exports

data ModuleReexport = ModuleReexport {
       moduleReexportOriginalPackage :: Maybe PackageName,
       moduleReexportOriginalName    :: ModuleName,
       moduleReexportName            :: ModuleName
    }
    deriving (Eq, Generic, Read, Show, Typeable, Data)

instance Binary ModuleReexport

instance Pretty ModuleReexport where
    pretty (ModuleReexport mpkgname origname newname) =
          maybe Disp.empty (\pkgname -> pretty pkgname <<>> Disp.char ':') mpkgname
       <<>> pretty origname
      <+> if newname == origname
            then Disp.empty
            else Disp.text "as" <+> pretty newname

instance Text ModuleReexport where
    parse = do
      mpkgname <- Parse.option Nothing $ do
                    pkgname <- parse
                    _       <- Parse.char ':'
                    return (Just pkgname)
      origname <- parse
      newname  <- Parse.option origname $ do
                    Parse.skipSpaces
                    _ <- Parse.string "as"
                    Parse.skipSpaces
                    parse
      return (ModuleReexport mpkgname origname newname)
