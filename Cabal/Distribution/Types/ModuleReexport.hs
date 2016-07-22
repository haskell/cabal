{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ModuleReexport (
    ModuleReexport(..)
) where

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.Binary
import Distribution.Package
import Distribution.ModuleName
import Distribution.Text

import Data.Data                  (Data)
import Data.Typeable               ( Typeable )
import GHC.Generics                (Generic)
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

instance Text ModuleReexport where
    disp (ModuleReexport mpkgname origname newname) =
          maybe Disp.empty (\pkgname -> disp pkgname <> Disp.char ':') mpkgname
       <> disp origname
      <+> if newname == origname
            then Disp.empty
            else Disp.text "as" <+> disp newname

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
