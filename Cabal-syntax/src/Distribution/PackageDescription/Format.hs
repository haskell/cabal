{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.PackageDescription.Format where

import Data.Functor.Identity
import Distribution.CabalSpecVersion (CabalSpecVersion, cabalSpecLatest)
import Distribution.FieldGrammar
import Distribution.FieldGrammar.Format
import Distribution.Fields.Field
import Distribution.PackageDescription.FieldGrammar
import Distribution.Parsec.Position
import Distribution.Types.LibraryName (LibraryName (LMainLibName))

import qualified Data.Map as M
import Debug.Trace

-- | A naive implementation that formats some fields and sections of a package description
exampleFormatPackageDescription
  :: CabalSpecVersion
  -> [Field (WithComments Position)]
  -> [Field (WithComments Position)]
exampleFormatPackageDescription spec = map go
  where
    formatToplevelField :: Field (WithComments Position) -> Field (WithComments Position)
    formatToplevelField = formatFieldGrammar spec packageDescriptionFieldGrammar

    formatLibrary :: LibraryName -> Field (WithComments Position) -> Field (WithComments Position)
    formatLibrary libName = formatFieldGrammar spec (libraryFieldGrammar libName)

    formatCommonStanza :: Field (WithComments Position) -> Field (WithComments Position)
    formatCommonStanza = formatFieldGrammar spec buildInfoFieldGrammar

    go field@(Field{}) = formatToplevelField field
    go (Section sname sargs fs)
      | getName sname == "library" && null sargs = Section sname sargs (map (formatLibrary LMainLibName) fs)
      | getName sname == "common" = Section sname sargs (map formatCommonStanza fs)
      -- Do nothing for now.
      | otherwise = Section sname sargs fs
