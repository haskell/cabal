module Distribution.Types.LibraryName.Parsec where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Types.LibraryName

import Distribution.Types.UnqualComponentName
import Distribution.Types.UnqualComponentName.Parsec

import qualified Distribution.Compat.CharParsing as P

parsecLibraryNameComponent :: CabalParsing m => m LibraryName
parsecLibraryNameComponent = do
  _ <- P.string "lib"
  parseComposite <|> parseSingle
  where
    parseSingle = return LMainLibName
    parseComposite = do
      _ <- P.char ':'
      LSubLibName <$> parsec
