{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.LibraryName (
  LibraryName(..),
  defaultLibName,
  maybeToLibraryName,
  showLibraryName,
  libraryNameStanza,
  libraryNameString,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.UnqualComponentName
import Distribution.Pretty
import Distribution.Parsec

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

data LibraryName = LMainLibName
                 | LSubLibName UnqualComponentName
                 deriving (Eq, Generic, Ord, Read, Show, Typeable, Data)

instance Binary LibraryName
instance NFData LibraryName where rnf = genericRnf

-- Build-target-ish syntax
instance Pretty LibraryName where
    pretty LMainLibName = Disp.text "lib"
    pretty (LSubLibName str) = Disp.text "lib:" <<>> pretty str

instance Parsec LibraryName where
    parsec = do
        _ <- P.string "lib"
        parseComposite <|> parseSingle
      where
        parseSingle = return LMainLibName
        parseComposite = do
            _ <- P.char ':'
            LSubLibName <$> parsec

defaultLibName :: LibraryName
defaultLibName = LMainLibName

showLibraryName :: LibraryName -> String
showLibraryName LMainLibName       = "library"
showLibraryName (LSubLibName name) = "library '" ++ prettyShow name ++ "'"

libraryNameStanza :: LibraryName -> String
libraryNameStanza LMainLibName       = "library"
libraryNameStanza (LSubLibName name) = "library " ++ prettyShow name

libraryNameString :: LibraryName -> Maybe UnqualComponentName
libraryNameString LMainLibName = Nothing
libraryNameString (LSubLibName n) = Just n

-- | Convert the 'UnqualComponentName' of a library into a
-- 'LibraryName'.
maybeToLibraryName :: Maybe UnqualComponentName -> LibraryName
maybeToLibraryName Nothing = LMainLibName
maybeToLibraryName (Just n) = LSubLibName n

