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

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP   ((<++))
import Distribution.Types.UnqualComponentName
import Distribution.Pretty
import Distribution.Text

import Text.PrettyPrint as Disp

data LibraryName = LMainLibName
                 | LSubLibName UnqualComponentName
                 deriving (Eq, Generic, Ord, Read, Show, Typeable, Data)

instance Binary LibraryName
instance NFData LibraryName where rnf = genericRnf

-- Build-target-ish syntax
instance Pretty LibraryName where
    pretty LMainLibName = Disp.text "lib"
    pretty (LSubLibName str) = Disp.text "lib:" <<>> pretty str

instance Text LibraryName where
    parse = parseComposite <++ parseSingle
     where
      parseSingle = Parse.string "lib" >> return LMainLibName
      parseComposite = do
        ctor <- Parse.string "lib:" >> return LSubLibName
        ctor <$> parse

defaultLibName :: LibraryName
defaultLibName = LMainLibName

showLibraryName :: LibraryName -> String
showLibraryName LMainLibName          = "library"
showLibraryName (LSubLibName name) = "library '" ++ display name ++ "'"

libraryNameStanza :: LibraryName -> String
libraryNameStanza LMainLibName          = "library"
libraryNameStanza (LSubLibName name) = "library " ++ display name

libraryNameString :: LibraryName -> Maybe UnqualComponentName
libraryNameString LMainLibName = Nothing
libraryNameString (LSubLibName n) = Just n

-- | Convert the 'UnqualComponentName' of a library into a
-- 'LibraryName'.
maybeToLibraryName :: Maybe UnqualComponentName -> LibraryName
maybeToLibraryName Nothing = LMainLibName
maybeToLibraryName (Just n) = LSubLibName n

