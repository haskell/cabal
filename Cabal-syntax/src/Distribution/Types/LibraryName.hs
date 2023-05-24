{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.LibraryName
  ( LibraryName (..)
  , defaultLibName
  , maybeToLibraryName
  , showLibraryName
  , libraryNameStanza
  , libraryNameString

    -- * Pretty & Parse
  , prettyLibraryNameComponent
  , parsecLibraryNameComponent
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.UnqualComponentName

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

data LibraryName
  = LMainLibName
  | LSubLibName UnqualComponentName
  deriving (Eq, Generic, Ord, Read, Show, Typeable, Data)

instance Binary LibraryName
instance Structured LibraryName
instance NFData LibraryName where rnf = genericRnf

-- | Pretty print 'LibraryName' in build-target-ish syntax.
--
-- /Note:/ there are no 'Pretty' or 'Parsec' instances,
-- as there's other way to represent 'LibraryName', namely as bare
-- 'UnqualComponentName'.
prettyLibraryNameComponent :: LibraryName -> Disp.Doc
prettyLibraryNameComponent LMainLibName = Disp.text "lib"
prettyLibraryNameComponent (LSubLibName str) = Disp.text "lib:" <<>> pretty str

parsecLibraryNameComponent :: CabalParsing m => m LibraryName
parsecLibraryNameComponent = do
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
showLibraryName LMainLibName = "library"
showLibraryName (LSubLibName name) = "library '" ++ prettyShow name ++ "'"

libraryNameStanza :: LibraryName -> String
libraryNameStanza LMainLibName = "library"
libraryNameStanza (LSubLibName name) = "library " ++ prettyShow name

libraryNameString :: LibraryName -> Maybe UnqualComponentName
libraryNameString LMainLibName = Nothing
libraryNameString (LSubLibName n) = Just n

-- | Convert the 'UnqualComponentName' of a library into a
-- 'LibraryName'.
maybeToLibraryName :: Maybe UnqualComponentName -> LibraryName
maybeToLibraryName Nothing = LMainLibName
maybeToLibraryName (Just n) = LSubLibName n
