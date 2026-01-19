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
  , prettyLibraryNames
  , prettierLibraryNames
  , prettyLibraryNameComponent
  , parsecLibraryNameComponent
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.UnqualComponentName

import Distribution.Types.Annotation

import qualified Data.List.NonEmpty as NEL
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

data LibraryName
  = LMainLibName
  | LSubLibName UnqualComponentName
  deriving (Eq, Generic, Ord, Read, Show, Data)


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

-- | Pretty print a 'LibraryName' after a package name.
--
-- Produces output like @foo@, @foo:bar@, or @foo:{bar,baz}@
prettyLibraryNames :: Pretty a => a -> NonEmpty LibraryName -> Disp.Doc
prettyLibraryNames = prettierLibraryNames mempty
-- ^ backwards compat

prettierLibraryNames :: Pretty a => TriviaTree -> a -> NonEmpty LibraryName -> Disp.Doc
prettierLibraryNames t package libraries =
  let doc = pretty package

      prettyComponent LMainLibName = pretty package
      prettyComponent (LSubLibName component) = Disp.text $ unUnqualComponentName component

      prettyComponents = commaSep $ prettyComponent <$> NEL.toList libraries
   in case libraries of
        LMainLibName :| [] -> doc
        LSubLibName component :| [] -> doc <<>> Disp.colon <<>> pretty component
        _ -> doc <<>> Disp.colon <<>> Disp.braces prettyComponents

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
