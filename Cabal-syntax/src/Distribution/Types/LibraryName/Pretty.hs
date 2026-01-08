{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.LibraryName.Pretty
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
  -- , parsecLibraryNameComponent
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Pretty
import Distribution.Types.LibraryName.Internal
import Distribution.Types.UnqualComponentName.Internal
import Distribution.Types.UnqualComponentName.Pretty

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import qualified Data.List.NonEmpty as NEL
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

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

prettierLibraryNames :: Pretty a => Map Namespace [Trivium] -> a -> NonEmpty LibraryName -> Disp.Doc
prettierLibraryNames t package libraries =
  let doc = pretty package

      prettyComponent LMainLibName = pretty package
      prettyComponent (LSubLibName component) = Disp.text $ unUnqualComponentName component

      prettyComponents = commaSep $ prettyComponent <$> NEL.toList libraries
   in case libraries of
        LMainLibName :| [] -> doc
        LSubLibName component :| [] -> doc <<>> Disp.colon <<>> pretty component
        _ -> doc <<>> Disp.colon <<>> Disp.braces prettyComponents


showLibraryName :: LibraryName -> String
showLibraryName LMainLibName = "library"
showLibraryName (LSubLibName name) = "library '" ++ prettyShow name ++ "'"

libraryNameStanza :: LibraryName -> String
libraryNameStanza LMainLibName = "library"
libraryNameStanza (LSubLibName name) = "library " ++ prettyShow name
