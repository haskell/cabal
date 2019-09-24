{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ComponentName (
  ComponentName(..),
  showComponentName,
  componentNameStanza,
  componentNameString,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.UnqualComponentName
import Distribution.Types.LibraryName
import Distribution.Pretty
import Distribution.Parsec

import qualified Text.PrettyPrint as Disp
import qualified Distribution.Compat.CharParsing as P

-- Libraries live in a separate namespace, so must distinguish
data ComponentName = CLibName   LibraryName
                   | CFLibName  UnqualComponentName
                   | CExeName   UnqualComponentName
                   | CTestName  UnqualComponentName
                   | CBenchName UnqualComponentName
                   deriving (Eq, Generic, Ord, Read, Show, Typeable)

instance Binary ComponentName
instance Structured ComponentName

-- Build-target-ish syntax
instance Pretty ComponentName where
    pretty (CLibName lib)    = prettyLibraryNameComponent lib
    pretty (CFLibName str)   = Disp.text "flib:" <<>> pretty str
    pretty (CExeName str)    = Disp.text "exe:" <<>> pretty str
    pretty (CTestName str)   = Disp.text "test:" <<>> pretty str
    pretty (CBenchName str)  = Disp.text "bench:" <<>> pretty str

instance Parsec ComponentName where
    -- note: this works as lib/flib/... all start with different character!
    parsec = parseComposite <|> parseLib
      where
        parseLib = CLibName <$> parsecLibraryNameComponent
        parseComposite = do
            ctor <- P.choice
                [ P.string "flib:" >> return CFLibName
                , P.string "exe:" >> return CExeName
                , P.string "bench:" >> return CBenchName
                , P.string "test:" >> return CTestName
                ]
            ctor <$> parsec

showComponentName :: ComponentName -> String
showComponentName (CLibName lib)    = showLibraryName lib
showComponentName (CFLibName  name) = "foreign library '" ++ prettyShow name ++ "'"
showComponentName (CExeName   name) = "executable '" ++ prettyShow name ++ "'"
showComponentName (CTestName  name) = "test suite '" ++ prettyShow name ++ "'"
showComponentName (CBenchName name) = "benchmark '" ++ prettyShow name ++ "'"

componentNameStanza :: ComponentName -> String
componentNameStanza (CLibName lib)    = libraryNameStanza lib
componentNameStanza (CFLibName  name) = "foreign-library " ++ prettyShow name
componentNameStanza (CExeName   name) = "executable " ++ prettyShow name
componentNameStanza (CTestName  name) = "test-suite " ++ prettyShow name
componentNameStanza (CBenchName name) = "benchmark " ++ prettyShow name

-- | This gets the underlying unqualified component name. In fact, it is
-- guaranteed to uniquely identify a component, returning
-- @Nothing@ if the 'ComponentName' was for the public
-- library.
componentNameString :: ComponentName -> Maybe UnqualComponentName
componentNameString (CLibName lib) = libraryNameString lib
componentNameString (CFLibName  n) = Just n
componentNameString (CExeName   n) = Just n
componentNameString (CTestName  n) = Just n
componentNameString (CBenchName n) = Just n
