{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ComponentName (
  ComponentName(..),
  libraryComponentName,
  showComponentName,
  componentNameStanza,
  componentNameString,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP   ((<++))
import Distribution.Types.UnqualComponentName
import Distribution.Types.LibraryName
import Distribution.Pretty
import Distribution.Text

import Text.PrettyPrint as Disp

-- Libraries live in a separate namespace, so must distinguish
data ComponentName = CLibName   LibraryName
                   | CFLibName  UnqualComponentName
                   | CExeName   UnqualComponentName
                   | CTestName  UnqualComponentName
                   | CBenchName UnqualComponentName
                   deriving (Eq, Generic, Ord, Read, Show, Typeable)

instance Binary ComponentName

-- Build-target-ish syntax
instance Pretty ComponentName where
    pretty (CLibName lib)    = pretty lib
    pretty (CFLibName str)   = Disp.text "flib:" <<>> pretty str
    pretty (CExeName str)    = Disp.text "exe:" <<>> pretty str
    pretty (CTestName str)   = Disp.text "test:" <<>> pretty str
    pretty (CBenchName str)  = Disp.text "bench:" <<>> pretty str

instance Text ComponentName where
    parse = parseComposite <++ parseLib
     where
      parseLib = CLibName <$> parse
      parseComposite = do
        ctor <- Parse.choice [ Parse.string "flib:" >> return CFLibName
                             , Parse.string "exe:" >> return CExeName
                             , Parse.string "bench:" >> return CBenchName
                             , Parse.string "test:" >> return CTestName ]
        ctor <$> parse

showComponentName :: ComponentName -> String
showComponentName (CLibName lib)    = showLibraryName lib
showComponentName (CFLibName  name) = "foreign library '" ++ display name ++ "'"
showComponentName (CExeName   name) = "executable '" ++ display name ++ "'"
showComponentName (CTestName  name) = "test suite '" ++ display name ++ "'"
showComponentName (CBenchName name) = "benchmark '" ++ display name ++ "'"

componentNameStanza :: ComponentName -> String
componentNameStanza (CLibName lib)    = libraryNameStanza lib
componentNameStanza (CFLibName  name) = "foreign-library " ++ display name
componentNameStanza (CExeName   name) = "executable " ++ display name
componentNameStanza (CTestName  name) = "test-suite " ++ display name
componentNameStanza (CBenchName name) = "benchmark " ++ display name

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

-- | Convert the 'UnqualComponentName' of a library into a
-- 'ComponentName'.
libraryComponentName :: Maybe UnqualComponentName -> ComponentName
libraryComponentName Nothing = CLibName LMainLibName
libraryComponentName (Just n) = CLibName $ LSubLibName n
