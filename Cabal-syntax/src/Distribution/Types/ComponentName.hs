{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Distribution.Types.ComponentName
  ( ComponentName (.., CFLibName, CExeName, CTestName, CBenchName)
  , showComponentName
  , componentNameRaw
  , componentNameStanza
  , componentNameString
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.LibraryName
import Distribution.Types.UnqualComponentName

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- Libraries live in a separate namespace, so must distinguish
data ComponentName
  = CLibName LibraryName
  | CNotLibName NotLibComponentName
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

data NotLibComponentName
  = CNLFLibName {toCompName :: UnqualComponentName}
  | CNLExeName {toCompName :: UnqualComponentName}
  | CNLTestName {toCompName :: UnqualComponentName}
  | CNLBenchName {toCompName :: UnqualComponentName}
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

pattern CFLibName :: UnqualComponentName -> ComponentName
pattern CFLibName n = CNotLibName (CNLFLibName n)

pattern CExeName :: UnqualComponentName -> ComponentName
pattern CExeName n = CNotLibName (CNLExeName n)

pattern CTestName :: UnqualComponentName -> ComponentName
pattern CTestName n = CNotLibName (CNLTestName n)

pattern CBenchName :: UnqualComponentName -> ComponentName
pattern CBenchName n = CNotLibName (CNLBenchName n)
{-# COMPLETE CLibName, CFLibName, CExeName, CTestName, CBenchName #-}

instance Binary NotLibComponentName
instance Structured NotLibComponentName

instance Binary ComponentName
instance Structured ComponentName

-- Build-target-ish syntax
instance Pretty ComponentName where
  pretty (CLibName lib) = prettyLibraryNameComponent lib
  pretty (CFLibName str) = Disp.text "flib:" <<>> pretty str
  pretty (CExeName str) = Disp.text "exe:" <<>> pretty str
  pretty (CTestName str) = Disp.text "test:" <<>> pretty str
  pretty (CBenchName str) = Disp.text "bench:" <<>> pretty str

instance Parsec ComponentName where
  -- note: this works as lib/flib/... all start with different character!
  parsec = parseComposite <|> parseLib
    where
      parseLib = CLibName <$> parsecLibraryNameComponent
      parseComposite = do
        ctor <-
          P.choice
            [ P.string "flib:" >> return CFLibName
            , P.string "exe:" >> return CExeName
            , P.string "bench:" >> return CBenchName
            , P.string "test:" >> return CTestName
            ]
        ctor <$> parsec

showComponentName :: ComponentName -> String
showComponentName (CLibName lib) = showLibraryName lib
showComponentName (CFLibName name) = "foreign library '" ++ prettyShow name ++ "'"
showComponentName (CExeName name) = "executable '" ++ prettyShow name ++ "'"
showComponentName (CTestName name) = "test suite '" ++ prettyShow name ++ "'"
showComponentName (CBenchName name) = "benchmark '" ++ prettyShow name ++ "'"

componentNameRaw :: ComponentName -> String
componentNameRaw l@(CLibName _) = showComponentName l
componentNameRaw (CNotLibName x) = prettyShow $ toCompName x

componentNameStanza :: ComponentName -> String
componentNameStanza (CLibName lib) = libraryNameStanza lib
componentNameStanza (CFLibName name) = "foreign-library " ++ prettyShow name
componentNameStanza (CExeName name) = "executable " ++ prettyShow name
componentNameStanza (CTestName name) = "test-suite " ++ prettyShow name
componentNameStanza (CBenchName name) = "benchmark " ++ prettyShow name

-- | This gets the underlying unqualified component name. In fact, it is
-- guaranteed to uniquely identify a component, returning
-- @Nothing@ if the 'ComponentName' was for the public
-- library.
componentNameString :: ComponentName -> Maybe UnqualComponentName
componentNameString (CLibName lib) = libraryNameString lib
componentNameString (CNotLibName x) = Just $ toCompName x
