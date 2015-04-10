{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ComponentName (
  ComponentName(..),
  defaultLibName,
  showComponentName,
  componentNameString,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP   ((<++))
import Distribution.Package
import Distribution.Text

import Text.PrettyPrint as Disp

-- Libraries live in a separate namespace, so must distinguish
data ComponentName = CLibName
                   | CSubLibName String
                   | CFLibName  String
                   | CExeName   String
                   | CTestName  String
                   | CBenchName String
                   deriving (Eq, Generic, Ord, Read, Show)

instance Binary ComponentName

-- Build-target-ish syntax
instance Text ComponentName where
    disp CLibName = Disp.text "lib"
    disp (CSubLibName str) = Disp.text ("lib:" ++ str)
    disp (CFLibName str)   = Disp.text ("flib:" ++ str)
    disp (CExeName str)    = Disp.text ("exe:" ++ str)
    disp (CTestName str)   = Disp.text ("test:" ++ str)
    disp (CBenchName str)  = Disp.text ("bench:" ++ str)

    parse = parseComposite <++ parseSingle
     where
      parseSingle = Parse.string "lib" >> return CLibName
      parseComposite = do
        ctor <- Parse.choice [ Parse.string "lib:" >> return CSubLibName
                             , Parse.string "flib:" >> return CFLibName
                             , Parse.string "exe:" >> return CExeName
                             , Parse.string "bench:" >> return CBenchName
                             , Parse.string "test:" >> return CTestName ]
        -- For now, component names coincide with package name syntax
        -- (since they can show up in build-depends, which are parsed
        -- as package names.)
        fmap (ctor . unPackageName) parse

defaultLibName :: ComponentName
defaultLibName = CLibName

showComponentName :: ComponentName -> String
showComponentName CLibName          = "library"
showComponentName (CSubLibName name) = "library '" ++ name ++ "'"
showComponentName (CFLibName  name) = "foreign library '" ++ name ++ "'"
showComponentName (CExeName   name) = "executable '" ++ name ++ "'"
showComponentName (CTestName  name) = "test suite '" ++ name ++ "'"
showComponentName (CBenchName name) = "benchmark '" ++ name ++ "'"

-- | This gets the 'String' component name. In fact, it is
-- guaranteed to uniquely identify a component, returning
-- @Nothing@ if the 'ComponentName' was for the public
-- library.
componentNameString :: ComponentName -> Maybe String
componentNameString CLibName = Nothing
componentNameString (CSubLibName n) = Just n
componentNameString (CFLibName  n) = Just n
componentNameString (CExeName   n) = Just n
componentNameString (CTestName  n) = Just n
componentNameString (CBenchName n) = Just n
