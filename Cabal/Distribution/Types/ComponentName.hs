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
                   | CSubLibName UnqualComponentName
                   | CExeName    UnqualComponentName
                   | CTestName   UnqualComponentName
                   | CBenchName  UnqualComponentName
                   deriving (Eq, Generic, Ord, Read, Show)

instance Binary ComponentName

-- Build-target-ish syntax
instance Text ComponentName where
    disp CLibName = Disp.text "lib"
    disp (CSubLibName str) = Disp.text "lib:" <<>> disp str
    disp (CExeName str) = Disp.text "exe:" <<>> disp str
    disp (CTestName str) = Disp.text "test:" <<>> disp str
    disp (CBenchName str) = Disp.text "bench:" <<>> disp str

    parse = parseComposite <++ parseSingle
     where
      parseSingle = Parse.string "lib" >> return CLibName
      parseComposite = do
        ctor <- Parse.choice [ Parse.string "lib:" >> return CSubLibName
                             , Parse.string "exe:" >> return CExeName
                             , Parse.string "bench:" >> return CBenchName
                             , Parse.string "test:" >> return CTestName ]
        ctor <$> parse

defaultLibName :: ComponentName
defaultLibName = CLibName

showComponentName :: ComponentName -> String
showComponentName CLibName          = "library"
showComponentName (CSubLibName name) = "library '" ++ unUnqualComponentName name ++ "'"
showComponentName (CExeName   name) = "executable '" ++ unUnqualComponentName name ++ "'"
showComponentName (CTestName  name) = "test suite '" ++ unUnqualComponentName name ++ "'"
showComponentName (CBenchName name) = "benchmark '" ++ unUnqualComponentName name ++ "'"

-- | This gets the underlying unqualified component name. In fact, it is
-- guaranteed to uniquely identify a component, returning
-- @Nothing@ if the 'ComponentName' was for the public
-- library.
componentNameString :: ComponentName -> Maybe UnqualComponentName
componentNameString CLibName = Nothing
componentNameString (CSubLibName n) = Just n
componentNameString (CExeName   n) = Just n
componentNameString (CTestName  n) = Just n
componentNameString (CBenchName n) = Just n
