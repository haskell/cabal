{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.ExeDependency
  ( ExeDependency(..)
  , qualifiedExeName
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.Types.ComponentName
import Distribution.Types.UnqualComponentName
import Distribution.Version ( VersionRange, anyVersion )

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
import Distribution.Text

import Text.PrettyPrint ((<+>), text)

-- | Describes a dependency on an executable from a package
--
data ExeDependency = ExeDependency
                     PackageName
                     UnqualComponentName -- name of executable component of package
                     VersionRange
                     deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary ExeDependency
instance NFData ExeDependency where rnf = genericRnf

instance Text ExeDependency where
  disp (ExeDependency name exe ver) =
    (disp name <<>> text ":" <<>> disp exe) <+> disp ver

  parse = do name <- parse
             _ <- Parse.char ':'
             exe <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return (ExeDependency name exe ver)

qualifiedExeName :: ExeDependency -> ComponentName
qualifiedExeName (ExeDependency _ ucn _) = CExeName ucn
