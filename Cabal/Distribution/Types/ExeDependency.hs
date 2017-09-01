{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.ExeDependency
  ( ExeDependency(..)
  , qualifiedExeName
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.ComponentName
import Distribution.Types.UnqualComponentName
import Distribution.Types.PackageName
import Distribution.Version ( VersionRange, anyVersion )

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
import Distribution.Pretty
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

instance Pretty ExeDependency where
  pretty (ExeDependency name exe ver) =
    (pretty name <<>> text ":" <<>> pretty exe) <+> pretty ver

instance Text ExeDependency where
  parse = do name <- parse
             _ <- Parse.char ':'
             exe <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return (ExeDependency name exe ver)

qualifiedExeName :: ExeDependency -> ComponentName
qualifiedExeName (ExeDependency _ ucn _) = CExeName ucn
