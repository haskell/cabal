{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint(..)
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Text
import Distribution.Pretty
import Text.PrettyPrint ((<+>))

import Distribution.Types.VersionRange
import Distribution.Types.PackageName


-- | A version constraint on a package. Different from 'ExeDependency' and
-- 'Dependency' since it does not specify the need for a component, not even
-- the main library.
-- There are a few places in the codebase where 'Dependency' is used where
-- 'PackageVersionConstraint' should be used instead (#5570).
data PackageVersionConstraint = PackageVersionConstraint PackageName VersionRange
                  deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary PackageVersionConstraint
instance NFData PackageVersionConstraint where rnf = genericRnf

instance Pretty PackageVersionConstraint where
  pretty (PackageVersionConstraint name ver) = pretty name <+> pretty ver

instance Text PackageVersionConstraint where
  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse Parse.<++ return anyVersion
             Parse.skipSpaces
             return (PackageVersionConstraint name ver)

