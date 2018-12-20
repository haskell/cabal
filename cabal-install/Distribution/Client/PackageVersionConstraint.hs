{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Client.PackageVersionConstraint
  ( PackageVersionConstraint(..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.PackageName
import Distribution.Types.VersionRange

import qualified Distribution.Compat.CharParsing as P
import           Text.PrettyPrint                ((<+>))

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

instance Parsec PackageVersionConstraint where
  parsec = do
      name <- parsec
      P.spaces
      ver <- parsec <|> return anyVersion
      P.spaces
      return (PackageVersionConstraint name ver)

