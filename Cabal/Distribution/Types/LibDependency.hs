{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Types.LibDependency
   ( LibDependency(..)
   , thisPackageVersion
   , notThisPackageVersion
   , libDependencyToDependency
   , simplifyLibDependency
   ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Types.Dependency
import Distribution.Version ( VersionRange, anyVersion
                            , simplifyVersionRange )

import qualified Distribution.Compat.CharParsing as P
import           Distribution.Compat.ReadP
import           Text.PrettyPrint as PP ((<+>), text, empty)

-- | Like 'Dependency', but this corresponds exactly to the syntax we support in
-- a Cabal file.
data LibDependency = LibDependency {
    libDepPackageName :: PackageName,
    libDepLibraryName :: Maybe UnqualComponentName,
    libDepVersionRange :: VersionRange
  }
  deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary LibDependency
instance NFData LibDependency where rnf = genericRnf

instance Pretty LibDependency where
  pretty (LibDependency name mCname ver) =
    (pretty name <<>> prettyMaybeCname) <+> pretty ver
    where
      prettyMaybeCname = case mCname of
        Nothing    -> PP.empty
        Just cname -> text ":" <<>> pretty cname

instance Parsec LibDependency where
    parsec = do
        name <- parsec
        mb_cname <- P.option Nothing $ do
            _ <- P.char ':'
            fmap Just parsec
        P.spaces
        ver <- parsec <|> pure anyVersion
        return (LibDependency name mb_cname ver)

instance Text LibDependency where
  parse = do name <- parse
             mb_cname <- option Nothing $ do
               _ <- char ':'
               fmap Just parse
             skipSpaces
             ver <- parse <++ return anyVersion
             return (LibDependency name mb_cname ver)

libDependencyToDependency :: LibDependency -> Dependency
libDependencyToDependency (LibDependency pn _ vr) = Dependency pn vr


-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyLibDependency :: LibDependency -> LibDependency
simplifyLibDependency (LibDependency name mb_cname range) =
  LibDependency name mb_cname (simplifyVersionRange range)
