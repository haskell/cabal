{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.Dependency
  ( Dependency(..)
  , depPkgName
  , depVerRange
  , depLibraries
  , thisPackageVersion
  , notThisPackageVersion
  , simplifyDependency
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version ( VersionRange, thisVersion
                            , notThisVersion, anyVersion
                            , simplifyVersionRange )

import qualified Distribution.Compat.ReadP as Parse

import Distribution.Text
import Distribution.Pretty
import qualified Text.PrettyPrint as PP
import Distribution.Parsec.Class
import Distribution.Compat.CharParsing (char)
import Distribution.Compat.Parsing (between, option)
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.LibraryName
import Distribution.Types.UnqualComponentName

import Text.PrettyPrint ((<+>))
import Data.Set (Set)
import qualified Data.Set as Set

-- | Describes a dependency on a source package (API)
--
data Dependency = Dependency PackageName VersionRange (Set LibraryName)
                  deriving (Generic, Read, Show, Eq, Typeable, Data)

depPkgName :: Dependency -> PackageName
depPkgName (Dependency pn _ _) = pn

depVerRange :: Dependency -> VersionRange
depVerRange (Dependency _ vr _) = vr

depLibraries :: Dependency -> Set LibraryName
depLibraries (Dependency _ _ cs) = cs

instance Binary Dependency
instance NFData Dependency where rnf = genericRnf

instance Pretty Dependency where
    pretty (Dependency name ver sublibs) = pretty name
                                       <+> PP.text ":{" <+> prettySublibs <+> PP.text "}"
                                       <+> pretty ver
      where
        prettySublibs = PP.text $ concat $ intersperse "," $ showSublib <$> Set.toList sublibs
        showSublib LMainLibName = unPackageName name
        showSublib (LSubLibName un) = unUnqualComponentName un

instance Parsec Dependency where
    parsec = do
        name <- lexemeParsec
        ver  <- parsec <|> pure anyVersion
        libs <- option [LMainLibName]
              $ (char ':' >>)
              $ between (char '{') (char '}')
              $ parsecCommaList (makeLib name <$> parsecUnqualComponentName)
        return $ Dependency name ver $ Set.fromList libs
      where makeLib pn ln | unPackageName pn == ln = LMainLibName
                          | otherwise = LSubLibName $ mkUnqualComponentName ln

instance Text Dependency where
  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse Parse.<++ return anyVersion
             Parse.skipSpaces
             libs <- option [LMainLibName]
                   $ (char ':' >>)
                   $ between (char '{') (char '}')
                   $ parsecCommaList (makeLib name <$> parsecUnqualComponentName)
             Parse.skipSpaces
             return $ Dependency name ver $ Set.fromList libs
    where makeLib pn ln | unPackageName pn == ln = LMainLibName
                        | otherwise = LSubLibName $ mkUnqualComponentName ln

thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) =
  Dependency n (thisVersion v) Set.empty --TODO what does this do? is it safe to put empty? same for below.

notThisPackageVersion :: PackageIdentifier -> Dependency
notThisPackageVersion (PackageIdentifier n v) =
  Dependency n (notThisVersion v) Set.empty

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range comps) =
  Dependency name (simplifyVersionRange range) comps
