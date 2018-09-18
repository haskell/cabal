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

import Distribution.CabalSpecVersion
import Distribution.Text
import Distribution.Pretty
import qualified Text.PrettyPrint as PP
import Distribution.Parsec.Class
import Distribution.Compat.CharParsing (char, spaces)
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
data Dependency = Dependency
                    PackageName
                    VersionRange
                    (Set LibraryName)
                    -- ^ The set of libraries required from the package.
                    -- Only the selected libraries will be built.
                    -- It does not affect the cabal-install solver yet.
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
                                       <+> optionalMonoid
                                             (sublibs /= Set.singleton LMainLibName)
                                             (PP.colon <+> PP.braces prettySublibs)
                                       <+> pretty ver
      where
        optionalMonoid True x = x
        optionalMonoid False _ = mempty
        prettySublibs = PP.hsep $ PP.punctuate PP.comma $ prettySublib <$> Set.toList sublibs
        prettySublib LMainLibName = PP.text $ unPackageName name
        prettySublib (LSubLibName un) = PP.text $ unUnqualComponentName un

versionGuardMultilibs :: (Monad m, CabalParsing m) => m a -> m a
versionGuardMultilibs expr = do
  csv <- askCabalSpecVersion
  if csv < CabalSpecV3_0
  then fail $ unwords
    [ "Sublibrary dependency syntax used."
    , "To use this syntax the package needs to specify at least 'cabal-version: 3.0'."
    , "Alternatively, if you are depending on an internal library, you can write"
    , "directly the library name as it were a package."
    ]
  else
    expr

instance Parsec Dependency where
    parsec = do
        name <- lexemeParsec

        libs <- option [LMainLibName]
              $ (char ':' *> spaces *>)
              $ versionGuardMultilibs
              $ between (char '{' *> spaces) (spaces <* char '}')
              $ parsecCommaList (makeLib name <$> parsecUnqualComponentName)
        ver  <- parsec <|> pure anyVersion
        return $ Dependency name ver $ Set.fromList libs
      where makeLib pn ln | unPackageName pn == ln = LMainLibName
                          | otherwise = LSubLibName $ mkUnqualComponentName ln

instance Text Dependency where
  parse = do name <- parse
             Parse.skipSpaces
             libs <- option [LMainLibName]
                   $ (char ':' *>)
                   $ versionGuardMultilibs
                   $ between (char '{') (char '}')
                   $ parsecCommaList (makeLib name <$> parsecUnqualComponentName)
             Parse.skipSpaces
             ver <- parse Parse.<++ return anyVersion
             Parse.skipSpaces
             return $ Dependency name ver $ Set.fromList libs
    where makeLib pn ln | unPackageName pn == ln = LMainLibName
                        | otherwise = LSubLibName $ mkUnqualComponentName ln

-- mempty should never be in a Dependency-as-dependency.
-- This is only here until the Dependency-as-constraint problem is solved #5570.
-- Same for below.
thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) =
  Dependency n (thisVersion v) Set.empty

notThisPackageVersion :: PackageIdentifier -> Dependency
notThisPackageVersion (PackageIdentifier n v) =
  Dependency n (notThisVersion v) Set.empty

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range comps) =
  Dependency name (simplifyVersionRange range) comps
