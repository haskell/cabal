{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Distribution.Types.Dependency
  ( Dependency
  , DependencyAnn
  , DependencyWith (..)
  , unannotateDependencyAnn
  , mkDependency
  , depPkgName
  , depVerRange
  , depLibraries
  , simplifyDependency
  , mainLibSet
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Types.VersionRange (isAnyVersionLight, unAnnVersionRange)
import Distribution.Version (VersionRange, VersionRangeAnn, VersionRangeWith (..), anyVersionAnn, simplifyVersionRange)

import Distribution.CabalSpecVersion
import Distribution.Compat.CharParsing (char, spaces, spaces')
import Distribution.Compat.Parsing (between, option)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Trivia
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName

import qualified Distribution.Compat.NonEmptySet as NES
import qualified Text.PrettyPrint as PP

import qualified Distribution.Types.Modify as Mod

-- | Describes a dependency on a source package (API)
--
-- /Invariant:/ package name does not appear as 'LSubLibName' in
-- set of library names.
type Dependency = DependencyWith Mod.HasNoAnn

type DependencyAnn = DependencyWith Mod.HasAnn

data DependencyWith (m :: Mod.HasAnnotation)
  = -- | The set of libraries required from the package.
    -- Only the selected libraries will be built.
    -- It does not affect the cabal-install solver yet.
    Dependency
      (PackageNameWith m)
      (VersionRangeWith m)
      (NonEmptySet LibraryName)
  deriving (Generic)

deriving instance Read Dependency
deriving instance Show Dependency
deriving instance Eq Dependency
deriving instance Ord Dependency
deriving instance Data Dependency

-- TODO: less instances?
deriving instance Read (DependencyWith Mod.HasAnn)
deriving instance Show (DependencyWith Mod.HasAnn)
deriving instance Eq (DependencyWith Mod.HasAnn)
deriving instance Ord (DependencyWith Mod.HasAnn)
deriving instance Data (DependencyWith Mod.HasAnn)

unannotateDependencyAnn :: DependencyAnn -> Dependency
unannotateDependencyAnn (Dependency pname vrange libs) =
  Dependency
    (unannotatePackageName pname)
    (unAnnVersionRange vrange)
    libs

depPkgName :: Dependency -> PackageName
depPkgName (Dependency pn _ _) = pn

depVerRange :: Dependency -> VersionRange
depVerRange (Dependency _ vr _) = vr

depLibraries :: Dependency -> NonEmptySet LibraryName
depLibraries (Dependency _ _ cs) = cs

-- | Smart constructor of 'Dependency'.
--
-- If 'PackageName' is appears as 'LSubLibName' in a set of sublibraries,
-- it is automatically converted to 'LMainLibName'.
--
-- @since 3.4.0.0
mkDependency :: PackageName -> VersionRange -> NonEmptySet LibraryName -> Dependency
mkDependency pn vr lb = Dependency pn vr (NES.map conv lb)
  where
    pn' = packageNameToUnqualComponentName pn
    conv l@LMainLibName = l
    conv l@(LSubLibName ln)
      | ln == pn' = LMainLibName
      | otherwise = l

mkDependencyAnn :: PackageNameAnn -> VersionRangeAnn -> NonEmptySet LibraryName -> DependencyWith Mod.HasAnn
mkDependencyAnn pn vr lb = Dependency pn vr (NES.map conv lb)
  where
    pn' = packageNameToUnqualComponentNameWith pn

    -- TODO(leana8959): lossy?
    conv l@LMainLibName = l
    conv l@(LSubLibName ln)
      | ln == unAnn pn' = LMainLibName
      | otherwise = l

instance Binary Dependency
instance Structured Dependency
instance NFData Dependency where rnf = genericRnf

-- |
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion mainLibSet
-- "pkg"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.insert (LSubLibName $ mkUnqualComponentName "sublib") mainLibSet
-- "pkg:{pkg,sublib}"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.singleton (LSubLibName $ mkUnqualComponentName "sublib")
-- "pkg:sublib"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.insert (LSubLibName $ mkUnqualComponentName "sublib-b") $ NES.singleton (LSubLibName $ mkUnqualComponentName "sublib-a")
-- "pkg:{sublib-a,sublib-b}"
instance Pretty Dependency where
  pretty (Dependency name ver sublibs) = prettyLibraryNames name (NES.toNonEmpty sublibs) <+> pver
    where
      -- TODO: change to isAnyVersion after #6736
      pver
        | isAnyVersionLight ver = PP.empty
        | otherwise = pretty ver

-- TODO(leana8959): implement packagename part
instance Pretty DependencyAnn where
  pretty (Dependency name ver sublibs) = prettyLibraryNames name (NES.toNonEmpty sublibs) <> pretty ver

-- |
--
-- >>> simpleParsec "mylib:sub" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub") :| [])))
--
-- >>> simpleParsec "mylib:{sub1,sub2}" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub1") :| [LSubLibName (UnqualComponentName "sub2")])))
--
-- >>> simpleParsec "mylib:{ sub1 , sub2 }" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub1") :| [LSubLibName (UnqualComponentName "sub2")])))
--
-- >>> simpleParsec "mylib:{ sub1 , sub2 } ^>= 42" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (MajorBoundVersion (mkVersion [42])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub1") :| [LSubLibName (UnqualComponentName "sub2")])))
--
-- >>> simpleParsec "mylib:{ } ^>= 42" :: Maybe Dependency
-- Nothing
--
-- >>> traverse_ print (map simpleParsec ["mylib:mylib", "mylib:{mylib}", "mylib:{mylib,sublib}" ] :: [Maybe Dependency])
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LMainLibName :| [])))
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LMainLibName :| [])))
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LMainLibName :| [LSubLibName (UnqualComponentName "sublib")])))
--
-- Spaces around colon are not allowed:
--
-- >>> map simpleParsec ["mylib: sub", "mylib :sub", "mylib: {sub1,sub2}", "mylib :{sub1,sub2}"] :: [Maybe Dependency]
-- [Nothing,Nothing,Nothing,Nothing]
--
-- Sublibrary syntax is accepted since @cabal-version: 3.0@
--
-- >>> map (`simpleParsec'` "mylib:sub") [CabalSpecV2_4, CabalSpecV3_0] :: [Maybe Dependency]
-- [Nothing,Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub") :| [])))]
instance Parsec Dependency where
  parsec = unannotateDependencyAnn <$> parsec

instance Parsec (DependencyAnn) where
  parsec = do
    (pname :: PackageNameAnn, libraries) <- do
      name <- unPackageNameST <$> parsec
      libs <- option mainLibSet $ do
        _ <- char ':'
        versionGuardMultilibs
        NES.singleton <$> parseLib <|> parseMultipleLibs

      post <- spaces' -- https://github.com/haskell/cabal/issues/5846
      pure (PackageName $ Ann (postTrivia post) name, libs)

    ver <- parsec <|> pure anyVersionAnn
    return $ mkDependencyAnn pname ver libraries
    where
      parseLib = LSubLibName <$> parsec
      parseMultipleLibs =
        between
          (char '{' *> spaces)
          (spaces *> char '}')
          (NES.fromNonEmpty <$> parsecCommaNonEmpty parseLib)

versionGuardMultilibs :: CabalParsing m => m ()
versionGuardMultilibs = do
  csv <- askCabalSpecVersion
  when (csv < CabalSpecV3_0) $
    fail $
      unwords
        [ "Sublibrary dependency syntax used."
        , "To use this syntax the package needs to specify at least 'cabal-version: 3.0'."
        , "Alternatively, if you are depending on an internal library, you can write"
        , "directly the library name as it were a package."
        ]

-- | Library set with main library.
--
-- @since 3.4.0.0
mainLibSet :: NonEmptySet LibraryName
mainLibSet = NES.singleton LMainLibName

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range comps) =
  Dependency name (simplifyVersionRange range) comps
