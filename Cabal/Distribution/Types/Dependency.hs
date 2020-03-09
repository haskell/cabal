{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.Dependency
  ( Dependency(..)
  , depPkgName
  , depVerRange
  , depLibraries
  , thisPackageVersion
  , notThisPackageVersion
  , simplifyDependency
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Version
       (VersionRange, anyVersion, notThisVersion, simplifyVersionRange, thisVersion)

import Distribution.CabalSpecVersion
import Distribution.Compat.CharParsing        (char, spaces)
import Distribution.Compat.Parsing            (between, option)
import Distribution.FieldGrammar.Described
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.LibraryName
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Text.PrettyPrint                       ((<+>))

import qualified Data.Set         as Set
import qualified Text.PrettyPrint as PP

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
instance Structured Dependency
instance NFData Dependency where rnf = genericRnf

instance Pretty Dependency where
    pretty (Dependency name ver sublibs) = pretty name
                                       <<>> optionalMonoid
                                            (sublibs /= Set.singleton LMainLibName)
                                            (PP.colon <<>> PP.braces prettySublibs)
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

-- |
--
-- >>> simpleParsec "mylib:sub" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") AnyVersion (fromList [LSubLibName (UnqualComponentName "sub")]))
--
-- >>> simpleParsec "mylib:{sub1,sub2}" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") AnyVersion (fromList [LSubLibName (UnqualComponentName "sub1"),LSubLibName (UnqualComponentName "sub2")]))
--
-- >>> simpleParsec "mylib:{ sub1 , sub2 }" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") AnyVersion (fromList [LSubLibName (UnqualComponentName "sub1"),LSubLibName (UnqualComponentName "sub2")]))
--
-- >>> simpleParsec "mylib:{ sub1 , sub2 } ^>= 42" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (MajorBoundVersion (mkVersion [42])) (fromList [LSubLibName (UnqualComponentName "sub1"),LSubLibName (UnqualComponentName "sub2")]))
--
-- >>> simpleParsec "mylib:{ } ^>= 42" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (MajorBoundVersion (mkVersion [42])) (fromList []))
--
-- Spaces around colon are not allowed:
--
-- >>> simpleParsec "mylib: sub" :: Maybe Dependency
-- Nothing
--
-- >>> simpleParsec "mylib :sub" :: Maybe Dependency
-- Nothing
--
-- >>> simpleParsec "mylib: {sub1,sub2}" :: Maybe Dependency
-- Nothing
--
-- >>> simpleParsec "mylib :{sub1,sub2}" :: Maybe Dependency
-- Nothing
--
instance Parsec Dependency where
    parsec = do
        name <- parsec

        libs <- option [LMainLibName]
              $ (char ':' *>)
              $ versionGuardMultilibs
              $ pure <$> parseLib name <|> parseMultipleLibs name

        spaces -- https://github.com/haskell/cabal/issues/5846

        ver  <- parsec <|> pure anyVersion
        return $ Dependency name ver $ Set.fromList libs
      where makeLib pn ln | unPackageName pn == ln = LMainLibName
                          | otherwise = LSubLibName $ mkUnqualComponentName ln
            parseLib pn = makeLib pn <$> parsecUnqualComponentName
            parseMultipleLibs pn = between (char '{' *> spaces)
                                           (spaces <* char '}')
                                           $ parsecCommaList $ parseLib pn

instance Described Dependency where
    describe _ = REAppend
        [ RENamed "pkg-name" (describe (Proxy :: Proxy PackageName))
        , REOpt $ 
               RESpaces
            <> reChar ':'
            <> RESpaces
            <> REUnion
                [ reUnqualComponent
                , REAppend
                    [ reChar '{'
                    , RESpaces
                    , RECommaList reUnqualComponent
                    , RESpaces
                    , reChar '}'
                    ] 
                ]
        , REOpt $ RESpaces <> vr
        ]
      where
        vr = RENamed "version-range" (describe (Proxy :: Proxy VersionRange))

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
