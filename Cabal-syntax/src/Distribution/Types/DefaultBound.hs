{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.DefaultBound where

import Distribution.Compat.Prelude

import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.UnqualComponentName
import Distribution.Types.VersionRange

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P

-- | Describes a default bound on a package, executable or pkg-config package.
data DefaultBound
  = DefaultUnqualBound PackageName VersionRange
  | DefaultQualBound PackageName UnqualComponentName VersionRange
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

applyDefaultBoundToDependency :: Dependency -> DefaultBound -> Dependency
applyDefaultBoundToDependency dep@(Dependency pkg vorig l) (DefaultUnqualBound pkg' v)
  | pkg == pkg' && isAnyVersion vorig = Dependency pkg v l
  | otherwise = dep
applyDefaultBoundToDependency dep _ = dep

applyDefaultBoundToExeDependency :: ExeDependency -> DefaultBound -> ExeDependency
applyDefaultBoundToExeDependency dep@(ExeDependency pkg comp vorig) (DefaultQualBound pkg' comp' v)
  | pkg == pkg' && comp == comp' && isAnyVersion vorig = ExeDependency pkg comp v
  | otherwise = dep
applyDefaultBoundToExeDependency dep _ = dep

applyDefaultBoundToPkgconfigDependency :: PkgconfigDependency -> DefaultBound -> PkgconfigDependency
applyDefaultBoundToPkgconfigDependency dep@(PkgconfigDependency pkg PcAnyVersion) (DefaultUnqualBound pkg' v)
  | pkg == mkPkgconfigName (unPackageName pkg') = PkgconfigDependency pkg (versionRangeToPkgconfigVersionRange v)
  | otherwise = dep
applyDefaultBoundToPkgconfigDependency dep _ = dep

instance Binary DefaultBound
instance Structured DefaultBound
instance NFData DefaultBound where rnf = genericRnf

instance Pretty DefaultBound where
  pretty (DefaultUnqualBound name ver) = pretty $ Dependency name ver mainLibSet
  pretty (DefaultQualBound name comp ver) = pretty $ ExeDependency name comp ver

instance Parsec DefaultBound where
  parsec = do
    name <- parsec
    mexe <-
      ( do
          _ <- P.char ':'
          exe <- lexemeParsec
          pure (Just exe)
        )
        <|> pure Nothing
    P.spaces
    verRange <- parsec
    case mexe of
      Nothing ->
        pure $ DefaultUnqualBound name verRange
      Just exe ->
        pure $ DefaultQualBound name exe verRange
