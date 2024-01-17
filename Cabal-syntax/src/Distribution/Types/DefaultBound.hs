{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.DefaultBound
  ( DefaultBound (..)
  , applyDefaultBoundsToDependency
  , applyDefaultBoundsToExeDependency
  ) where

import Distribution.Compat.Prelude

import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Types.VersionRange

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P

-- | Describes a default bound on a package, executable or pkg-config package,
-- provided via the @default-package-bounds@ section in a cabal file.
data DefaultBound
  = DefaultUnqualBound PackageName VersionRange
  | DefaultQualBound PackageName UnqualComponentName VersionRange
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

applyDefaultBoundsToDependency :: Dependency -> [DefaultBound] -> Dependency
applyDefaultBoundsToDependency dep@(Dependency pkg vorig l) defaultBounds
  | isAnyVersion vorig =
      maybe dep (\v -> Dependency pkg v l) $
        listToMaybe
          [ v | DefaultUnqualBound name v <- defaultBounds, name == pkg
          ]
  | otherwise = dep

applyDefaultBoundsToExeDependency :: ExeDependency -> [DefaultBound] -> ExeDependency
applyDefaultBoundsToExeDependency dep@(ExeDependency pkg comp vorig) defaultBounds
  | isAnyVersion vorig =
      maybe dep (\v -> ExeDependency pkg comp v) $
        listToMaybe
          [ v | DefaultQualBound name comp2 v <- defaultBounds, name == pkg, comp == comp2
          ]
  | otherwise = dep

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
