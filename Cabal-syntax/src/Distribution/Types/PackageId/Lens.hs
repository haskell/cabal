module Distribution.Types.PackageId.Lens
  ( PackageIdentifier
  , module Distribution.Types.PackageId.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.PackageId (PackageIdentifier, PackageIdentifierWith)
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (Version)

import Distribution.Trivia
import Distribution.Types.Modify (AnnotateWith)

import qualified Distribution.Types.PackageId as T

pkgName :: Lens' (PackageIdentifierWith mod) (AnnotateWith Positions mod PackageName)
pkgName f s = fmap (\x -> s{T.pkgName = x}) (f (T.pkgName s))
{-# INLINE pkgName #-}

pkgVersion :: Lens' (PackageIdentifierWith mod) Version
pkgVersion f s = fmap (\x -> s{T.pkgVersion = x}) (f (T.pkgVersion s))
{-# INLINE pkgVersion #-}
