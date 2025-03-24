module Distribution.Types.PackageId.Lens
  ( PackageIdentifier
  , module Distribution.Types.PackageId.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.PackageId (PackageIdentifier)
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (Version)
import Distribution.Compiler (CompilerId)

import qualified Distribution.Types.PackageId as T

pkgName :: Lens' PackageIdentifier PackageName
pkgName f s = fmap (\x -> s{T.pkgName = x}) (f (T.pkgName s))
{-# INLINE pkgName #-}

pkgVersion :: Lens' PackageIdentifier Version
pkgVersion f s = fmap (\x -> s{T.pkgVersion = x}) (f (T.pkgVersion s))
{-# INLINE pkgVersion #-}

pkgCompiler :: Lens' PackageIdentifier (Maybe CompilerId)
pkgCompiler f s = fmap (\x -> s{T.pkgCompiler = x}) (f (T.pkgCompiler s))
{-# INLINE pkgCompiler #-}

