module Distribution.Types.Library.Lens
  ( Library
  , module Distribution.Types.Library.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Modify (AnnotateWith, PreserveGrouping)
import Distribution.Trivia

import Distribution.ModuleName (ModuleName)
import Distribution.Types.BuildInfo (BuildInfoWith)
import Distribution.Types.Library (Library, LibraryWith)
import Distribution.Types.LibraryName (LibraryName)
import Distribution.Types.LibraryVisibility (LibraryVisibility)
import Distribution.Types.ModuleReexport (ModuleReexport)

import qualified Distribution.Types.Library as T

libName :: Lens' (LibraryWith mod) LibraryName
libName f s = fmap (\x -> s{T.libName = x}) (f (T.libName s))
{-# INLINE libName #-}

exposedModules :: Lens' (LibraryWith mod) [ModuleName]
exposedModules f s = fmap (\x -> s{T.exposedModules = x}) (f (T.exposedModules s))
{-# INLINE exposedModules #-}

reexportedModules :: Lens' (LibraryWith mod) [ModuleReexport]
reexportedModules f s = fmap (\x -> s{T.reexportedModules = x}) (f (T.reexportedModules s))
{-# INLINE reexportedModules #-}

signatures :: Lens' (LibraryWith mod) [ModuleName]
signatures f s = fmap (\x -> s{T.signatures = x}) (f (T.signatures s))
{-# INLINE signatures #-}

libExposed :: Lens' (LibraryWith mod) (PreserveGrouping mod (AnnotateWith Positions mod Bool))
libExposed f s = fmap (\x -> s{T.libExposed = x}) (f (T.libExposed s))
{-# INLINE libExposed #-}

libVisibility :: Lens' (LibraryWith mod) LibraryVisibility
libVisibility f s = fmap (\x -> s{T.libVisibility = x}) (f (T.libVisibility s))
{-# INLINE libVisibility #-}

libBuildInfo :: Lens' (LibraryWith mod) (BuildInfoWith mod)
libBuildInfo f s = fmap (\x -> s{T.libBuildInfo = x}) (f (T.libBuildInfo s))
{-# INLINE libBuildInfo #-}
