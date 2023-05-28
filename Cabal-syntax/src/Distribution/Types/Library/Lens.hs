module Distribution.Types.Library.Lens
  ( Library
  , module Distribution.Types.Library.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (ModuleName)
import Distribution.Types.BuildInfo (BuildInfo)
import Distribution.Types.Library (Library)
import Distribution.Types.LibraryName (LibraryName)
import Distribution.Types.LibraryVisibility (LibraryVisibility)
import Distribution.Types.ModuleReexport (ModuleReexport)

import qualified Distribution.Types.Library as T

libName :: Lens' Library LibraryName
libName f s = fmap (\x -> s{T.libName = x}) (f (T.libName s))
{-# INLINE libName #-}

exposedModules :: Lens' Library [ModuleName]
exposedModules f s = fmap (\x -> s{T.exposedModules = x}) (f (T.exposedModules s))
{-# INLINE exposedModules #-}

reexportedModules :: Lens' Library [ModuleReexport]
reexportedModules f s = fmap (\x -> s{T.reexportedModules = x}) (f (T.reexportedModules s))
{-# INLINE reexportedModules #-}

signatures :: Lens' Library [ModuleName]
signatures f s = fmap (\x -> s{T.signatures = x}) (f (T.signatures s))
{-# INLINE signatures #-}

libExposed :: Lens' Library Bool
libExposed f s = fmap (\x -> s{T.libExposed = x}) (f (T.libExposed s))
{-# INLINE libExposed #-}

libVisibility :: Lens' Library LibraryVisibility
libVisibility f s = fmap (\x -> s{T.libVisibility = x}) (f (T.libVisibility s))
{-# INLINE libVisibility #-}

libBuildInfo :: Lens' Library BuildInfo
libBuildInfo f s = fmap (\x -> s{T.libBuildInfo = x}) (f (T.libBuildInfo s))
{-# INLINE libBuildInfo #-}
