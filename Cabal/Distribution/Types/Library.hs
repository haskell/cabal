{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Library (
    Library(..),
    emptyLibrary,
    libModules,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.BuildInfo
import Distribution.Types.ModuleReexport
import Distribution.ModuleName

data Library = Library {
        libName :: Maybe String,
        exposedModules    :: [ModuleName],
        reexportedModules :: [ModuleReexport],
        requiredSignatures:: [ModuleName], -- ^ What sigs need implementations?
        libExposed        :: Bool, -- ^ Is the lib to be exposed by default?
        libBuildInfo      :: BuildInfo
    }
    deriving (Generic, Show, Eq, Read, Typeable, Data)

instance Binary Library

instance Monoid Library where
  mempty = Library {
    libName = mempty,
    exposedModules = mempty,
    reexportedModules = mempty,
    requiredSignatures = mempty,
    libExposed     = True,
    libBuildInfo   = mempty
  }
  mappend = (<>)

instance Semigroup Library where
  a <> b = Library {
    libName = combine libName,
    exposedModules = combine exposedModules,
    reexportedModules = combine reexportedModules,
    requiredSignatures = combine requiredSignatures,
    libExposed     = libExposed a && libExposed b, -- so False propagates
    libBuildInfo   = combine libBuildInfo
  }
    where combine field = field a `mappend` field b

emptyLibrary :: Library
emptyLibrary = mempty

-- | Get all the module names from the library (exposed and internal modules)
-- which need to be compiled.  (This does not include reexports, which
-- do not need to be compiled.)
libModules :: Library -> [ModuleName]
libModules lib = exposedModules lib
              ++ otherModules (libBuildInfo lib)
              ++ requiredSignatures lib
