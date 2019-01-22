{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.Library (
    Library(..),
    emptyLibrary,
    explicitLibModules,
    libModulesAutogen,
    libModules,
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Types.BuildInfo
import Distribution.Types.LibraryVisibility
import Distribution.Types.ModuleReexport
import Distribution.Types.LibraryName

import qualified Distribution.Types.BuildInfo.Lens as L

data Library = Library
    { libName           :: LibraryName
    , exposedModules    :: [ModuleName]
    , reexportedModules :: [ModuleReexport]
    , signatures        :: [ModuleName]       -- ^ What sigs need implementations?
    , libExposed        :: Bool               -- ^ Is the lib to be exposed by default? (i.e. whether its modules available in GHCi for example)
    , libVisibility     :: LibraryVisibility  -- ^ Whether this multilib can be dependent from outside.
    , libBuildInfo      :: BuildInfo
    }
    deriving (Generic, Show, Eq, Read, Typeable, Data)

instance L.HasBuildInfo Library where
    buildInfo f l = (\x -> l { libBuildInfo = x }) <$> f (libBuildInfo l)

instance Binary Library

instance NFData Library where rnf = genericRnf

emptyLibrary :: Library
emptyLibrary = Library
    { libName           = LMainLibName
    , exposedModules    = mempty
    , reexportedModules = mempty
    , signatures        = mempty
    , libExposed        = True
    , libVisibility     = mempty
    , libBuildInfo      = mempty
    }

-- | This instance is not good.
--
-- We need it for 'PackageDescription.Configuration.addBuildableCondition'.
-- More correct method would be some kind of "create empty clone".
--
-- More concretely, 'addBuildableCondition' will make `libVisibility = False`
-- libraries when `buildable: false`. This may cause problems.
--
instance Monoid Library where
    mempty = emptyLibrary
    mappend = (<>)

instance Semigroup Library where
  a <> b = Library
    { libName           = combineLibraryName (libName a) (libName b)
    , exposedModules    = combine exposedModules
    , reexportedModules = combine reexportedModules
    , signatures        = combine signatures
    , libExposed        = libExposed a && libExposed b -- so False propagates
    , libVisibility     = combine libVisibility
    , libBuildInfo      = combine libBuildInfo
    }
    where combine field = field a `mappend` field b

-- | Get all the module names from the library (exposed and internal modules)
-- which are explicitly listed in the package description which would
-- need to be compiled.  (This does not include reexports, which
-- do not need to be compiled.)  This may not include all modules for which
-- GHC generated interface files (i.e., implicit modules.)
explicitLibModules :: Library -> [ModuleName]
explicitLibModules lib = exposedModules lib
              ++ otherModules (libBuildInfo lib)
              ++ signatures lib

-- | Get all the auto generated module names from the library, exposed or not.
-- This are a subset of 'libModules'.
libModulesAutogen :: Library -> [ModuleName]
libModulesAutogen lib = autogenModules (libBuildInfo lib)

-- | Backwards-compatibility shim for 'explicitLibModules'.  In most cases,
-- you actually want 'allLibModules', which returns all modules that will
-- actually be compiled, as opposed to those which are explicitly listed
-- in the package description ('explicitLibModules'); unfortunately, the
-- type signature for 'allLibModules' is incompatible since we need a
-- 'ComponentLocalBuildInfo'.
{-# DEPRECATED libModules "If you want all modules that are built with a library, use 'allLibModules'.  Otherwise, use 'explicitLibModules' for ONLY the modules explicitly mentioned in the package description. This symbol will be removed in Cabal-3.0 (est. Mar 2019)." #-}
libModules :: Library -> [ModuleName]
libModules = explicitLibModules

-- | Combine 'LibraryName'. in parsing we prefer value coming
-- from munged @name@ field over the @lib-name@.
--
-- /Should/ be irrelevant.
combineLibraryName :: LibraryName -> LibraryName -> LibraryName
combineLibraryName l@(LSubLibName _) _ = l
combineLibraryName _ l                 = l
