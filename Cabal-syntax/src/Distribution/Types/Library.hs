{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.Library
  ( Library
  , LibraryAnn
  , LibraryWith (..)
  , emptyLibrary
  , explicitLibModules
  , libModulesAutogen
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Fields.Field

import Distribution.ModuleName
import Distribution.Parsec.Position
import Distribution.Types.BuildInfo
import Distribution.Types.LibraryName
import Distribution.Types.LibraryVisibility
import Distribution.Types.ModuleReexport

import qualified Distribution.Types.BuildInfo.Lens as L

import Distribution.Types.Annotation
import Distribution.Types.Trivia

type Library = LibraryWith Abst
type LibraryAnn = LibraryWith Conc

data LibraryWith (m :: ParsingPhase) = Library
  { libExt :: Maybe (Position, FieldName)
  -- NOTE(leana8959): this breaks a lot of instances, which is one of the reasons that GPD
  -- is bad for th exactprint job.
  -- ^ The extension point, saves exactprint details.
  , libName :: LibraryName
  , exposedModules :: MonoidalFieldAla m [ModuleName]
  , reexportedModules :: [ModuleReexport]
  , signatures :: [ModuleName]
  -- ^ What sigs need implementations?
  , libExposed :: PreserveGrouping m (AnnotateWith Positions m Bool)
  -- ^ Is the lib to be exposed by default? (i.e. whether its modules available in GHCi for example)
  , -- [(Positions, Bool)]

    libVisibility :: LibraryVisibility
  -- ^ Whether this multilib can be used as a dependency for other packages.
  , libBuildInfo :: BuildInfoWith m
  }
  deriving (Generic)

deriving instance Show Library
deriving instance Eq Library
deriving instance Ord Library
deriving instance Read Library
deriving instance Data Library

deriving instance Show (LibraryWith Conc)

instance L.HasBuildInfoWith mod (LibraryWith mod) where
  buildInfo f l = (\x -> l{libBuildInfo = x}) <$> f (libBuildInfo l)

instance Binary Library
instance Structured Library
instance NFData Library where rnf = genericRnf

emptyLibrary :: Library
emptyLibrary =
  Library
    { libName = LMainLibName
    , libExt = Nothing
    , exposedModules = mempty
    , reexportedModules = mempty
    , signatures = mempty
    , libExposed = True
    , libVisibility = mempty
    , libBuildInfo = mempty
    }

emptyLibraryAnn :: LibraryWith Conc
emptyLibraryAnn =
  Library
    { libName = LMainLibName
    , libExt = Nothing
    , exposedModules = mempty
    , reexportedModules = mempty
    , signatures = mempty
    , libExposed = []
    , libVisibility = mempty
    , libBuildInfo = mempty
    }

-- | This instance is not good.
--
-- We need it for 'PackageDescription.Configuration.addBuildableCondition'.
-- More correct method would be some kind of "create empty clone".
--
-- More concretely, 'addBuildableCondition' will make `libVisibility = False`
-- libraries when `buildable: false`. This may cause problems.
instance Monoid Library where
  mempty = emptyLibrary
  mappend = (<>)

instance Monoid (LibraryWith Conc) where
  mempty = emptyLibraryAnn
  mappend = (<>)

instance Semigroup (LibraryWith Conc) where
  a <> b =
    Library
      { libExt = libExt a <|> libExt b
      , libName = combineLibraryName (libName a) (libName b)
      , exposedModules = exposedModules a <> exposedModules b
      , reexportedModules = reexportedModules a <> reexportedModules b
      , signatures = combine signatures
      , libExposed = libExposed a <> libExposed b -- so False propagates
      , libVisibility = libVisibility a <> libVisibility b
      , libBuildInfo = libBuildInfo a <> libBuildInfo b
      }
    where
      combine field = field a `mappend` field b

instance Semigroup Library where
  a <> b =
    Library
      { libExt = libExt a <|> libExt b
      , libName = combineLibraryName (libName a) (libName b)
      , exposedModules = exposedModules a <> exposedModules b
      , reexportedModules = combine reexportedModules
      , signatures = signatures a <> signatures b
      , libExposed = libExposed a && libExposed b -- so False propagates
      , libVisibility = libVisibility a <> libVisibility b
      , libBuildInfo = libBuildInfo a <> libBuildInfo b
      }
    where
      combine field = field a `mappend` field b

-- | Get all the module names from the library (exposed and internal modules)
-- which are explicitly listed in the package description which would
-- need to be compiled.  (This does not include reexports, which
-- do not need to be compiled.)  This may not include all modules for which
-- GHC generated interface files (i.e., implicit modules.)
explicitLibModules :: Library -> [ModuleName]
explicitLibModules lib =
  exposedModules lib
    ++ otherModules (libBuildInfo lib)
    ++ signatures lib

-- | Get all the auto generated module names from the library, exposed or not.
-- This are a subset of 'libModules'.
libModulesAutogen :: Library -> [ModuleName]
libModulesAutogen lib = autogenModules (libBuildInfo lib)

-- | Combine 'LibraryName'. in parsing we prefer value coming
-- from munged @name@ field over the @lib-name@.
--
-- /Should/ be irrelevant.
combineLibraryName :: LibraryName -> LibraryName -> LibraryName
combineLibraryName l@(LSubLibName _) _ = l
combineLibraryName _ l = l
