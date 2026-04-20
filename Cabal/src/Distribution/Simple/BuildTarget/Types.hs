{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.BuildTargets.Types
-- Copyright   :  (c) Duncan Coutts 2012
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Handling for user-specified build targets
module Distribution.Simple.BuildTarget.Types
  ( -- * Build targets
    BuildTarget (..)
  , buildTargetComponentName

    -- * User build targets
  , UserBuildTarget (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (ModuleName)
import Distribution.PackageDescription (ComponentName)

-- ------------------------------------------------------------

-- * User build targets

-- ------------------------------------------------------------

-- | Various ways that a user may specify a build target.
data UserBuildTarget
  = -- | A target specified by a single name. This could be a component
    -- module or file.
    --
    -- > cabal build foo
    -- > cabal build Data.Foo
    -- > cabal build Data/Foo.hs  Data/Foo.hsc
    UserBuildTargetSingle String
  | -- | A target specified by a qualifier and name. This could be a component
    -- name qualified by the component namespace kind, or a module or file
    -- qualified by the component name.
    --
    -- > cabal build lib:foo exe:foo
    -- > cabal build foo:Data.Foo
    -- > cabal build foo:Data/Foo.hs
    UserBuildTargetDouble String String
  | -- | A fully qualified target, either a module or file qualified by a
    -- component name with the component namespace kind.
    --
    -- > cabal build lib:foo:Data/Foo.hs exe:foo:Data/Foo.hs
    -- > cabal build lib:foo:Data.Foo exe:foo:Data.Foo
    UserBuildTargetTriple String String String
  deriving (Show, Eq, Ord)

-- ------------------------------------------------------------

-- * Resolved build targets

-- ------------------------------------------------------------

-- | A fully resolved build target.
data BuildTarget
  = -- | A specific component
    BuildTargetComponent ComponentName
  | -- | A specific module within a specific component.
    BuildTargetModule ComponentName ModuleName
  | -- | A specific file within a specific component.
    BuildTargetFile ComponentName FilePath
  deriving (Eq, Show, Generic)

instance Binary BuildTarget

buildTargetComponentName :: BuildTarget -> ComponentName
buildTargetComponentName (BuildTargetComponent cn) = cn
buildTargetComponentName (BuildTargetModule cn _) = cn
buildTargetComponentName (BuildTargetFile cn _) = cn
