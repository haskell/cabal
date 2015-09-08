{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.Types
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Helpers for writing code that works both inside and outside a sandbox.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.Types (
  UseSandbox(..),
  SandboxPackageInfo(..)
  ) where

import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import Distribution.Client.Types (SourcePackage)

import qualified Data.Set as S

-- | Are we using a sandbox? XXX: Comment update
data UseSandbox = UseSandbox { usSandboxDir :: FilePath }

-- | Data about the packages installed in the sandbox that is passed from
-- 'reinstallAddSourceDeps' to the solver.
data SandboxPackageInfo = SandboxPackageInfo {
  modifiedAddSourceDependencies :: ![SourcePackage],
  -- ^ Modified add-source deps that we want to reinstall. These are guaranteed
  -- to be already installed in the sandbox.

  otherAddSourceDependencies    :: ![SourcePackage],
  -- ^ Remaining add-source deps. Some of these may be not installed in the
  -- sandbox.

  otherInstalledSandboxPackages :: !InstalledPackageIndex.InstalledPackageIndex,
  -- ^ All packages installed in the sandbox. Intersection with
  -- 'modifiedAddSourceDependencies' and/or 'otherAddSourceDependencies' can be
  -- non-empty.

  allAddSourceDependencies      :: !(S.Set FilePath)
  -- ^ A set of paths to all add-source dependencies, for convenience.
  }
