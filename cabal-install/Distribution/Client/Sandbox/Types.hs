-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.Types
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Helpers for writing code that works both inside and outside a sandbox.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.Types (
  UseSandbox(..), isUseSandbox, whenUsingSandbox,
  SandboxPackageInfo(..)
  ) where

import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import Distribution.Client.Types (SourcePackage)

import Data.Monoid
import qualified Data.Set as S

-- | Are we using a sandbox?
data UseSandbox = UseSandbox FilePath | NoSandbox

instance Monoid UseSandbox where
  mempty = NoSandbox

  NoSandbox        `mappend` s                  = s
  u0@(UseSandbox _) `mappend` NoSandbox         = u0
  (UseSandbox   _)  `mappend` u1@(UseSandbox _) = u1

-- | Convert a @UseSandbox@ value to a boolean. Useful in conjunction with
-- @when@.
isUseSandbox :: UseSandbox -> Bool
isUseSandbox (UseSandbox _) = True
isUseSandbox NoSandbox      = False

-- | Execute an action only if we're in a sandbox, feeding to it the path to the
-- sandbox directory.
whenUsingSandbox :: UseSandbox -> (FilePath -> IO ()) -> IO ()
whenUsingSandbox NoSandbox               _   = return ()
whenUsingSandbox (UseSandbox sandboxDir) act = act sandboxDir

-- | Data about the packages installed in the sandbox that is passed from
-- 'reinstallAddSourceDeps' to the solver.
data SandboxPackageInfo = SandboxPackageInfo {
  modifiedAddSourceDependencies :: ![SourcePackage],
  -- ^ Modified add-source deps that we want to reinstall. These are guaranteed
  -- to be already installed in the sandbox.

  otherAddSourceDependencies    :: ![SourcePackage],
  -- ^ Remaining add-source deps. Some of these may be not installed in the
  -- sandbox.

  otherInstalledSandboxPackages :: !InstalledPackageIndex.PackageIndex,
  -- ^ All packages installed in the sandbox. Intersection with
  -- 'modifiedAddSourceDependencies' and/or 'otherAddSourceDependencies' can be
  -- non-empty.

  allAddSourceDependencies      :: !(S.Set FilePath)
  -- ^ A set of paths to all add-source dependencies, for convenience.
  }
