-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.Types
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Helpers for writing code that works both inside and outside a sandbox.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.Types (
  UseSandbox(..), isUseSandbox, whenUsingSandbox
  ) where

import Data.Monoid

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
