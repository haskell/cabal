-- | Tier-1 stable API: the @cabal outdated@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdOutdated
  ( outdatedCommand
  , outdatedAction
  , OutdatedFlags (..)
  , defaultOutdatedFlags
  , outdatedOptions
  , IgnoreMajorVersionBumps (..)
  ) where

import Distribution.Client.CmdOutdated
  ( IgnoreMajorVersionBumps (..)
  , OutdatedFlags (..)
  , defaultOutdatedFlags
  , outdatedAction
  , outdatedCommand
  , outdatedOptions
  )
