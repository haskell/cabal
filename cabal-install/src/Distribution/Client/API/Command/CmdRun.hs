-- | Tier-1 stable API: the @cabal run@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdRun
  ( -- * The @run@ CLI and action
    runCommand
  , runAction
  ) where

import Distribution.Client.CmdRun
  ( runAction
  , runCommand
  )
