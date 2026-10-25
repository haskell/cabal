-- | Tier-1 stable API: the @cabal freeze@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdFreeze
  ( freezeCommand
  , freezeAction
  ) where

import Distribution.Client.CmdFreeze
  ( freezeAction
  , freezeCommand
  )
