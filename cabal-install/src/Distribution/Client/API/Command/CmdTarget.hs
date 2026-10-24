-- | Tier-1 stable API: the @cabal target@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdTarget
  ( targetCommand
  , targetAction
  ) where

import Distribution.Client.CmdTarget
  ( targetAction
  , targetCommand
  )
