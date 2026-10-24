-- | Tier-1 stable API: the @cabal update@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdUpdate
  ( updateCommand
  , updateAction -- Note: hix
  ) where

import Distribution.Client.CmdUpdate
  ( updateAction
  , updateCommand
  )
