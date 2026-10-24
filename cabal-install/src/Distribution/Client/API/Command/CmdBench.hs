-- | Tier-1 stable API: the @cabal bench@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdBench
  ( -- * The @bench@ CLI and action
    benchCommand
  , benchAction
  ) where

import Distribution.Client.CmdBench
  ( benchAction
  , benchCommand
  )
