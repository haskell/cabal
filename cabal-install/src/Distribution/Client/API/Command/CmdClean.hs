-- | Tier-1 stable API: the @cabal clean@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdClean
  ( cleanCommand
  , cleanAction
  ) where

import Distribution.Client.CmdClean
  ( cleanAction
  , cleanCommand
  )
