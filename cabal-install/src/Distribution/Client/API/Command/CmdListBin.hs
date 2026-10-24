-- | Tier-1 stable API: the @cabal list-bin@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdListBin
  ( listbinCommand
  , listbinAction
  ) where

import Distribution.Client.CmdListBin
  ( listbinAction
  , listbinCommand
  )
