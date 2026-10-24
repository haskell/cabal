-- | Tier-1 stable API: the @cabal exec@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdExec
  ( execAction
  , execCommand
  ) where

import Distribution.Client.CmdExec
  ( execAction
  , execCommand
  )
