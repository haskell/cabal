-- | Tier-1 stable API: the @cabal configure@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdConfigure
  ( configureCommand
  , configureAction
  ) where

import Distribution.Client.CmdConfigure
  ( configureAction
  , configureCommand
  )
