-- | Tier-1 stable API: the @cabal repl@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdRepl
  ( -- * The @repl@ CLI and action
    replCommand
  , replAction
  ) where

import Distribution.Client.CmdRepl
  ( replAction
  , replCommand
  )
