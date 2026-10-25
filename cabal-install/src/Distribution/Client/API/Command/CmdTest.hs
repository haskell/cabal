-- | Tier-1 stable API: the @cabal test@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdTest
  ( -- * The @test@ CLI and action
    testCommand
  , testAction
  ) where

import Distribution.Client.CmdTest
  ( testAction
  , testCommand
  )
