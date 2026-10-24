-- | Tier-1 stable API: the @cabal haddock-project@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdHaddockProject
  ( haddockProjectCommand
  , haddockProjectAction
  ) where

import Distribution.Client.CmdHaddockProject
  ( haddockProjectAction
  , haddockProjectCommand
  )
