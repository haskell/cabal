-- | Tier-1 stable API: the @cabal haddock@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdHaddock
  ( -- * The @haddock@ CLI and action
    haddockCommand
  , haddockAction
  , ClientHaddockFlags (..)
  , haddockOptions
  ) where

import Distribution.Client.CmdHaddock
  ( ClientHaddockFlags (..)
  , haddockAction
  , haddockCommand
  , haddockOptions
  )
