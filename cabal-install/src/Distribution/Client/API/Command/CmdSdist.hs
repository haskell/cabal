-- | Tier-1 stable API: the @cabal sdist@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdSdist
  ( -- * The @sdist@ CLI and action
    sdistCommand
  , sdistAction
  , SdistFlags (..)
  , defaultSdistFlags
  , sdistOptions

    -- * The sdist engine
  , OutputFormat (..) -- Note: hix
  , packageToSdist -- Note: hix
  ) where

import Distribution.Client.CmdSdist
  ( OutputFormat (..)
  , SdistFlags (..)
  , defaultSdistFlags
  , packageToSdist
  , sdistAction
  , sdistCommand
  , sdistOptions
  )
