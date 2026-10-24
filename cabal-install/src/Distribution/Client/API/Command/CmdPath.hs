-- | Tier-1 stable API: the @cabal path@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdPath
  ( pathCommand
  , pathAction
  , PathFlags (..)
  , defaultPathFlags
  , pathOptions
  , PathOutputFormat (..)
  , ConfigPath (..)
  , pathName
  ) where

import Distribution.Client.CmdPath
  ( ConfigPath (..)
  , PathFlags (..)
  , PathOutputFormat (..)
  , defaultPathFlags
  , pathAction
  , pathCommand
  , pathName
  , pathOptions
  )
