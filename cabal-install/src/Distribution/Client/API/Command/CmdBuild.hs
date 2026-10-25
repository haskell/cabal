-- | Tier-1 stable API: the @cabal build@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdBuild
  ( -- * The @build@ CLI and action
    buildCommand
  , buildAction
  , BuildFlags (..)
  , defaultBuildFlags
  ) where

import Distribution.Client.CmdBuild
  ( BuildFlags (..)
  , buildAction
  , buildCommand
  , defaultBuildFlags
  )
