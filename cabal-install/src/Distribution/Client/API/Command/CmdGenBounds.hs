-- | Tier-1 stable API: the @cabal gen-bounds@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdGenBounds
  ( -- * The @gen-bounds@ CLI and action
    genBoundsCommand
  , genBoundsAction
  , GenBoundsFlags (..)
  , defaultGenBoundsFlags
  ) where

import Distribution.Client.CmdGenBounds
  ( GenBoundsFlags (..)
  , defaultGenBoundsFlags
  , genBoundsAction
  , genBoundsCommand
  )
