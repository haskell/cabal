-- | Tier-1 stable API: the argument dictionary of the command facades.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Flags
  ( GlobalFlags (..) -- Note: cabal-install (API.Command.*), hix
  , defaultGlobalFlags -- Note: cabal-install (API.Command.*), hix
  , NixStyleFlags (..) -- Note: cabal-install (API.Command.*), hix, cabal-buck2
  , defaultNixStyleFlags -- Note: cabal-install (API.Command.*), hix, cabal-buck2
  , ProjectFlags (..) -- Note: cabal-install (API.Command.CmdClean, CmdSdist), hix
  , defaultProjectFlags -- Note: cabal-install (API.Command.CmdClean, CmdSdist)
  , ReplFlags (..) -- Note: cabal-install (API.Command.CmdRepl)
  , defaultReplFlags -- Note: cabal-install (API.Command.CmdRepl)
  ) where

import Distribution.Client.GlobalFlags (GlobalFlags (..), defaultGlobalFlags)
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  )
import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  )
import Distribution.Client.ReplFlags (ReplFlags (..), defaultReplFlags)
