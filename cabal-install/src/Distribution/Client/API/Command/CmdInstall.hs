-- | Tier-1 stable API: the @cabal install@ command facade.
--
-- Policy: additive-only -- new names in a minor release, changes to
-- the exported signatures in a major.
module Distribution.Client.API.Command.CmdInstall
  ( -- * The @install@ CLI and action
    installCommand
  , installAction

    -- * Client install configuration
  , ClientInstallFlags (..)
  , defaultClientInstallFlags
  , clientInstallOptions
  , OverwritePolicy (..)
  , InstallMethod (..)
  ) where

import Distribution.Client.CmdInstall
  ( installAction
  , installCommand
  )
import Distribution.Client.CmdInstall.ClientInstallFlags
  ( ClientInstallFlags (..)
  , InstallMethod (..)
  , clientInstallOptions
  , defaultClientInstallFlags
  )
import Distribution.Client.Types.OverwritePolicy
  ( OverwritePolicy (..)
  )
