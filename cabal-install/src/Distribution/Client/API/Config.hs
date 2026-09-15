{-# LANGUAGE PatternSynonyms #-}

-- | Tier-0 stable API: the user-level cabal configuration.
--
-- Thin wrapper over the internal @Distribution.Client.Config@ and
-- @Distribution.Client.GlobalFlags@. Third-party tools must import only
-- this module, not the underlying ones.
module Distribution.Client.API.Config
  ( GlobalConfig (..)
  , loadGlobalConfig
  , defaultGlobalConfigFile
  , globalCacheDir
  , globalHttpTransport
  ) where

import Distribution.Client.Config
  ( SavedConfig (..)
  , defaultConfigFile
  , loadConfig
  )
import qualified Distribution.Client.GlobalFlags as GF
import Distribution.Simple.Flag (Flag, flagToMaybe, pattern Flag)
import Distribution.Verbosity (Verbosity)

-- | The contents of @~/.cabal/config@ plus the process-global settings.
--
-- TODO: make opaque once the accessors below cover the needs of all
-- known consumers (hackage-revdeps, cabal-matrix, cabal-add, cabal-hoogle).
newtype GlobalConfig = GlobalConfig SavedConfig

-- | Load the user config from @~/.cabal/config@ (or the given override).
loadGlobalConfig :: Verbosity -> Maybe FilePath -> IO GlobalConfig
loadGlobalConfig verbosity configFile =
  GlobalConfig <$> loadConfig verbosity (maybe mempty Flag configFile)

-- | The default location of the user config file.
defaultGlobalConfigFile :: IO FilePath
defaultGlobalConfigFile = defaultConfigFile

-- | The directory where downloaded packages and the Hackage index are cached.
globalCacheDir :: GlobalConfig -> Maybe FilePath
globalCacheDir (GlobalConfig cfg) =
  flagToMaybe (GF.globalCacheDir (savedGlobalFlags cfg))

-- | The user-selected HTTP transport override, if any.
globalHttpTransport :: GlobalConfig -> Maybe String
globalHttpTransport (GlobalConfig cfg) =
  flagToMaybe (GF.globalHttpTransport (savedGlobalFlags cfg))
