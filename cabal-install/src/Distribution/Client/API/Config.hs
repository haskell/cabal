-- | Tier-0 stable API: the user-level cabal configuration.
module Distribution.Client.API.Config
  ( GlobalConfig (..) -- Note: hackage-revdeps, cabal-matrix
  , loadGlobalConfig -- Note: hackage-revdeps, cabal-matrix
  ) where

import Distribution.Client.Config
  ( SavedConfig
  , loadConfig
  , savedGlobalFlags
  )
import qualified Distribution.Client.GlobalFlags as GF
import Distribution.Simple.Flag (flagToMaybe, pattern Flag)
import Distribution.Verbosity (Verbosity)

-- | The contents of @~/.cabal/config@ plus the process-global settings.
data GlobalConfig = GlobalConfig
  { globalCacheDir :: Maybe FilePath
  -- ^ The directory where downloaded packages and the Hackage index are cached.
  , globalSavedConfig :: Distribution.Client.Config.SavedConfig
  -- ^ The full parsed configuration. Not intended for third-party use:
  -- it is the internal representation and may change.
  }

-- | Load the user config from @~/.cabal/config@ (or the given override).
loadGlobalConfig :: Verbosity -> Maybe FilePath -> IO GlobalConfig
loadGlobalConfig verbosity configFile = do
  saved <- loadConfig verbosity (maybe mempty Flag configFile)
  pure
    GlobalConfig
      { globalCacheDir = flagToMaybe (GF.globalCacheDir (savedGlobalFlags saved))
      , globalSavedConfig = saved
      }
