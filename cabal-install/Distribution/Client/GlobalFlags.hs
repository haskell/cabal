{-# LANGUAGE CPP #-}
module Distribution.Client.GlobalFlags (
    GlobalFlags(..)
  , defaultGlobalFlags
  ) where

import Distribution.Client.Types
         ( RemoteRepo(..) )
import Distribution.Simple.Setup
         ( Flag(..) )
import Distribution.Utils.NubList
         ( NubList )

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
         ( Monoid(..) )
#endif

-- ------------------------------------------------------------
-- * Global flags
-- ------------------------------------------------------------

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags {
    globalVersion           :: Flag Bool,
    globalNumericVersion    :: Flag Bool,
    globalConfigFile        :: Flag FilePath,
    globalSandboxConfigFile :: Flag FilePath,
    globalConstraintsFile   :: Flag FilePath,
    globalRemoteRepos       :: NubList RemoteRepo,     -- ^ Available Hackage servers.
    globalCacheDir          :: Flag FilePath,
    globalLocalRepos        :: NubList FilePath,
    globalLogsDir           :: Flag FilePath,
    globalWorldFile         :: Flag FilePath,
    globalRequireSandbox    :: Flag Bool,
    globalIgnoreSandbox     :: Flag Bool,
    globalIgnoreExpiry      :: Flag Bool,    -- ^ Ignore security expiry dates
    globalHttpTransport     :: Flag String
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags  = GlobalFlags {
    globalVersion           = Flag False,
    globalNumericVersion    = Flag False,
    globalConfigFile        = mempty,
    globalSandboxConfigFile = mempty,
    globalConstraintsFile   = mempty,
    globalRemoteRepos       = mempty,
    globalCacheDir          = mempty,
    globalLocalRepos        = mempty,
    globalLogsDir           = mempty,
    globalWorldFile         = mempty,
    globalRequireSandbox    = Flag False,
    globalIgnoreSandbox     = Flag False,
    globalIgnoreExpiry      = Flag False,
    globalHttpTransport     = mempty
  }

instance Monoid GlobalFlags where
  mempty = GlobalFlags {
    globalVersion           = mempty,
    globalNumericVersion    = mempty,
    globalConfigFile        = mempty,
    globalSandboxConfigFile = mempty,
    globalConstraintsFile   = mempty,
    globalRemoteRepos       = mempty,
    globalCacheDir          = mempty,
    globalLocalRepos        = mempty,
    globalLogsDir           = mempty,
    globalWorldFile         = mempty,
    globalRequireSandbox    = mempty,
    globalIgnoreSandbox     = mempty,
    globalIgnoreExpiry      = mempty,
    globalHttpTransport     = mempty
  }
  mappend a b = GlobalFlags {
    globalVersion           = combine globalVersion,
    globalNumericVersion    = combine globalNumericVersion,
    globalConfigFile        = combine globalConfigFile,
    globalSandboxConfigFile = combine globalConfigFile,
    globalConstraintsFile   = combine globalConstraintsFile,
    globalRemoteRepos       = combine globalRemoteRepos,
    globalCacheDir          = combine globalCacheDir,
    globalLocalRepos        = combine globalLocalRepos,
    globalLogsDir           = combine globalLogsDir,
    globalWorldFile         = combine globalWorldFile,
    globalRequireSandbox    = combine globalRequireSandbox,
    globalIgnoreSandbox     = combine globalIgnoreSandbox,
    globalIgnoreExpiry      = combine globalIgnoreExpiry,
    globalHttpTransport     = combine globalHttpTransport
  }
    where combine field = field a `mappend` field b
