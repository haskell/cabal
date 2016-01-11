{-# LANGUAGE CPP #-}
module Distribution.Client.GlobalFlags (
    GlobalFlags(..)
  , defaultGlobalFlags
  , RepoContext(..)
  , withRepoContext
  ) where

import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..) )
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Utils.NubList
         ( NubList, fromNubList )
import Distribution.Client.HttpUtils
         ( HttpTransport, configureTransport )
import Distribution.Verbosity
         ( Verbosity )

import Control.Concurrent
         ( MVar, newMVar, modifyMVar )
import System.FilePath
         ( (</>) )

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

-- ------------------------------------------------------------
-- * Repo context
-- ------------------------------------------------------------

-- | Access to repositories
data RepoContext = RepoContext {
    -- | All user-specified repositories
    repoContextRepos :: [Repo]

    -- | Get the HTTP transport
    --
    -- The transport will be initialized on the first call to this function.
    --
    -- NOTE: It is important that we don't eagerly initialize the transport.
    -- Initializing the transport is not free, and especially in contexts where
    -- we don't know a-priori whether or not we need the transport (for instance
    -- when using cabal in "nix mode") incurring the overhead of transport
    -- initialization on _every_ invocation (eg @cabal build@) is undesirable.
  , repoContextGetTransport :: IO HttpTransport

    -- | Should we ignore expiry times (when checking security)?
  , repoContextIgnoreExpiry :: Bool
  }

withRepoContext :: Verbosity -> GlobalFlags -> (RepoContext -> IO a) -> IO a
withRepoContext verbosity globalFlags callback = do
    transportRef <- newMVar Nothing
    callback RepoContext {
        repoContextRepos        = remoteRepos ++ localRepos
      , repoContextGetTransport = getTransport transportRef
      , repoContextIgnoreExpiry = fromFlagOrDefault False
                                    (globalIgnoreExpiry globalFlags)
      }
  where
    remoteRepos =
      [ RepoRemote remote cacheDir
      | remote <- fromNubList $ globalRemoteRepos globalFlags
      , let cacheDir = fromFlag (globalCacheDir globalFlags)
                   </> remoteRepoName remote ]
    localRepos =
      [ RepoLocal local
      | local <- fromNubList $ globalLocalRepos globalFlags ]

    getTransport :: MVar (Maybe HttpTransport) -> IO HttpTransport
    getTransport transportRef =
      modifyMVar transportRef $ \mTransport -> do
        transport <- case mTransport of
          Just tr -> return tr
          Nothing -> configureTransport
                       verbosity
                       (flagToMaybe (globalHttpTransport globalFlags))
        return (Just transport, transport)
