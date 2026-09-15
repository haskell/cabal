-- | Tier-0 stable API: downloading files over HTTP.
--
-- Thin wrapper over the internal @Distribution.Client.HttpUtils@.
module Distribution.Client.API.Download
  ( HttpTransport
  , DownloadResult (..)
  , configureHttpTransport
  , downloadFile
  ) where

import Distribution.Client.API.Config (GlobalConfig (..))
import Distribution.Client.Config (savedGlobalFlags)
import Distribution.Client.GlobalFlags (globalHttpTransport)
import Distribution.Client.HttpUtils
  ( DownloadResult (..)
  , HttpTransport
  , configureTransport
  , downloadURI
  )
import Distribution.Simple.Setup (flagToMaybe)
import Distribution.Verbosity (Verbosity)
import Network.URI (URI)

-- | Pick the HTTP transport according to the user's config
-- (respecting the @http-transport@ setting).
configureHttpTransport :: Verbosity -> GlobalConfig -> IO HttpTransport
configureHttpTransport verbosity (GlobalConfig cfg) =
  configureTransport
    verbosity
    []
    (flagToMaybe (globalHttpTransport (savedGlobalFlags cfg)))

-- | Download the given URI to a local path.
downloadFile :: HttpTransport -> Verbosity -> URI -> FilePath -> IO DownloadResult
downloadFile = downloadURI
