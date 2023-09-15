{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}

-- This is a quick hack for uploading build reports to Hackage.

module Distribution.Client.BuildReports.Upload
  ( BuildLog
  , BuildReportId
  , uploadReports
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

{-
import Network.Browser
         ( BrowserAction, request, setAllowRedirects )
import Network.HTTP
         ( Header(..), HeaderName(..)
         , Request(..), RequestMethod(..), Response(..) )
import Network.TCP (HandleStream)
-}
import Network.URI (URI, uriPath) -- parseRelativeReference, relativeTo)

import Distribution.Client.BuildReports.Anonymous (BuildReport, showBuildReport)
import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import Distribution.Client.Errors
import Distribution.Client.HttpUtils
import Distribution.Client.Setup
  ( RepoContext (..)
  )
import Distribution.Client.Types.Credentials (Auth)
import Distribution.Simple.Utils (dieWithException)
import System.FilePath.Posix
  ( (</>)
  )

type BuildReportId = URI
type BuildLog = String

uploadReports :: Verbosity -> RepoContext -> Auth -> URI -> [(BuildReport, Maybe BuildLog)] -> IO ()
uploadReports verbosity repoCtxt auth uri reports = do
  for_ reports $ \(report, mbBuildLog) -> do
    buildId <- postBuildReport verbosity repoCtxt auth uri report
    case mbBuildLog of
      Just buildLog -> putBuildLog verbosity repoCtxt auth buildId buildLog
      Nothing -> return ()

postBuildReport :: Verbosity -> RepoContext -> Auth -> URI -> BuildReport -> IO BuildReportId
postBuildReport verbosity repoCtxt auth uri buildReport = do
  let fullURI = uri{uriPath = "/package" </> prettyShow (BuildReport.package buildReport) </> "reports"}
  transport <- repoContextGetTransport repoCtxt
  res <- postHttp transport verbosity fullURI (showBuildReport buildReport) (Just auth)
  case res of
    (303, redir) -> return $ undefined redir -- TODO parse redir
    _ -> dieWithException verbosity UnrecognizedResponse -- give response

{- FOURMOLU_DISABLE -}
{-
  setAllowRedirects False
  (_, response) <- request Request {
    rqURI     = uri { uriPath = "/package" </> prettyShow (BuildReport.package buildReport) </> "reports" },
    rqMethod  = POST,
    rqHeaders = [Header HdrContentType   ("text/plain"),
                 Header HdrContentLength (show (length body)),
                 Header HdrAccept        ("text/plain")],
    rqBody    = body
  }
  case rspCode response of
    (3,0,3) | [Just buildId] <- [ do rel <- parseRelativeReference location
#if defined(VERSION_network_uri)
                                     return $ relativeTo rel uri
#elif defined(VERSION_network)
#if MIN_VERSION_network(2,4,0)
                                     return $ relativeTo rel uri
#else
                                     relativeTo rel uri
#endif
#endif
                                  | Header HdrLocation location <- rspHeaders response ]
              -> return $ buildId
    _         -> error "Unrecognised response from server."
  where body  = BuildReport.show buildReport
-}
{- FOURMOLU_ENABLE -}

-- TODO force this to be a PUT?

putBuildLog
  :: Verbosity
  -> RepoContext
  -> Auth
  -> BuildReportId
  -> BuildLog
  -> IO ()
putBuildLog verbosity repoCtxt auth reportId buildLog = do
  let fullURI = reportId{uriPath = uriPath reportId </> "log"}
  transport <- repoContextGetTransport repoCtxt
  res <- postHttp transport verbosity fullURI buildLog (Just auth)
  case res of
    (200, _) -> return ()
    _ -> dieWithException verbosity UnrecognizedResponse -- give response
