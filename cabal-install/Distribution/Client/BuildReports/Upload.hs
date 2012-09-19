{-# LANGUAGE CPP, PatternGuards #-}
-- This is a quick hack for uploading build reports to Hackage.

module Distribution.Client.BuildReports.Upload
    ( BuildLog
    , BuildReportId
    , uploadReports
    , postBuildReport
    , putBuildLog
    ) where

import Network.Browser
         ( BrowserAction, request, setAllowRedirects )
import Network.HTTP
         ( Header(..), HeaderName(..)
         , Request(..), RequestMethod(..), Response(..) )
import Network.TCP (HandleStream)
import Network.URI (URI, uriPath, parseRelativeReference, relativeTo)

import Control.Monad
         ( forM_ )
import System.FilePath.Posix
         ( (</>) )
import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import Distribution.Client.BuildReports.Anonymous (BuildReport)
import Distribution.Text (display)

type BuildReportId = URI
type BuildLog = String

uploadReports :: URI -> [(BuildReport, Maybe BuildLog)]
              ->  BrowserAction (HandleStream BuildLog) ()
uploadReports uri reports = do
  forM_ reports $ \(report, mbBuildLog) -> do
     buildId <- postBuildReport uri report
     case mbBuildLog of
       Just buildLog -> putBuildLog buildId buildLog
       Nothing       -> return ()

postBuildReport :: URI -> BuildReport
                -> BrowserAction (HandleStream BuildLog) BuildReportId
postBuildReport uri buildReport = do
  setAllowRedirects False
  (_, response) <- request Request {
    rqURI     = uri { uriPath = "/package" </> display (BuildReport.package buildReport) </> "reports" },
    rqMethod  = POST,
    rqHeaders = [Header HdrContentType   ("text/plain"),
                 Header HdrContentLength (show (length body)),
                 Header HdrAccept        ("text/plain")],
    rqBody    = body
  }
  case rspCode response of
    (3,0,3) | [Just buildId] <- [ do rel <- parseRelativeReference location
#if MIN_VERSION_network(2,4,0)
                                     return $ relativeTo rel uri
#else
                                     relativeTo rel uri
#endif
                                  | Header HdrLocation location <- rspHeaders response ]
              -> return $ buildId
    _         -> error "Unrecognised response from server."
  where body  = BuildReport.show buildReport

putBuildLog :: BuildReportId -> BuildLog
            -> BrowserAction (HandleStream BuildLog) ()
putBuildLog reportId buildLog = do
  --FIXME: do something if the request fails
  (_, _response) <- request Request {
      rqURI     = reportId{uriPath = uriPath reportId </> "log"},
      rqMethod  = PUT,
      rqHeaders = [Header HdrContentType   ("text/plain"),
                   Header HdrContentLength (show (length buildLog)),
                   Header HdrAccept        ("text/plain")],
      rqBody    = buildLog
    }
  return ()
