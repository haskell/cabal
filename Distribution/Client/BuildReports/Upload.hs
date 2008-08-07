{-# LANGUAGE PatternGuards #-}
-- This is a quick hack for uploading build reports to Hackage.

module Distribution.Client.BuildReports.Upload
    ( BuildLog
    , BuildReportId
    , uploadReports
    , postBuildReport
    , putBuildLog
    ) where

import Distribution.Client.Types (Username(..), Password(..))
import Distribution.Client.HttpUtils (proxy)

import Distribution.Simple.Utils (debug, notice, warn)
import Distribution.Verbosity (Verbosity)

import Network.Browser
         ( BrowserAction, browse, request
         , Authority(..), addAuthority, setAuthorityGen
         , setOutHandler, setErrHandler, setProxy
         , setAllowRedirects )
import Network.HTTP
         ( Header(..), HeaderName(..)
         , Request(..), RequestMethod(..), Response(..) )
import Network.URI (URI, uriPath, parseURI,parseRelativeReference, relativeTo)

import Data.Char        (intToDigit)
import Numeric          (showHex)
import System.IO        (hFlush, stdin, stdout, hGetEcho, hSetEcho
                        ,openBinaryFile, IOMode(ReadMode), hGetContents)
import Control.Exception (bracket)
import Control.Monad
import System.Random    (randomRIO)
import System.FilePath.Posix
import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import Distribution.Client.BuildReports.Anonymous (BuildReport)

type BuildReportId = URI
type BuildLog = String

uploadReports :: URI -> [(BuildReport, Maybe BuildLog)] ->  BrowserAction ()
uploadReports uri reports
    = forM_ reports $ \(report, mbBuildLog) ->
      do buildId <- postBuildReport uri report
         case mbBuildLog of
           Just buildLog -> putBuildLog buildId buildLog
           Nothing       -> return ()

postBuildReport :: URI -> BuildReport -> BrowserAction BuildReportId
postBuildReport uri buildReport = do
  setAllowRedirects False
  (_, response) <- request Request {
    rqURI     = uri,
    rqMethod  = POST,
    rqHeaders = [Header HdrContentType   ("text/plain"),
                 Header HdrContentLength (show (length body)),
                 Header HdrAccept        ("text/plain")],
    rqBody    = body
  }
  case rspCode response of
    (3,0,3) | [Just buildId] <- [ do rel <- parseRelativeReference location
                                     relativeTo rel uri
                                  | Header HdrLocation location <- rspHeaders response ]
              -> return $ buildId
    _         -> error "Unrecognised response from server."
  where body  = BuildReport.show buildReport

putBuildLog :: BuildReportId -> BuildLog -> BrowserAction ()
putBuildLog reportId buildLog = do
  (_, response) <- request Request {
      rqURI     = reportId{uriPath = uriPath reportId </> "buildlog"},
      rqMethod  = PUT,
      rqHeaders = [Header HdrContentType   ("text/plain"),
                   Header HdrContentLength (show (length buildLog)),
                   Header HdrAccept        ("text/plain")],
      rqBody    = buildLog
    }
  return ()
