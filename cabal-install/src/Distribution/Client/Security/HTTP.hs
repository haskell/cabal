{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Implementation of 'HttpLib' using cabal-install's own 'HttpTransport'
module Distribution.Client.Security.HTTP (HttpLib, transportAdapter) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

-- stdlibs

import qualified Data.ByteString.Lazy as BS.L
import qualified Network.HTTP as HTTP
import Network.URI
  ( URI
  )
import System.Directory
  ( getTemporaryDirectory
  )

-- Cabal/cabal-install

import Distribution.Client.HttpUtils
  ( HttpCode
  , HttpTransport (..)
  )
import Distribution.Client.Utils
  ( withTempFileName
  )
import Distribution.Verbosity
  ( Verbosity
  )

-- hackage-security

import qualified Hackage.Security.Client as HC
import Hackage.Security.Client.Repository.HttpLib (HttpLib (..))
import qualified Hackage.Security.Client.Repository.HttpLib as HC
import qualified Hackage.Security.Util.Checked as HC
import qualified Hackage.Security.Util.Pretty as HC

{-------------------------------------------------------------------------------
  'HttpLib' implementation
-------------------------------------------------------------------------------}

-- | Translate from hackage-security's 'HttpLib' to cabal-install's 'HttpTransport'
--
-- NOTE: The match between these two APIs is currently not perfect:
--
-- * We don't get any response headers back from the 'HttpTransport', so we
--   don't know if the server supports range requests. For now we optimistically
--   assume that it does.
-- * The 'HttpTransport' wants to know where to place the resulting file,
--   whereas the 'HttpLib' expects an 'IO' action which streams the download;
--   the security library then makes sure that the file gets written to a
--   location which is suitable (in particular, to a temporary file in the
--   directory where the file needs to end up, so that it can "finalize" the
--   file simply by doing 'renameFile'). Right now we write the file to a
--   temporary file in the system temp directory here and then read it again
--   to pass it to the security library; this is a problem for two reasons: it
--   is a source of inefficiency; and it means that the security library cannot
--   insist on a minimum download rate (potential security attack).
--   Fixing it however would require changing the 'HttpTransport'.
transportAdapter :: Verbosity -> IO HttpTransport -> HttpLib
transportAdapter verbosity getTransport =
  HttpLib
    { httpGet = \headers uri callback -> do
        transport <- getTransport
        httpGetImpl verbosity transport headers uri callback
    , httpGetRange = \headers uri range callback -> do
        transport <- getTransport
        getRange verbosity transport headers uri range callback
    }

httpGetImpl
  :: HC.Throws HC.SomeRemoteError
  => Verbosity
  -> HttpTransport
  -> [HC.HttpRequestHeader]
  -> URI
  -> ([HC.HttpResponseHeader] -> HC.BodyReader -> IO a)
  -> IO a
httpGetImpl verbosity transport reqHeaders uri callback = wrapCustomEx $ do
  get' verbosity transport reqHeaders uri Nothing $ \code respHeaders br ->
    case code of
      200 -> callback respHeaders br
      _ -> HC.throwChecked $ UnexpectedResponse uri code

getRange
  :: HC.Throws HC.SomeRemoteError
  => Verbosity
  -> HttpTransport
  -> [HC.HttpRequestHeader]
  -> URI
  -> (Int, Int)
  -> (HC.HttpStatus -> [HC.HttpResponseHeader] -> HC.BodyReader -> IO a)
  -> IO a
getRange verbosity transport reqHeaders uri range callback = wrapCustomEx $ do
  get' verbosity transport reqHeaders uri (Just range) $ \code respHeaders br ->
    case code of
      200 -> callback HC.HttpStatus200OK respHeaders br
      206 -> callback HC.HttpStatus206PartialContent respHeaders br
      _ -> HC.throwChecked $ UnexpectedResponse uri code

-- | Internal generalization of 'get' and 'getRange'
get'
  :: Verbosity
  -> HttpTransport
  -> [HC.HttpRequestHeader]
  -> URI
  -> Maybe (Int, Int)
  -> (HttpCode -> [HC.HttpResponseHeader] -> HC.BodyReader -> IO a)
  -> IO a
get' verbosity transport reqHeaders uri mRange callback = do
  tempDir <- getTemporaryDirectory
  withTempFileName tempDir "transportAdapterGet" $ \temp -> do
    (code, _etag) <- getHttp transport verbosity uri Nothing temp reqHeaders'
    br <- HC.bodyReaderFromBS =<< BS.L.readFile temp
    callback code [HC.HttpResponseAcceptRangesBytes] br
  where
    reqHeaders' = mkReqHeaders reqHeaders mRange

{-------------------------------------------------------------------------------
  Request headers
-------------------------------------------------------------------------------}

mkRangeHeader :: Int -> Int -> HTTP.Header
mkRangeHeader from to = HTTP.Header HTTP.HdrRange rangeHeader
  where
    -- Content-Range header uses inclusive rather than exclusive bounds
    -- See <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html>
    rangeHeader = "bytes=" ++ show from ++ "-" ++ show (to - 1)

mkReqHeaders :: [HC.HttpRequestHeader] -> Maybe (Int, Int) -> [HTTP.Header]
mkReqHeaders reqHeaders mRange' =
  concat
    [ tr [] reqHeaders
    , [mkRangeHeader fr to | Just (fr, to) <- [mRange]]
    ]
  where
    -- guard against malformed range headers.
    mRange = case mRange' of
      Just (fr, to) | fr >= to -> Nothing
      _ -> mRange'

    tr :: [(HTTP.HeaderName, [String])] -> [HC.HttpRequestHeader] -> [HTTP.Header]
    tr acc [] =
      concatMap finalize acc
    tr acc (HC.HttpRequestMaxAge0 : os) =
      tr (insert HTTP.HdrCacheControl ["max-age=0"] acc) os
    tr acc (HC.HttpRequestNoTransform : os) =
      tr (insert HTTP.HdrCacheControl ["no-transform"] acc) os

    -- Some headers are comma-separated, others need multiple headers for
    -- multiple options.
    --
    -- TODO: Right we just comma-separate all of them.
    finalize :: (HTTP.HeaderName, [String]) -> [HTTP.Header]
    finalize (name, strs) = [HTTP.Header name (intercalate ", " (reverse strs))]

    insert :: Eq a => a -> [b] -> [(a, [b])] -> [(a, [b])]
    insert x y = modifyAssocList x (++ y)

    -- modify the first matching element
    modifyAssocList :: Eq a => a -> (b -> b) -> [(a, b)] -> [(a, b)]
    modifyAssocList a f = go
      where
        go [] = []
        go (p@(a', b) : xs)
          | a == a' = (a', f b) : xs
          | otherwise = p : go xs

{-------------------------------------------------------------------------------
  Custom exceptions
-------------------------------------------------------------------------------}

data UnexpectedResponse = UnexpectedResponse URI Int
  deriving (Typeable)

instance HC.Pretty UnexpectedResponse where
  pretty (UnexpectedResponse uri code) =
    "Unexpected response "
      ++ show code
      ++ " for "
      ++ show uri

#if MIN_VERSION_base(4,8,0)
deriving instance Show UnexpectedResponse
instance Exception UnexpectedResponse where displayException = HC.pretty
#else
instance Show UnexpectedResponse where show = HC.pretty
instance Exception UnexpectedResponse
#endif

wrapCustomEx
  :: ( ( HC.Throws UnexpectedResponse
       , HC.Throws IOException
       )
       => IO a
     )
  -> (HC.Throws HC.SomeRemoteError => IO a)
wrapCustomEx act =
  HC.handleChecked (\(ex :: UnexpectedResponse) -> go ex) $
    HC.handleChecked (\(ex :: IOException) -> go ex) $
      act
  where
    go ex = HC.throwChecked (HC.SomeRemoteError ex)
