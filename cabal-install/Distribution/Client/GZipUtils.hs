{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.GZipUtils
-- Copyright   :  (c) Dmitry Astapov 2010
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides a convenience functions for working with files that may or may not
-- be zipped.
-----------------------------------------------------------------------------
module Distribution.Client.GZipUtils (
    maybeDecompress,
  ) where

import Data.Monoid ((<>))
import Control.Monad.ST.Lazy (ST)
import Data.ByteString.Lazy as BS (ByteString, take, unpack, fromStrict)
import qualified Data.ByteString as SBS (ByteString)
import qualified Codec.Compression.GZip as GZip
import Codec.Compression.Zlib.Internal
import Data.Maybe (fromMaybe)

-- | Does the argument start with @0x1f8b@?  See
-- <http://tools.ietf.org/html/rfc1952#page-5>.
verifyGzipMagicHeader :: ByteString -> Bool
verifyGzipMagicHeader = (== [31, 139]) . BS.unpack . BS.take 2

-- | If first two bytes of input contain magic gzip marker, decompress as gzip.
-- If they don't, attempt to decompress as zlib.  If that fails, return input
-- bytestring.  It is the callers responsibility to verify that it is not, in
-- fact, garbage.
--
-- This is to deal with http proxies that lie to us and transparently
-- decompress without removing the content-encoding header. See:
-- <https://github.com/haskell/cabal/issues/678>.  (Zlib does not provide a
-- magic marker that we can check for, so the only safe way to determine
-- whether the string is zlib-compressed is to try decompress it.)
--
maybeDecompress :: ByteString -> ByteString
maybeDecompress bytes | verifyGzipMagicHeader bytes = GZip.decompress bytes
maybeDecompress bytes = fromMaybe bytes $ foldDecompressStreamWithInput outputAv inputReq err s bytes
  where
    outputAv :: SBS.ByteString -> Maybe ByteString -> Maybe ByteString
    outputAv outchunk = fmap (<> fromStrict outchunk)

    inputReq :: ByteString -> Maybe ByteString
    inputReq = Just

    err :: DecompressError -> Maybe ByteString
    err _ = Nothing

    s :: forall s . DecompressStream (ST s)
    s = decompressST gzipOrZlibFormat defaultDecompressParams

