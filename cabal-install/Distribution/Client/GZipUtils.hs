{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

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
#if MIN_VERSION_zlib(0,6,0)
import Data.ByteString.Lazy as BS (ByteString, fromChunks)
#else
import Data.ByteString.Lazy.Internal as BS (ByteString(Empty, Chunk))
#endif
import qualified Data.ByteString as SBS (ByteString)
import Codec.Compression.Zlib.Internal
import Data.Maybe (fromMaybe)

-- | Attempts to decompress the `bytes' under the assumption that
-- "data format" error at the very beginning of the stream means
-- that it is already decompressed. Caller should make sanity checks
-- to verify that it is not, in fact, garbage.
--
-- This is to deal with http proxies that lie to us and transparently
-- decompress without removing the content-encoding header. See:
-- <https://github.com/haskell/cabal/issues/678>
--
maybeDecompress :: ByteString -> ByteString
#if MIN_VERSION_zlib(0,6,0)
maybeDecompress bytes = fromMaybe bytes $ foldDecompressStreamWithInput outputAv inputReq err s bytes
  where
    outputAv :: SBS.ByteString -> Maybe ByteString -> Maybe ByteString
    outputAv outchunk = fmap (fromChunks [outchunk] <>)
      -- (7.4.2 and down do not support 'fromStrict'.)

    inputReq :: ByteString -> Maybe ByteString
    inputReq = Just

    err :: DecompressError -> Maybe ByteString
    err _ = Nothing

    s :: forall s . DecompressStream (ST s)
    s = decompressST gzipOrZlibFormat defaultDecompressParams
#else
maybeDecompress bytes = foldStream $ decompressWithErrors gzipOrZlibFormat defaultDecompressParams bytes
  where
    -- DataError at the beginning of the stream probably means that stream is not compressed.
    -- Returning it as-is.
    -- TODO: alternatively, we might consider looking for the two magic bytes
    -- at the beginning of the gzip header.
    foldStream (StreamError DataError _) = bytes
    foldStream somethingElse = doFold somethingElse

    doFold StreamEnd               = BS.Empty
    doFold (StreamChunk bs stream) = BS.Chunk bs (doFold stream)
    doFold (StreamError _ msg)  = error $ "Codec.Compression.Zlib: " ++ msg
#endif
