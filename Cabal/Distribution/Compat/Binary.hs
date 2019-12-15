{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE PatternSynonyms #-}
#endif

#ifndef MIN_VERSION_binary
#define MIN_VERSION_binary(x, y, z) 0
#endif

module Distribution.Compat.Binary
       ( decodeOrFailIO
       , decodeFileOrFail'
#if __GLASGOW_HASKELL__ >= 708 || MIN_VERSION_binary(0,7,0)
       , module Data.Binary
#else
       , Binary(..)
       , decode, encode, encodeFile
#endif
       ) where

import Control.Exception (ErrorCall (..), catch, evaluate)
import Data.ByteString.Lazy (ByteString)

#if __GLASGOW_HASKELL__ >= 708 || MIN_VERSION_binary(0,7,0)

import Data.Binary

-- | Lazily reconstruct a value previously written to a file.
decodeFileOrFail' :: Binary a => FilePath -> IO (Either String a)
decodeFileOrFail' f = either (Left . snd) Right `fmap` decodeFileOrFail f

#else

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BSL

import Distribution.Compat.Binary.Class
import Distribution.Compat.Binary.Generic ()

-- | Decode a value from a lazy ByteString, reconstructing the
-- original structure.
--
decode :: Binary a => ByteString -> a
decode = runGet get

-- | Encode a value using binary serialisation to a lazy ByteString.
--
encode :: Binary a => a -> ByteString
encode = runPut . put
{-# INLINE encode #-}

-- | Lazily reconstruct a value previously written to a file.
decodeFileOrFail' :: Binary a => FilePath -> IO (Either String a)
decodeFileOrFail' f = decodeOrFailIO =<< BSL.readFile f

-- | Lazily serialise a value to a file
encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile f = BSL.writeFile f . encode

#endif

decodeOrFailIO :: Binary a => ByteString -> IO (Either String a)
decodeOrFailIO bs =
    catch (evaluate (decode bs) >>= return . Right) handler
  where
#if MIN_VERSION_base(4,9,0)
    handler (ErrorCallWithLocation str _) = return $ Left str
#else
    handler (ErrorCall str) = return $ Left str
#endif
