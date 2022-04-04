{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

#ifndef MIN_VERSION_binary
#define MIN_VERSION_binary(x, y, z) 0
#endif

module Distribution.Compat.Binary
       ( decodeOrFailIO
       , decodeFileOrFail'
       , Binary(..)
       , decode, encode, encodeFile
       ) where

import Control.Exception (ErrorCall (..), catch, evaluate)
import Data.ByteString.Lazy (ByteString)

import Data.Binary

-- | Lazily reconstruct a value previously written to a file.
decodeFileOrFail' :: Binary a => FilePath -> IO (Either String a)
decodeFileOrFail' f = either (Left . snd) Right `fmap` decodeFileOrFail f

decodeOrFailIO :: Binary a => ByteString -> IO (Either String a)
decodeOrFailIO bs =
    catch (evaluate (decode bs) >>= return . Right) handler
  where
    handler (ErrorCallWithLocation str _) = return $ Left str
