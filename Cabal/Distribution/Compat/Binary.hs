{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_binary
#define MIN_VERSION_binary(x, y, z) 0
#endif

module Distribution.Compat.Binary
       ( decodeOrFailIO
#if __GLASGOW_HASKELL__ >= 708 || MIN_VERSION_binary(0,7,0)
       , module Data.Binary
#else
       , Binary(..)
       , decode, encode
#endif
       ) where

import Data.ByteString.Lazy (ByteString)

#if __GLASGOW_HASKELL__ >= 708 || MIN_VERSION_binary(0,7,0)

import Data.Binary

decodeOrFailIO :: Binary a => ByteString -> IO (Either String a)
decodeOrFailIO bs =
  return $ case decodeOrFail bs of
    Left (_, _, msg) -> Left msg
    Right (_, _, a) -> Right a

#else

import Control.Exception (ErrorCall(..), catch, evaluate)
import Data.Binary.Get
import Data.Binary.Put
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Distribution.Compat.Binary.Class
import Distribution.Compat.Binary.Generic ()

-- | Decode a value from a lazy ByteString, reconstructing the original structure.
--
decode :: Binary a => ByteString -> a
decode = runGet get

-- | Encode a value using binary serialisation to a lazy ByteString.
--
encode :: Binary a => a -> ByteString
encode = runPut . put
{-# INLINE encode #-}

decodeOrFailIO :: Binary a => ByteString -> IO (Either String a)
decodeOrFailIO bs =
  catch (evaluate (decode bs) >>= return . Right)
  $ \(ErrorCall str) -> return $ Left str

#endif
