{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Distribution.Compat.Binary
  ( decodeOrFailIO
  , decodeFileOrFail'
  , module Data.Binary
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
