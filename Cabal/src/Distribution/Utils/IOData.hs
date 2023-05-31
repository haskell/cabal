{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | @since 2.2.0
module Distribution.Utils.IOData
  ( -- * 'IOData' & 'IODataMode' type
    IOData (..)
  , IODataMode (..)
  , KnownIODataMode (..)
  , withIOData
  , null
  , hPutContents
  ) where

import qualified Data.ByteString.Lazy as LBS
import Distribution.Compat.Prelude hiding (null)
import qualified System.IO
import qualified Prelude

-- | Represents either textual or binary data passed via I/O functions
-- which support binary/text mode
--
-- @since 2.2
data IOData
  = -- | How Text gets encoded is usually locale-dependent.
    IODataText String
  | -- | Raw binary which gets read/written in binary mode.
    IODataBinary LBS.ByteString

withIOData :: IOData -> (forall mode. IODataMode mode -> mode -> r) -> r
withIOData (IODataText str) k = k IODataModeText str
withIOData (IODataBinary lbs) k = k IODataModeBinary lbs

-- | Test whether 'IOData' is empty
null :: IOData -> Bool
null (IODataText s) = Prelude.null s
null (IODataBinary b) = LBS.null b

instance NFData IOData where
  rnf (IODataText s) = rnf s
  rnf (IODataBinary lbs) = rnf lbs

-- | @since 2.2
class NFData mode => KnownIODataMode mode where
  -- | 'IOData' Wrapper for 'System.IO.hGetContents'
  --
  -- __Note__: This operation uses lazy I/O. Use 'NFData' to force all
  -- data to be read and consequently the internal file handle to be
  -- closed.
  hGetIODataContents :: System.IO.Handle -> Prelude.IO mode

  toIOData :: mode -> IOData
  iodataMode :: IODataMode mode

-- | @since 3.2
data IODataMode mode where
  IODataModeText :: IODataMode String
  IODataModeBinary :: IODataMode LBS.ByteString

instance a ~ Char => KnownIODataMode [a] where
  hGetIODataContents h = do
    System.IO.hSetBinaryMode h False
    System.IO.hGetContents h

  toIOData = IODataText
  iodataMode = IODataModeText

instance KnownIODataMode LBS.ByteString where
  hGetIODataContents h = do
    System.IO.hSetBinaryMode h True
    LBS.hGetContents h

  toIOData = IODataBinary
  iodataMode = IODataModeBinary

-- | 'IOData' Wrapper for 'System.IO.hPutStr' and 'System.IO.hClose'
--
-- This is the dual operation ot 'hGetIODataContents',
-- and consequently the handle is closed with `hClose`.
--
-- /Note:/ this performs lazy-IO.
--
-- @since 2.2
hPutContents :: System.IO.Handle -> IOData -> Prelude.IO ()
hPutContents h (IODataText c) = do
  System.IO.hSetBinaryMode h False
  System.IO.hPutStr h c
  System.IO.hClose h
hPutContents h (IODataBinary c) = do
  System.IO.hSetBinaryMode h True
  LBS.hPutStr h c
  System.IO.hClose h
