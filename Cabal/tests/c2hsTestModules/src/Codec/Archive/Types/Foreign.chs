{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Codec.Archive.Types.Foreign ( -- * Callbacks
                                     ArchiveReadCallback
                                   , ArchiveSkipCallback
                                   , ArchiveSeekCallback
                                   , ArchiveWriteCallback
                                   , ArchiveCloseCallbackRaw
                                   , ArchiveOpenCallbackRaw
                                   , ArchiveSwitchCallbackRaw
                                   , ArchivePassphraseCallback
                                   -- * Abstract types
                                   , Archive
                                   , ArchiveEntry
                                   , Stat
                                   , LinkResolver
                                   -- * Enum types
                                   , ArchiveResult (..)
                                   -- * Macros
                                   , Flags (..)
                                   , ArchiveFilter (..)
                                   , ArchiveFormat (..)
                                   , FileType (..)
                                   , ArchiveCapabilities (..)
                                   , ReadDiskFlags (..)
                                   , TimeFlag (..)
                                   , EntryACL (..)
                                   -- * libarchive types
                                   , LaInt64
                                   , LaSSize
                                   , Dev
                                   ) where

import           Control.DeepSeq    (NFData)
import           Data.Bits          (Bits (..))
import           Data.Int           (Int64)
import           Data.Semigroup
import           Foreign.C.String   (CString)
import           Foreign.C.Types    (CInt, CSize)
import           Foreign.Ptr        (Ptr)
import           GHC.Generics       (Generic)
import           System.Posix.Types (CMode)

#include <archive.h>

type LaInt64 = {# type la_int64_t #}
type LaSSize = {# type la_ssize_t #}
type Dev = {# type dev_t #}


{# enum define ArchiveResult { ARCHIVE_OK as ArchiveOk
                             , ARCHIVE_EOF as ArchiveEOF
                             , ARCHIVE_RETRY as ArchiveRetry
                             , ARCHIVE_WARN as ArchiveWarn
                             , ARCHIVE_FAILED as ArchiveFailed
                             , ARCHIVE_FATAL as ArchiveFatal
                             } deriving (Eq, Show, Generic, NFData)
    #}

-- | Abstract type
data Archive

-- | Abstract type
data ArchiveEntry

data Stat

data LinkResolver

type ArchiveReadCallback a b = Ptr Archive -> Ptr a -> Ptr (Ptr b) -> IO LaSSize
type ArchiveSkipCallback a = Ptr Archive -> Ptr a -> Int64 -> IO LaInt64
type ArchiveSeekCallback a = Ptr Archive -> Ptr a -> Int64 -> CInt -> IO LaInt64
type ArchiveWriteCallback a b = Ptr Archive -> Ptr a -> Ptr b -> CSize -> IO LaSSize
type ArchiveOpenCallbackRaw a = Ptr Archive -> Ptr a -> IO CInt
type ArchiveCloseCallbackRaw a = Ptr Archive -> Ptr a -> IO CInt
type ArchiveSwitchCallbackRaw a b = Ptr Archive -> Ptr a -> Ptr b -> IO CInt
type ArchivePassphraseCallback a = Ptr Archive -> Ptr a -> IO CString

newtype ArchiveFormat = ArchiveFormat CInt
    deriving (Eq)

newtype FileType = FileType CMode
    deriving (Eq)

newtype Flags = Flags CInt

newtype ReadDiskFlags = ReadDiskFlags CInt

newtype TimeFlag = TimeFlag CInt

newtype EntryACL = EntryACL CInt

newtype ArchiveFilter = ArchiveFilter CInt

newtype ArchiveCapabilities = ArchiveCapabilities CInt
    deriving (Eq)

instance Semigroup ArchiveCapabilities where
    (<>) (ArchiveCapabilities x) (ArchiveCapabilities y) = ArchiveCapabilities (x .|. y)

instance Monoid ArchiveCapabilities where
    mempty = ArchiveCapabilities 0
    mappend = (<>)

instance Semigroup ReadDiskFlags where
    (<>) (ReadDiskFlags x) (ReadDiskFlags y) = ReadDiskFlags (x .|. y)

instance Semigroup Flags where
    (<>) (Flags x) (Flags y) = Flags (x .|. y)

instance Monoid Flags where
    mempty = Flags 0
    mappend = (<>)

instance Semigroup EntryACL where
    (<>) (EntryACL x) (EntryACL y) = EntryACL (x .|. y)

-- TODO: `has` function for EntryACL
