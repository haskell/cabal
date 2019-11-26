-- | This module corresponds to @archive.h@
--
-- Functions in this module are stateful and hence take place in the 'IO'
-- monad.
module Codec.Archive.Foreign.Archive ( archiveReadHasEncryptedEntries
                                     , archiveReadDiskCanDescend
                                     , archiveReadDiskCurrentFilesystemIsSynthetic
                                     , archiveReadDiskCurrentFilesystemIsRemote
                                     -- * Miscellany
                                     , archiveErrorString
                                     , archiveFormatName
                                     , archiveFormat
                                     , archiveClearError
                                     , archiveSetError
                                     , archiveCopyError
                                     , archiveFileCount
                                     , archiveVersionNumber
                                     , archiveVersionString
                                     , archiveVersionDetails
                                     , archiveFilterCount
                                     , archiveFilterBytes
                                     , archiveFilterCode
                                     , archiveFilterName
                                     , archiveWriteNew
                                     , archiveWriteData
                                     , archiveReadData
                                     , archiveReadNew
                                     -- * Version macros
                                     , archiveVersionNumberMacro
                                     , archiveVersionOnlyString
                                     , archiveVersionStringMacro
                                     -- * Capability macros
                                     , archiveReadFormatCapsNone
                                     , archiveReadFormatCapsEncryptData
                                     , archiveReadFormatCapsEncryptMetadata
                                     -- * Time matching macros
                                     , archiveMatchMTime
                                     , archiveMatchCTime
                                     , archiveMatchNewer
                                     , archiveMatchOlder
                                     , archiveMatchEqual
                                     -- * Entry flags
                                     , archiveExtractOwner
                                     , archiveExtractPerm
                                     , archiveExtractTime
                                     , archiveExtractNoOverwrite
                                     , archiveExtractUnlink
                                     , archiveExtractACL
                                     , archiveExtractFFlags
                                     , archiveExtractXattr
                                     , archiveExtractSecureSymlinks
                                     , archiveExtractSecureNoDotDot
                                     , archiveExtractNoAutodir
                                     , archiveExtractSparse
                                     , archiveExtractMacMetadata
                                     , archiveExtractNoHfsCompression
                                     , archiveExtractHfsCompressionForced
                                     , archiveExtractSecureNoAbsolutePaths
                                     , archiveExtractClearNoChangeFFlags
                                     -- * Filters
                                     , archiveFilterNone
                                     , archiveFilterGzip
                                     , archiveFilterBzip2
                                     , archiveFilterCompress
                                     , archiveFilterProgram
                                     , archiveFilterLzma
                                     , archiveFilterXz
                                     , archiveFilterUu
                                     , archiveFilterRpm
                                     , archiveFilterLzip
                                     , archiveFilterLrzip
                                     , archiveFilterLzop
                                     , archiveFilterGrzip
                                     , archiveFilterLz4
                                     -- * Formats
                                     , archiveFormatCpio
                                     , archiveFormatShar
                                     , archiveFormatTar
                                     , archiveFormatIso9660
                                     , archiveFormatZip
                                     , archiveFormatEmpty
                                     , archiveFormatAr
                                     , archiveFormatMtree
                                     , archiveFormatRaw
                                     , archiveFormatXar
                                     , archiveFormatLha
                                     , archiveFormatCab
                                     , archiveFormatRar
                                     , archiveFormat7zip
                                     , archiveFormatWarc
                                     -- * Read disk flags
                                     , archiveReadDiskRestoreATime
                                     , archiveReadDiskHonorNoDump
                                     , archiveReadDiskMacCopyFile
                                     , archiveReadDiskNoTraverseMounts
                                     , archiveReadDiskNoXattr
                                     -- * Haskell-ized function equivalents
                                     , archiveReadNextHeader
                                     , archiveReadOpenFilename
                                     , archiveReadOpenMemory
                                     , archiveReadSetReadCallback
                                     , archiveReadSetCloseCallback
                                     , archiveReadSetCallbackData
                                     , archiveReadOpen1
                                     , archiveWriteOpenFilename
                                     , archiveWriteOpenMemory
                                     , archiveWriteClose
                                     , archiveWriteHeader
                                     , archiveFree
                                     , archiveWriteOpen
                                     , archiveWriteSetFormatPaxRestricted
                                     , archiveWriteSetFormatZip
                                     , archiveWriteSetFormat7zip
                                     , archiveMatchExcluded
                                     , archiveMatchPathExcluded
                                     , archiveMatchExcludePatternFromFile
                                     , archiveMatchExcludePatternFromFileW
                                     , archiveMatchIncludePatternFromFile
                                     , archiveMatchIncludePatternFromFileW
                                     , archiveMatchTimeExcluded
                                     , archiveMatchOwnerExcluded
                                     , archiveReadDataSkip
                                     , archiveReadSupportFormatAll
                                     , archiveReadExtract
                                     , archiveMatchIncludeGname
                                     , archiveMatchIncludeGnameW
                                     , archiveMatchIncludeUname
                                     , archiveMatchIncludeUnameW
                                     , archiveMatchIncludeUid
                                     , archiveMatchIncludeGid
                                     , archiveReadSupportFilterAll
                                     , archiveReadSupportFilterBzip2
                                     , archiveReadSupportFilterCompress
                                     , archiveReadSupportFilterGzip
                                     , archiveReadSupportFilterGrzip
                                     , archiveReadSupportFilterLrzip
                                     , archiveReadSupportFilterLz4
                                     , archiveReadSupportFilterLzip
                                     , archiveReadSupportFilterLzma
                                     , archiveReadSupportFilterLzop
                                     , archiveReadSupportFilterNone
                                     , archiveReadSupportFilterProgram
                                     , archiveReadSupportFilterProgramSignature
                                     , archiveReadSupportFilterRpm
                                     , archiveReadSupportFilterUu
                                     , archiveReadSupportFilterXz
                                     , archiveReadSupportFormat7zip
                                     , archiveReadSupportFormatAr
                                     , archiveReadSupportFormatByCode
                                     , archiveReadSupportFormatCab
                                     , archiveReadSupportFormatCpio
                                     , archiveReadSupportFormatEmpty
                                     , archiveReadSupportFormatGnutar
                                     , archiveReadSupportFormatIso9660
                                     , archiveReadSupportFormatLha
                                     , archiveReadSupportFormatMtree
                                     , archiveReadSupportFormatRar
                                     , archiveReadSupportFormatRaw
                                     , archiveReadSupportFormatTar
                                     , archiveReadSupportFormatWarc
                                     , archiveReadSupportFormatXar
                                     , archiveReadSupportFormatZip
                                     , archiveReadSupportFormatZipStreamable
                                     , archiveReadSupportFormatZipSeekable
                                     , archiveReadSetFormat
                                     , archiveErrno
                                     -- * Abstract types
                                     , Archive
                                     -- * Haskell types
                                     , ArchiveEncryption (..)
                                     -- * Lower-level API types
                                     , Flags
                                     , ArchiveFilter
                                     , ArchiveFormat
                                     , ArchiveCapabilities
                                     , ReadDiskFlags
                                     , TimeFlag
                                     -- * Callback types
                                     , ArchiveReadCallback
                                     , ArchiveSkipCallback
                                     , ArchiveSeekCallback
                                     , ArchiveWriteCallback
                                     , ArchiveOpenCallbackRaw
                                     , ArchiveCloseCallbackRaw
                                     , ArchiveSwitchCallbackRaw
                                     , ArchivePassphraseCallback
                                     -- * Callback constructors
                                     , noOpenCallback
                                     , mkReadCallback
                                     , mkSkipCallback
                                     , mkSeekCallback
                                     , mkWriteCallback
                                     , mkPassphraseCallback
                                     , mkOpenCallback
                                     , mkCloseCallback
                                     , mkSwitchCallback
                                     , mkWriteLookup
                                     , mkReadLookup
                                     , mkCleanup
                                     , mkMatch
                                     , mkFilter
                                     ) where

{# import qualified Codec.Archive.Types.Foreign #}

import Codec.Archive.Foreign.Archive.Macros
import Codec.Archive.Foreign.Archive.Raw
import Codec.Archive.Types
import Control.Composition ((.*), (.**), (.****))
import Data.Coerce (coerce)
import Data.Int (Int64)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable (Storable (peek))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

-- destructors: use "dynamic" instead of "wrapper" (but we don't want that)
-- callbacks
foreign import ccall "wrapper" mkReadCallback :: ArchiveReadCallback a b -> IO (FunPtr (ArchiveReadCallback a b))
foreign import ccall "wrapper" mkSkipCallback :: ArchiveSkipCallback a -> IO (FunPtr (ArchiveSkipCallback a))
foreign import ccall "wrapper" mkSeekCallback :: ArchiveSeekCallback a -> IO (FunPtr (ArchiveSeekCallback a))
foreign import ccall "wrapper" mkWriteCallback :: ArchiveWriteCallback a b -> IO (FunPtr (ArchiveWriteCallback a b))
foreign import ccall "wrapper" mkOpenCallbackRaw :: ArchiveOpenCallbackRaw a -> IO (FunPtr (ArchiveOpenCallbackRaw a))
foreign import ccall "wrapper" mkCloseCallbackRaw :: ArchiveCloseCallbackRaw a -> IO (FunPtr (ArchiveCloseCallbackRaw a))
foreign import ccall "wrapper" mkSwitchCallbackRaw :: ArchiveSwitchCallbackRaw a b -> IO (FunPtr (ArchiveSwitchCallbackRaw a b))
foreign import ccall "wrapper" mkPassphraseCallback :: ArchivePassphraseCallback a -> IO (FunPtr (ArchivePassphraseCallback a))

-- | Don't use an open callback. This is the recommended argument to 'archive_open_read'
noOpenCallback :: FunPtr (ArchiveOpenCallbackRaw a)
noOpenCallback = castPtrToFunPtr nullPtr

foreign import ccall "wrapper" mkWriteLookup :: (Ptr a -> CString -> Int64 -> IO Int64) -> IO (FunPtr (Ptr a -> CString -> Int64 -> IO Int64))
foreign import ccall "wrapper" mkReadLookup :: (Ptr a -> Int64 -> IO CString) -> IO (FunPtr (Ptr a -> Int64 -> IO CString))
foreign import ccall "wrapper" mkCleanup :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "wrapper" mkMatch :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()))
foreign import ccall "wrapper" preMkFilter :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt))

mkOpenCallback :: ArchiveOpenCallback a -> IO (FunPtr (ArchiveOpenCallbackRaw a))
mkOpenCallback f = let f' = fmap resultToErr .* f in mkOpenCallbackRaw f'

mkCloseCallback :: ArchiveCloseCallback a -> IO (FunPtr (ArchiveCloseCallbackRaw a))
mkCloseCallback f = let f' = fmap resultToErr .* f in mkCloseCallbackRaw f'

mkSwitchCallback :: ArchiveSwitchCallback a b -> IO (FunPtr (ArchiveSwitchCallbackRaw a b))
mkSwitchCallback f = let f' = fmap resultToErr .** f in mkSwitchCallbackRaw f'

mkFilter :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO Bool) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt))
mkFilter f = let f' = fmap boolToInt .** f in preMkFilter f'
    where boolToInt False = 0
          boolToInt True  = 1

#include <archive.h>

{#pointer *archive as ArchivePtr -> Archive #}
{#pointer *archive_entry as ArchiveEntryPtr -> ArchiveEntry #}

{#typedef size_t CSize#}
{#typedef wchar_t CWchar#}
{#default in `CWString' [wchar_t*] castPtr#}

{# fun archive_error_string as ^ { `ArchivePtr' } -> `CString' #}
{# fun archive_format_name as ^ { `ArchivePtr' } -> `CString' #}
{# fun archive_format as ^ { `ArchivePtr' } -> `ArchiveFormat' ArchiveFormat #}
{# fun archive_clear_error as ^ { `ArchivePtr' } -> `()' #}
{# fun archive_set_error as ^ { `ArchivePtr', `CInt', `CString' } -> `()' #}
{# fun archive_copy_error as ^ { `ArchivePtr', `ArchivePtr' } -> `()' #}
{# fun archive_file_count as ^ { `ArchivePtr' } -> `CInt' #}
{# fun archive_version_number as ^ {} -> `CInt' #}
{# fun archive_version_string as ^ {} -> `CString' #}
{# fun archive_version_details as ^ {} -> `CString' #}
{# fun archive_filter_count as ^ { `ArchivePtr' } -> `CInt' #}
{# fun archive_filter_bytes as ^ { `ArchivePtr', `CInt' } -> `LaInt64' id #}
{# fun archive_filter_code as ^ { `ArchivePtr', `CInt' } -> `Int' #}
{# fun archive_filter_name as ^ { `ArchivePtr', `CInt' } -> `CString' #}
{# fun archive_write_new as ^ {} -> `ArchivePtr' #}
{# fun archive_write_data as ^ { `ArchivePtr', castPtr `Ptr a', fromIntegral `CSize' } -> `CSize' fromIntegral #}
{# fun archive_read_new as ^ {} -> `ArchivePtr' #}
-- TODO: fix?
{# fun archive_read_data as ^ { `ArchivePtr', castPtr `Ptr a', fromIntegral `CSize' } -> `CSize' fromIntegral #}

{# fun archive_read_disk_can_descend as ^ { `ArchivePtr' } -> `Bool' #}
{# fun archive_read_disk_current_filesystem_is_synthetic as ^ { `ArchivePtr' } -> `Bool' #}
{# fun archive_read_disk_current_filesystem_is_remote as ^ { `ArchivePtr' } -> `Bool' #}
{# fun archive_match_excluded as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_match_path_excluded as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_match_exclude_pattern_from_file as ^ { `ArchivePtr', `CString', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_exclude_pattern_from_file_w as ^ { `ArchivePtr', `CWString', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_include_pattern_from_file as ^ { `ArchivePtr', `CString', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_include_pattern_from_file_w as ^ { `ArchivePtr', `CWString', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_time_excluded as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_read_has_encrypted_entries as ^ { `ArchivePtr' } -> `ArchiveEncryption' encryptionResult #}
{# fun archive_match_owner_excluded as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_read_next_header as ^ { `ArchivePtr', alloca- `ArchiveEntryPtr' peek* } -> `ArchiveResult' #}
{# fun archive_read_open_filename as ^ { `ArchivePtr', `CString', fromIntegral `CSize' } -> `ArchiveResult' #}
{# fun archive_read_open_memory as ^ { `ArchivePtr', castPtr `Ptr a', fromIntegral `CSize' } -> `ArchiveResult' #}

archiveReadSetReadCallback :: Ptr Archive -> FunPtr (ArchiveReadCallback a b) -> IO ArchiveResult
archiveReadSetReadCallback = fmap errorRes .* archive_read_set_read_callback

archiveReadSetCloseCallback :: Ptr Archive -> FunPtr (ArchiveCloseCallbackRaw a) -> IO ArchiveResult
archiveReadSetCloseCallback = fmap errorRes .* archive_read_set_close_callback

{# fun archive_read_set_callback_data as ^ { `ArchivePtr', castPtr `Ptr a' } -> `ArchiveResult' #}
{# fun archive_read_open1 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_open_filename as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_write_open_memory as ^ { `ArchivePtr', castPtr `Ptr a' , `CSize', alloca- `CSize' peek* } -> `ArchiveResult' #}
{# fun archive_write_close as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_header as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `ArchiveResult' #}
{# fun archive_free as ^ { `ArchivePtr' } -> `ArchiveResult' #}

archiveWriteOpen :: Ptr Archive -> Ptr a -> FunPtr (ArchiveOpenCallbackRaw a) -> FunPtr (ArchiveWriteCallback a b) -> FunPtr (ArchiveCloseCallbackRaw a) -> IO ArchiveResult
archiveWriteOpen = fmap errorRes .**** archive_write_open

{# fun archive_match_include_gname_w as ^ { `ArchivePtr', `CWString' } -> `ArchiveResult' #}
{# fun archive_match_include_gname as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_match_include_uname_w as ^ { `ArchivePtr', `CWString' } -> `ArchiveResult' #}
{# fun archive_match_include_uname as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_match_include_gid as ^ { `ArchivePtr', fromIntegral `Id' } -> `ArchiveResult' #}
{# fun archive_match_include_uid as ^ { `ArchivePtr', fromIntegral `Id' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_all as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_bzip2 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_compress as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_gzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_grzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lrzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lz4 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lzma as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lzop as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_none as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_program as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_program_signature as ^ { `ArchivePtr', `CString', castPtr `Ptr a', coerce `CSize' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_rpm as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_uu as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_xz as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_7zip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_all as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_ar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_by_code as ^ { `ArchivePtr', `CInt' } -> `ArchiveResult' #}
{# fun archive_read_support_format_cab as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_cpio as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_empty as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_gnutar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_iso9660 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_lha as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_mtree as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_rar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_raw as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_tar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_warc as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_xar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_zip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_zip_streamable as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_zip_seekable as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_set_format as ^ { `ArchivePtr', coerce `ArchiveFormat' } -> `ArchiveResult' #}
{# fun archive_write_set_format_zip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_7zip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_pax_restricted as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_data_skip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_extract as ^ { `ArchivePtr', `ArchiveEntryPtr', coerce `Flags' } -> `ArchiveResult' #}
{# fun archive_errno as ^ { `ArchivePtr' } -> `ArchiveResult' #}
