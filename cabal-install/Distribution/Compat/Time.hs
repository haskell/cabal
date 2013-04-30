{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Distribution.Compat.Time (EpochTime, getModTime, getFileAge, getCurTime)
       where

import Data.Int (Int64)
import System.Directory (getModificationTime)

#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixDayLength)
import Data.Time (getCurrentTime, diffUTCTime)
#else
import System.Time (ClockTime(..), getClockTime
                   ,diffClockTimes, normalizeTimeDiff, tdDay)
#endif

#if defined mingw32_HOST_OS

import Data.Int         (Int32)
import Data.Word        (Word32)
import Foreign          (Ptr, allocaBytes, peekByteOff)
import Foreign.C.Types  (CChar(..))
import Foreign.C.String (withCString)
import System.IO.Error  (mkIOError, doesNotExistErrorType)

type WIN32_FILE_ATTRIBUTE_DATA = Ptr ()
type LPCSTR = Ptr CChar

foreign import stdcall "Windows.h GetFileAttributesExA"
  c_getFileAttributesEx :: LPCSTR -> Int32
                           -> WIN32_FILE_ATTRIBUTE_DATA -> IO Bool

size_WIN32_FILE_ATTRIBUTE_DATA :: Int
size_WIN32_FILE_ATTRIBUTE_DATA = 36

index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime :: Int
index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime = 20

#else

import Foreign.C.Types    (CTime(..))
import System.Posix.Files (getFileStatus, modificationTime)

#endif

-- | The number of seconds since the UNIX epoch.
type EpochTime = Int64

-- | Return modification time of given file. Works around the low clock
-- resolution problem that 'getModificationTime' has on GHC < 7.8.
--
-- This is a modified version of the code originally written for OpenShake by
-- Neil Mitchell. See module Development.Shake.FileTime.
getModTime :: FilePath -> IO EpochTime

#if defined mingw32_HOST_OS

-- Directly against the Win32 API.
getModTime path = withCString path $ \file ->
  allocaBytes size_WIN32_FILE_ATTRIBUTE_DATA $ \info -> do
    res <- c_getFileAttributesEx file 0 info
    if not res
      then do
        let err = mkIOError doesNotExistErrorType
                  "Distribution.Compat.Time.getModTime"
                  Nothing (Just path)
        ioError err
      else do
        dword <- peekByteOff info
                 index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime
        -- TODO: Convert Windows seconds to POSIX seconds. ATM we don't care
        -- since we only use the value for comparisons.
        return $! fromIntegral (dword :: Word32)
#else

-- Directly against the unix library.
getModTime path = do
    (CTime i) <- fmap modificationTime $ getFileStatus path
    -- CTime is Int32 in base < 4.6, but Int64 in base >= 4.6.
    return (fromIntegral i)
#endif

-- | Return age of given file in days.
getFileAge :: FilePath -> IO Int
getFileAge file = do
  t0 <- getModificationTime file
#if MIN_VERSION_directory(1,2,0)
  t1 <- getCurrentTime
  let days = truncate $ (t1 `diffUTCTime` t0) / posixDayLength
#else
  t1 <- getClockTime
  let days = (tdDay . normalizeTimeDiff) (t1 `diffClockTimes` t0)
#endif
  return days

getCurTime :: IO EpochTime
getCurTime =  do
#if MIN_VERSION_directory(1,2,0)
  (truncate . utcTimeToPOSIXSeconds) `fmap` getCurrentTime
#else
  (TOD s _) <- getClockTime
  return $! fromIntegral s
#endif
