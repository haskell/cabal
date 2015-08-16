{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Distribution.Client.Compat.Time
       (EpochTime, getModTime, getFileAge, getCurTime)
       where

import Data.Int (Int64)
import System.Directory (getModificationTime)

#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixDayLength)
import Data.Time (getCurrentTime, diffUTCTime)
#else
import System.Time (ClockTime(..), getClockTime
                   ,diffClockTimes, normalizeTimeDiff, tdDay, tdHour)
#endif

#if defined mingw32_HOST_OS

#if MIN_VERSION_base(4,7,0)
import Data.Bits          ((.|.), finiteBitSize, unsafeShiftL)
#else
import Data.Bits          ((.|.), bitSize, unsafeShiftL)
#endif
import Data.Int           (Int32)
import Data.Word          (Word64)
import Foreign            (allocaBytes, peekByteOff)
import System.IO.Error    (mkIOError, doesNotExistErrorType)
import System.Win32.Types (BOOL, DWORD, LPCTSTR, LPVOID, withTString)

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV "windows.h GetFileAttributesExW"
  c_getFileAttributesEx :: LPCTSTR -> Int32 -> LPVOID -> IO BOOL

getFileAttributesEx :: String -> LPVOID -> IO BOOL
getFileAttributesEx path lpFileInformation =
  withTString path $ \c_path ->
      c_getFileAttributesEx c_path getFileExInfoStandard lpFileInformation

getFileExInfoStandard :: Int32
getFileExInfoStandard = 0

size_WIN32_FILE_ATTRIBUTE_DATA :: Int
size_WIN32_FILE_ATTRIBUTE_DATA = 36

index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime :: Int
index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime = 20

index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwHighDateTime :: Int
index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwHighDateTime = 24

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
getModTime path = allocaBytes size_WIN32_FILE_ATTRIBUTE_DATA $ \info -> do
  res <- getFileAttributesEx path info
  if not res
    then do
      let err = mkIOError doesNotExistErrorType
                "Distribution.Client.Compat.Time.getModTime"
                Nothing (Just path)
      ioError err
    else do
      dwLow  <- peekByteOff info
                index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime
      dwHigh <- peekByteOff info
                index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwHighDateTime
      return $! windowsTimeToPOSIXSeconds dwLow dwHigh
        where
          windowsTimeToPOSIXSeconds :: DWORD -> DWORD -> EpochTime
          windowsTimeToPOSIXSeconds dwLow dwHigh =
            let wINDOWS_TICK      = 10000000
                sEC_TO_UNIX_EPOCH = 11644473600
#if MIN_VERSION_base(4,7,0)
                qwTime = (fromIntegral dwHigh `unsafeShiftL` finiteBitSize dwHigh)
                         .|. (fromIntegral dwLow)
#else
                qwTime = (fromIntegral dwHigh `unsafeShiftL` bitSize dwHigh)
                         .|. (fromIntegral dwLow)
#endif
                res    = ((qwTime :: Word64) `div` wINDOWS_TICK)
                         - sEC_TO_UNIX_EPOCH
            -- TODO: What if the result is not representable as POSIX seconds?
            -- Probably fine to return garbage.
            in fromIntegral res
#else

-- Directly against the unix library.
getModTime path = do
    -- CTime is Int32 in base 4.5, Int64 in base >= 4.6, and an abstract type in
    -- base < 4.5.
    t <- fmap modificationTime $ getFileStatus path
#if MIN_VERSION_base(4,5,0)
    let CTime i = t
    return (fromIntegral i)
#else
    return (read . show $ t)
#endif
#endif

-- | Return age of given file in days.
getFileAge :: FilePath -> IO Double
getFileAge file = do
  t0 <- getModificationTime file
#if MIN_VERSION_directory(1,2,0)
  t1 <- getCurrentTime
  return $ realToFrac (t1 `diffUTCTime` t0) / realToFrac posixDayLength
#else
  t1 <- getClockTime
  let dt = normalizeTimeDiff (t1 `diffClockTimes` t0)
  return $ fromIntegral ((24 * tdDay dt) + tdHour dt) / 24.0
#endif

getCurTime :: IO EpochTime
getCurTime =  do
#if MIN_VERSION_directory(1,2,0)
  (truncate . utcTimeToPOSIXSeconds) `fmap` getCurrentTime
#else
  (TOD s _) <- getClockTime
  return $! fromIntegral s
#endif
