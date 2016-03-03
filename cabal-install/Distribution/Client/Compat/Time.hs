{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module Distribution.Client.Compat.Time
       ( ModTime(..) -- Needed for testing
       , getModTime, getFileAge, getCurTime
       , posixSecondsToModTime )
       where

import Control.Arrow    ( first )
import Data.Int         ( Int64 )
import Data.Word        ( Word64 )
import System.Directory ( getModificationTime )

import Distribution.Compat.Binary ( Binary )

import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock.POSIX ( posixDayLength )
import Data.Time             ( diffUTCTime, getCurrentTime )
#else
import System.Time ( getClockTime, diffClockTimes
                   , normalizeTimeDiff, tdDay, tdHour )
#endif

#if defined mingw32_HOST_OS

import Data.Bits          ((.|.), unsafeShiftL)
#if MIN_VERSION_base(4,7,0)
import Data.Bits          (finiteBitSize)
#else
import Data.Bits          (bitSize)
#endif

import Data.Int           ( Int32 )
import Foreign            ( allocaBytes, peekByteOff )
import System.IO.Error    ( mkIOError, doesNotExistErrorType )
import System.Win32.Types ( BOOL, DWORD, LPCTSTR, LPVOID, withTString )

#else

import System.Posix.Files ( FileStatus, getFileStatus )

#if MIN_VERSION_unix(2,6,0)
import System.Posix.Files ( modificationTimeHiRes )
#else
import System.Posix.Files ( modificationTime )
#endif

#endif

-- | An opaque type representing a file's modification time, represented
-- internally as a 64-bit unsigned integer in the Windows UTC format.
newtype ModTime = ModTime Word64
                deriving (Binary, Bounded, Eq, Ord)

instance Show ModTime where
  show (ModTime x) = show x

instance Read ModTime where
  readsPrec p str = map (first ModTime) (readsPrec p str)

-- | Return modification time of the given file. Works around the low clock
-- resolution problem that 'getModificationTime' has on GHC < 7.8.
--
-- This is a modified version of the code originally written for Shake by Neil
-- Mitchell. See module Development.Shake.FileInfo.
getModTime :: FilePath -> IO ModTime

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
#if MIN_VERSION_base(4,7,0)
      let qwTime =
            (fromIntegral (dwHigh :: DWORD) `unsafeShiftL` finiteBitSize dwHigh)
            .|. (fromIntegral (dwLow :: DWORD))
#else
      let qwTime =
            (fromIntegral (dwHigh :: DWORD) `unsafeShiftL` bitSize dwHigh)
            .|. (fromIntegral (dwLow :: DWORD))
#endif
      return $! ModTime (qwTime :: Word64)

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

-- Directly against the unix library.
getModTime path = do
    st <- getFileStatus path
    return $! (extractFileTime st)

extractFileTime :: FileStatus -> ModTime
#if MIN_VERSION_unix(2,6,0)
extractFileTime x = posixTimeToModTime (modificationTimeHiRes x)
#else
extractFileTime x = posixSecondsToModTime $ fromIntegral $ fromEnum $
                    modificationTime x
#endif

#endif

windowsTick, secToUnixEpoch :: Word64
windowsTick    = 10000000
secToUnixEpoch = 11644473600

-- | Convert POSIX seconds to ModTime.
posixSecondsToModTime :: Int64 -> ModTime
posixSecondsToModTime s =
  ModTime $ ((fromIntegral s :: Word64) + secToUnixEpoch) * windowsTick

-- | Convert 'POSIXTime' to 'ModTime'.
posixTimeToModTime :: POSIXTime -> ModTime
posixTimeToModTime p = ModTime $ (ceiling $ p * 1e7) -- 100 ns precision
                       + (secToUnixEpoch * windowsTick)

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

-- | Return the current time as 'ModTime'.
getCurTime :: IO ModTime
getCurTime = posixTimeToModTime `fmap` getPOSIXTime -- Uses 'gettimeofday'.
