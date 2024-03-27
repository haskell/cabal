{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Compat.Time
  ( ModTime (..) -- Needed for testing
  , getModTime
  , getFileAge
  , getCurTime
  , posixSecondsToModTime
  , calibrateMtimeChangeDelay
  )
where

import Distribution.Compat.Prelude
import Prelude ()

import System.Directory (getModificationTime)

import Distribution.Simple.Utils (withTempDirectoryCwd)
import Distribution.Utils.Path (getSymbolicPath, sameDirectory)
import Distribution.Verbosity (silent)

import System.FilePath

import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixDayLength)

#if defined mingw32_HOST_OS

import qualified Prelude
import Data.Bits          ((.|.), unsafeShiftL)
#if MIN_VERSION_base(4,7,0)
import Data.Bits          (finiteBitSize)
#else
import Data.Bits          (bitSize)
#endif

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
  deriving (Binary, Generic, Bounded, Eq, Ord, Typeable)

instance Structured ModTime

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
                "Distribution.Compat.Time.getModTime"
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

{- FOURMOLU_DISABLE -}
#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV "windows.h GetFileAttributesExW"
  c_getFileAttributesEx :: LPCTSTR -> Int32 -> LPVOID -> Prelude.IO BOOL

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
extractFileTime x = posixTimeToModTime (modificationTimeHiRes x)

#endif
{- FOURMOLU_ENABLE -}

windowsTick, secToUnixEpoch :: Word64
windowsTick = 10000000
secToUnixEpoch = 11644473600

-- | Convert POSIX seconds to ModTime.
posixSecondsToModTime :: Int64 -> ModTime
posixSecondsToModTime s =
  ModTime $ ((fromIntegral s :: Word64) + secToUnixEpoch) * windowsTick

-- | Convert 'POSIXTime' to 'ModTime'.
posixTimeToModTime :: POSIXTime -> ModTime
posixTimeToModTime p =
  ModTime $
    ceiling (p * 1e7) -- 100 ns precision
      + (secToUnixEpoch * windowsTick)

-- | Return age of given file in days.
getFileAge :: FilePath -> IO Double
getFileAge file = do
  t0 <- getModificationTime file
  t1 <- getCurrentTime
  return $ realToFrac (t1 `diffUTCTime` t0) / realToFrac posixDayLength

-- | Return the current time as 'ModTime'.
getCurTime :: IO ModTime
getCurTime = posixTimeToModTime `fmap` getPOSIXTime -- Uses 'gettimeofday'.

-- | Based on code written by Neil Mitchell for Shake. See
-- 'sleepFileTimeCalibrate' in 'Test.Type'.  Returns a pair
-- of microsecond values: first, the maximum delay seen, and the
-- recommended delay to use before testing for file modification change.
-- The returned delay is never smaller
-- than 10 ms, but never larger than 1 second.
calibrateMtimeChangeDelay :: IO (Int, Int)
calibrateMtimeChangeDelay =
  withTempDirectoryCwd silent Nothing sameDirectory "calibration-" $ \dir -> do
    let fileName = getSymbolicPath dir </> "probe"
    mtimes <- for [1 .. 25] $ \(i :: Int) -> time $ do
      writeFile fileName $ show i
      t0 <- getModTime fileName
      let spin j = do
            writeFile fileName $ show (i, j)
            t1 <- getModTime fileName
            unless (t0 < t1) (spin $ j + 1)
      spin (0 :: Int)
    let mtimeChange = maximum mtimes
        mtimeChange' = min 1000000 $ (max 10000 mtimeChange) * 2
    return (mtimeChange, mtimeChange')
  where
    time :: IO () -> IO Int
    time act = do
      t0 <- getCurrentTime
      act
      t1 <- getCurrentTime
      return . ceiling $! (t1 `diffUTCTime` t0) * 1e6 -- microseconds
