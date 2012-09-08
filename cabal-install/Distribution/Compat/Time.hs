{-# LANGUAGE CPP #-}
module Distribution.Compat.Time where

import Data.Int (Int64)
import System.Directory (getModificationTime)

#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixDayLength)
import Data.Time (getCurrentTime, diffUTCTime)
#else
import System.Time (ClockTime(..), getClockTime, diffClockTimes, normalizeTimeDiff, tdDay)
#endif

-- | The number of seconds since the UNIX epoch
type EpochTime = Int64

getModTime :: FilePath -> IO EpochTime
getModTime path =  do
#if MIN_VERSION_directory(1,2,0)
  (truncate . utcTimeToPOSIXSeconds) `fmap` getModificationTime path
#else
  (TOD s _) <- getModificationTime path
  return $! fromIntegral s
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
