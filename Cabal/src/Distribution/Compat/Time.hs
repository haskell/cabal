{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Compat.Time
  ( ModTime
  , getModTime
  , getFileAge
  , getCurTime
  )
where

import Distribution.Compat.Prelude
import Prelude ()

import System.Directory (getModificationTime)

import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixDayLength, utcTimeToPOSIXSeconds)

-- | File's modification time, represented
-- internally as a 64-bit unsigned integer in the Windows UTC format.
newtype ModTime = ModTime Word64
  deriving (Binary, Generic, Bounded, Eq, Ord)

instance Structured ModTime

instance Show ModTime where
  show (ModTime x) = show x

instance Read ModTime where
  readsPrec p str = map (first ModTime) (readsPrec p str)

-- | Return modification time of the given file.
getModTime :: FilePath -> IO ModTime
getModTime = fmap (posixTimeToModTime . utcTimeToPOSIXSeconds) . getModificationTime

windowsTick, secToUnixEpoch :: Word64
windowsTick = 10000000
secToUnixEpoch = 11644473600

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
getCurTime = posixTimeToModTime `fmap` getPOSIXTime
