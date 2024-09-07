module ClockUtil
  ( DiffTime
  , AbsoluteTime
  , diffAbsoluteTime
  , getAbsoluteTime
  , formatDiffTime
  ) where

import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

getAbsoluteTime :: IO AbsoluteTime
getAbsoluteTime = systemToTAITime <$> getSystemTime

formatDiffTime :: DiffTime -> String
formatDiffTime delta =
  let minute = secondsToDiffTime 60
      hour = 60 * minute
   in if delta >= hour
        then formatTime defaultTimeLocale "%h:%02M:%02ES" delta
        else
          if delta >= minute
            then formatTime defaultTimeLocale "%m:%2ES" delta
            else formatTime defaultTimeLocale "%2Ess" delta
