{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.IndexUtils.Timestamp
-- Copyright   :  (c) 2016 Herbert Valerio Riedel
-- License     :  BSD3
--
-- Timestamp type used in package indexes
module Distribution.Client.IndexUtils.Timestamp
  ( Timestamp
  , nullTimestamp
  , epochTimeToTimestamp
  , timestampToUTCTime
  , utcTimeToTimestamp
  , maximumTimestamp
  ) where

import Distribution.Client.Compat.Prelude

-- read is needed for Text instance
import Prelude (read)

import Data.Time (UTCTime (..), fromGregorianValid, makeTimeOfDayValid, showGregorian, timeOfDayToTime, timeToTimeOfDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import qualified Codec.Archive.Tar.Entry as Tar
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | UNIX timestamp (expressed in seconds since unix epoch, i.e. 1970).
newtype Timestamp = TS Int64 -- Tar.EpochTime
  deriving (Eq, Ord, Enum, NFData, Show, Generic)

epochTimeToTimestamp :: Tar.EpochTime -> Maybe Timestamp
epochTimeToTimestamp et
  | ts == nullTimestamp = Nothing
  | otherwise = Just ts
  where
    ts = TS et

timestampToUTCTime :: Timestamp -> Maybe UTCTime
timestampToUTCTime (TS t)
  | t == minBound = Nothing
  | otherwise = Just $ posixSecondsToUTCTime (fromIntegral t)

utcTimeToTimestamp :: UTCTime -> Maybe Timestamp
utcTimeToTimestamp utct
  | minTime <= t, t <= maxTime = Just (TS (fromIntegral t))
  | otherwise = Nothing
  where
    maxTime = toInteger (maxBound :: Int64)
    minTime = toInteger (succ minBound :: Int64)
    t :: Integer
    t = round . utcTimeToPOSIXSeconds $ utct

-- | Compute the maximum 'Timestamp' value
--
-- Returns 'nullTimestamp' for the empty list.  Also note that
-- 'nullTimestamp' compares as smaller to all non-'nullTimestamp'
-- values.
maximumTimestamp :: [Timestamp] -> Timestamp
maximumTimestamp [] = nullTimestamp
maximumTimestamp xs@(_ : _) = maximum xs

-- returns 'Nothing' if not representable as 'Timestamp'
posixSecondsToTimestamp :: Integer -> Maybe Timestamp
posixSecondsToTimestamp pt
  | minTs <= pt, pt <= maxTs = Just (TS (fromInteger pt))
  | otherwise = Nothing
  where
    maxTs = toInteger (maxBound :: Int64)
    minTs = toInteger (succ minBound :: Int64)

-- | Pretty-prints 'Timestamp' in ISO8601/RFC3339 format
-- (e.g. @"2017-12-31T23:59:59Z"@)
--
-- Returns empty string for 'nullTimestamp' in order for
--
-- > null (display nullTimestamp) == True
--
-- to hold.
showTimestamp :: Timestamp -> String
showTimestamp ts = case timestampToUTCTime ts of
  Nothing -> ""
  -- Note: we don't use 'formatTime' here to avoid incurring a
  -- dependency on 'old-locale' for older `time` libs
  Just UTCTime{..} -> showGregorian utctDay ++ ('T' : showTOD utctDayTime) ++ "Z"
  where
    showTOD = show . timeToTimeOfDay

instance Binary Timestamp
instance Structured Timestamp

instance Pretty Timestamp where
  pretty = Disp.text . showTimestamp

instance Parsec Timestamp where
  parsec = parsePosix <|> parseUTC
    where
      -- \| Parses unix timestamps, e.g. @"\@1474626019"@
      parsePosix = do
        _ <- P.char '@'
        t <- P.integral -- note, no negative timestamps
        maybe (fail (show t ++ " is not representable as timestamp")) return $
          posixSecondsToTimestamp t

      -- \| Parses ISO8601/RFC3339-style UTC timestamps,
      -- e.g. @"2017-12-31T23:59:59Z"@
      --
      -- TODO: support numeric tz offsets; allow to leave off seconds
      parseUTC = do
        -- Note: we don't use 'Data.Time.Format.parseTime' here since
        -- we want more control over the accepted formats.

        ye <- parseYear
        _ <- P.char '-'
        mo <- parseTwoDigits
        _ <- P.char '-'
        da <- parseTwoDigits
        _ <- P.char 'T'

        utctDay <-
          maybe (fail (show (ye, mo, da) ++ " is not valid gregorian date")) return $
            fromGregorianValid ye mo da

        ho <- parseTwoDigits
        _ <- P.char ':'
        mi <- parseTwoDigits
        _ <- P.char ':'
        se <- parseTwoDigits
        _ <- P.char 'Z'

        utctDayTime <-
          maybe (fail (show (ho, mi, se) ++ " is not valid time of day")) (return . timeOfDayToTime) $
            makeTimeOfDayValid ho mi (realToFrac (se :: Int))

        let utc = UTCTime{..}

        maybe (fail (show utc ++ " is not representable as timestamp")) return $ utcTimeToTimestamp utc

      parseTwoDigits = do
        d1 <- P.satisfy isDigit
        d2 <- P.satisfy isDigit
        return (read [d1, d2])

      -- A year must have at least 4 digits; e.g. "0097" is fine,
      -- while "97" is not c.f. RFC3339 which
      -- deprecates 2-digit years
      parseYear = do
        sign <- P.option ' ' (P.char '-')
        ds <- P.munch1 isDigit
        when (length ds < 4) $ fail "Year should have at least 4 digits"
        return (read (sign : ds))

-- | Special timestamp value to be used when 'timestamp' is
-- missing/unknown/invalid
nullTimestamp :: Timestamp
nullTimestamp = TS minBound
