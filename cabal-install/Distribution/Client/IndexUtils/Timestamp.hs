{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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

import Data.Time             (UTCTime (..), fromGregorianValid, makeTimeOfDayValid, showGregorian, timeOfDayToTime, timeToTimeOfDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Distribution.Parsec   (Parsec (..))
import Distribution.Pretty   (Pretty (..))

import Distribution.FieldGrammar.Described

import qualified Codec.Archive.Tar.Entry         as Tar
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint                as Disp

-- | UNIX timestamp (expressed in seconds since unix epoch, i.e. 1970).
newtype Timestamp = TS Int64 -- Tar.EpochTime
                  deriving (Eq,Ord,Enum,NFData,Show,Generic)

epochTimeToTimestamp :: Tar.EpochTime -> Maybe Timestamp
epochTimeToTimestamp et
  | ts == nullTimestamp  = Nothing
  | otherwise            = Just ts
  where
    ts = TS et

timestampToUTCTime :: Timestamp -> Maybe UTCTime
timestampToUTCTime (TS t)
  | t == minBound  = Nothing
  | otherwise      = Just $ posixSecondsToUTCTime (fromIntegral t)

utcTimeToTimestamp :: UTCTime -> Maybe Timestamp
utcTimeToTimestamp utct
  | minTime <= t, t <= maxTime  = Just (TS (fromIntegral t))
  | otherwise                   = Nothing
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
maximumTimestamp xs@(_:_) = maximum xs

-- returns 'Nothing' if not representable as 'Timestamp'
posixSecondsToTimestamp :: Integer -> Maybe Timestamp
posixSecondsToTimestamp pt
  | minTs <= pt, pt <= maxTs  = Just (TS (fromInteger pt))
  | otherwise                 = Nothing
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
    Nothing          -> ""
    -- Note: we don't use 'formatTime' here to avoid incurring a
    -- dependency on 'old-locale' for older `time` libs
    Just UTCTime{..} -> showGregorian utctDay ++ ('T':showTOD utctDayTime) ++ "Z"
  where
    showTOD = show . timeToTimeOfDay

instance Binary Timestamp
instance Structured Timestamp

instance Pretty Timestamp where
    pretty = Disp.text . showTimestamp

instance Described Timestamp where
    describe _ =  REUnion
        [ posix
        , utc
        ]
      where
        posix = reChar '@' <> reMunch1CS "0123456789"
        utc   = RENamed "date" date <> reChar 'T' <> RENamed "time" time <> reChar 'Z'

        date = REOpt digit <> REUnion
            [ leapYear   <> reChar '-' <> leapMD
            , commonYear <> reChar '-' <> commonMD
            ]

        -- leap year: either
        -- * divisible by 400
        -- * not divisible by 100 and divisible by 4
        leapYear = REUnion
            [ div4           <> "00"
            , digit <> digit <> div4not0
            ]

        -- common year: either
        -- * not divisible by 400 but divisible by 100
        -- * not divisible by 4
        commonYear = REUnion
            [ notDiv4        <> "00"
            , digit <> digit <> notDiv4
            ]

        div4 = REUnion
            [ "0" <> reChars "048"
            , "1" <> reChars "26"
            , "2" <> reChars "048"
            , "3" <> reChars "26"
            , "4" <> reChars "048"
            , "5" <> reChars "26"
            , "6" <> reChars "048"
            , "7" <> reChars "26"
            , "8" <> reChars "048"
            , "9" <> reChars "26"
            ]

        div4not0 = REUnion
            [ "0" <> reChars "48" -- no zero
            , "1" <> reChars "26"
            , "2" <> reChars "048"
            , "3" <> reChars "26"
            , "4" <> reChars "048"
            , "5" <> reChars "26"
            , "6" <> reChars "048"
            , "7" <> reChars "26"
            , "8" <> reChars "048"
            , "9" <> reChars "26"
            ]

        notDiv4 = REUnion
            [ "0" <> reChars "1235679"
            , "1" <> reChars "01345789"
            , "2" <> reChars "1235679"
            , "3" <> reChars "01345789"
            , "4" <> reChars "1235679"
            , "5" <> reChars "01345789"
            , "6" <> reChars "1235679"
            , "7" <> reChars "01345789"
            , "8" <> reChars "1235679"
            , "9" <> reChars "01345789"
            ]

        leapMD = REUnion
            [ jan, fe', mar, apr, may, jun, jul, aug, sep, oct, nov, dec ]

        commonMD = REUnion
            [ jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec ]

        jan = "01-" <> d31
        feb = "02-" <> d28
        fe' = "02-" <> d29
        mar = "03-" <> d31
        apr = "04-" <> d30
        may = "05-" <> d31
        jun = "06-" <> d30
        jul = "07-" <> d31
        aug = "08-" <> d31
        sep = "09-" <> d30
        oct = "10-" <> d31
        nov = "11-" <> d30
        dec = "12-" <> d31

        d28 = REUnion
            [ "0" <> digit1, "1" <> digit, "2" <> reChars "012345678" ]
        d29 = REUnion
            [ "0" <> digit1, "1" <> digit, "2" <> digit ]
        d30 = REUnion
            [ "0" <> digit1, "1" <> digit, "2" <> digit, "30" ]
        d31 = REUnion
            [ "0" <> digit1, "1" <> digit, "2" <> digit, "30", "31" ]

        time = ho <> reChar ':' <> minSec <> reChar ':' <> minSec

        -- 0..23
        ho = REUnion
            [ "0" <> digit
            , "1" <> digit
            , "2" <> reChars "0123"
            ]

        -- 0..59
        minSec = reChars "012345" <> digit

        digit  = reChars "0123456789"
        digit1 = reChars  "123456789"

instance Parsec Timestamp where
    parsec = parsePosix <|> parseUTC
      where
        -- | Parses unix timestamps, e.g. @"\@1474626019"@
        parsePosix = do
            _ <- P.char '@'
            t <- P.integral -- note, no negative timestamps
            maybe (fail (show t ++ " is not representable as timestamp")) return $
                posixSecondsToTimestamp t

        -- | Parses ISO8601/RFC3339-style UTC timestamps,
        -- e.g. @"2017-12-31T23:59:59Z"@
        --
        -- TODO: support numeric tz offsets; allow to leave off seconds
        parseUTC = do
            -- Note: we don't use 'Data.Time.Format.parseTime' here since
            -- we want more control over the accepted formats.

            ye <- parseYear
            _ <- P.char '-'
            mo   <- parseTwoDigits
            _ <- P.char '-'
            da   <- parseTwoDigits
            _ <- P.char 'T'

            utctDay <- maybe (fail (show (ye,mo,da) ++ " is not valid gregorian date")) return $
                       fromGregorianValid ye mo da

            ho   <- parseTwoDigits
            _ <- P.char ':'
            mi   <- parseTwoDigits
            _ <- P.char ':'
            se   <- parseTwoDigits
            _ <- P.char 'Z'

            utctDayTime <- maybe (fail (show (ho,mi,se) ++  " is not valid time of day")) (return . timeOfDayToTime) $
                           makeTimeOfDayValid ho mi (realToFrac (se::Int))

            let utc = UTCTime {..}

            maybe (fail (show utc ++ " is not representable as timestamp")) return $ utcTimeToTimestamp utc

        parseTwoDigits = do
            d1 <- P.satisfy isDigit
            d2 <- P.satisfy isDigit
            return (read [d1,d2])

        -- A year must have at least 4 digits; e.g. "0097" is fine,
        -- while "97" is not c.f. RFC3339 which
        -- deprecates 2-digit years
        parseYear = do
            sign <- P.option ' ' (P.char '-')
            ds <- P.munch1 isDigit
            when (length ds < 4) $ fail "Year should have at least 4 digits"
            return (read (sign:ds))

-- | Special timestamp value to be used when 'timestamp' is
-- missing/unknown/invalid
nullTimestamp :: Timestamp
nullTimestamp = TS minBound
