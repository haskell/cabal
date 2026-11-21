import Test.Cabal.Prelude

-- Each of these is an ISO8601 timestamp, but not one that cabal accepts yet
-- for --index-state. The only form it takes is YYYY-MM-DDThh:mm:ssZ.
--
-- The message for each is recorded in cabal.out. The whole output is recorded
-- because an option parsing error is not marked as output.
main = cabalTest . recordMode RecordAll $ do
  -- A date on its own.
  fails $ cabal "v2-build" ["--index-state=2026-06-01"]
  -- A time without seconds.
  fails $ cabal "v2-build" ["--index-state=2026-06-01T12:30Z"]
  -- A time with a UTC offset.
  fails $ cabal "v2-build" ["--index-state=2026-06-01T12:00:00+12:00"]
