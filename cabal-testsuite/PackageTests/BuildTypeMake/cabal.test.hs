import Test.Cabal.Prelude

-- 'build-type: Make' still parses for cabal-version < 3.18, but it is no longer
-- supported. It must be rejected with a proper error, not an internal one.
main = cabalTest $ do
  result <- fails $ cabal' "v2-build" ["all"]
  assertOutputContains "'build-type: Make' is no longer supported" result
