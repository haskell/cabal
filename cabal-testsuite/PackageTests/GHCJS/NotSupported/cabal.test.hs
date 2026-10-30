import Test.Cabal.Prelude

-- Selecting the removed GHCJS compiler flavor must fail with a message that
-- points at the JavaScript backend of GHC.
main = cabalTest $ do
  result <- fails $ cabal' "v2-build" ["all"]
  assertOutputContains "GHCJS is no longer supported" result
