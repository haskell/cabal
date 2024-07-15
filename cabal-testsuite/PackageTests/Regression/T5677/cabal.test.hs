import Test.Cabal.Prelude
main = cabalTest $ do
  -- -Wmissing-export-lists is new in 8.4.
  skipUnlessGhcVersion ">= 8.3"
  cabal "v2-build" ["all"]
