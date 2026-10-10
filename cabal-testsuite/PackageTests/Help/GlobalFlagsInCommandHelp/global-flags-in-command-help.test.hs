import Test.Cabal.Prelude

main = cabalTest $ do
  -- The help of an individual command should mention the global flags.
  buildHelp <- cabal' "build" ["--help"]
  assertOutputContains "Global flags:" buildHelp
  assertOutputContains "--store-dir=DIR" buildHelp

  -- The same help text is reachable via `cabal help build`.
  buildHelp2 <- cabal_raw' ["help", "build"] Nothing
  assertOutputContains "--store-dir=DIR" buildHelp2

  -- For comparison, the global help has always listed `--store-dir`.
  globalHelp <- cabal' "--help" []
  assertOutputContains "--store-dir=DIR" globalHelp
