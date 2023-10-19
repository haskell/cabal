import Test.Cabal.Prelude

main = cabalTest . void $ do
  res <- cabal_raw' ["path"] Nothing

  assertOutputContains "config-file:" res
  assertOutputContains "installdir:" res
  assertOutputContains "cache-dir:" res
  assertOutputContains "logs-dir:" res
  assertOutputContains "store-dir:" res
