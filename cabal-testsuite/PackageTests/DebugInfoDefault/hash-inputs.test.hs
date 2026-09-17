import Test.Cabal.Prelude

-- Regression test for the default debug-info level being NoDebugInfo in
-- the inputs used to compute the package hash.
--
-- By default the "debug-info" entry is omitted from the hash inputs, and
-- requesting NormalDebugInfo adds "debug-info: 2".
main = cabalTest $ do
  res <- recordMode DoNotRecord $ cabal' "v2-install" ["--overwrite-policy=always", "-v3"]
  assertOutputContains "creating file with the inputs used to compute the package hash:" res
  assertOutputDoesNotContain "debug-info:" res

  res2 <- recordMode DoNotRecord $ cabal' "v2-install" ["--overwrite-policy=always", "--enable-debug-info=2", "-v3"]
  assertOutputContains "creating file with the inputs used to compute the package hash:" res2
  assertOutputContains "debug-info: 2" res2
