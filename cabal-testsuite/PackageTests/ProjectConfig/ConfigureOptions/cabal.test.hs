import Test.Cabal.Prelude

-- Regression test for #12245: `configure-options` is a valid field in a
-- `package` stanza, so the parsec project file parser must not warn about it.
main = cabalTest $ recordMode DoNotRecord $ do
  result <- cabal' "build" ["--dry-run", "--project-file-parser=parsec"]
  assertOutputDoesNotContain "Unknown field: \"configure-options\"" result
