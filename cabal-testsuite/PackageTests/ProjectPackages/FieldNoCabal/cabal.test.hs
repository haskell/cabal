import Test.Cabal.Prelude

main = cabalTest $ do
  let expected = "- The package directory '.' does not contain any .cabal file."
  r1 <- fails $ cabal' "v2-build" ["--dry-run"]
  assertOutputContains expected r1

  r2 <- fails $ cabal' "v2-build" ["--dry-run", "all"]
  assertOutputContains expected r2
