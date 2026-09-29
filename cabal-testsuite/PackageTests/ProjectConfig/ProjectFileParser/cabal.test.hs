import Test.Cabal.Prelude

-- The parser is chosen before the project file is read, so a
-- project-file-parser field in the project file can have no effect. The
-- legacy parser accepts it without a word and the parsec parser warns.
main = cabalTest . recordMode RecordMarked $ do
  let warning = "Unknown field: \"project-file-parser\""

  legacy <- cabal' "build" ["--dry-run", "--project-file-parser=legacy"]
  assertOutputDoesNotContain warning legacy

  parsec <- cabal' "build" ["--dry-run", "--project-file-parser=parsec"]
  assertOutputContains warning parsec
  pure ()
