import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let errMsg = "Can't parse debug info level"

  numeric <- cabal' "build" ["--project-file=cabal.numeric.project", "--dry-run"]
  assertOutputDoesNotContain errMsg numeric

  boolean <- cabal' "build" ["--project-file=cabal.boolean.project", "--dry-run"]
  assertOutputDoesNotContain errMsg boolean
