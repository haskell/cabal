import Test.Cabal.Prelude

main = do
  cabalTest' "disable-enable" . recordMode RecordMarked $ do
    res <- fails $ cabal' "build" ["--project-file=cabal.disable-enable.project", "--dry-run"]
    assertOutputContains "Assertion failed" res

  cabalTest' "enable-disable" . recordMode RecordMarked $ do
    res <- cabal' "build" ["--project-file=cabal.enable-disable.project", "--dry-run"]
    assertOutputDoesNotContain "Assertion failed" res

  cabalTest' "disable" . recordMode RecordMarked $ do
    res <- fails $ cabal' "build" ["--project-file=cabal.disable.project", "--dry-run"]
    assertOutputContains "Assertion failed" res

  cabalTest' "import-disable" . recordMode RecordMarked $ do
    res <- fails $ cabal' "build" ["--project-file=cabal.import-disable.project", "--dry-run"]
    assertOutputContains "Assertion failed" res

  cabalTest' "import-enable" . recordMode RecordMarked $ do
    res <- fails $ cabal' "build" ["--project-file=cabal.import-enable.project", "--dry-run"]
    assertOutputContains "Assertion failed" res
