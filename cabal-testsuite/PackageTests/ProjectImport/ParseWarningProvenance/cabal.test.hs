import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  out <- fails $ cabal' "v2-build" [ "all", "--dry-run" ]

  assertOutputContains "Warnings found while parsing the project file, cabal.project:" out

  return ()
