import Test.Cabal.Prelude

-- The parser is chosen before the project file is read, so a
-- project-file-parser field in the project file can have no effect. Both
-- parsers warn about it, the parsec parser saying why.
main = do
  let warning = "The project-file-parser field has no effect in a project file"

  cabalTest' "legacy" . recordMode RecordMarked $ do
    legacy <- cabal' "build" ["--dry-run", "--project-file-parser=legacy"]
    assertOutputContains "Unrecognized field 'project-file-parser'" legacy

  cabalTest' "parsec" . recordMode RecordMarked $ do
    parsec <- cabal' "build" ["--dry-run", "--project-file-parser=parsec"]
    assertOutputContains warning parsec
