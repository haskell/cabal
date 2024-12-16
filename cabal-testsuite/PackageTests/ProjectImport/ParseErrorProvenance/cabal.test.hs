import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  outIf <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=if.project" ]
  assertOutputContains "Error parsing project file dir-if/if.config:3" outIf

  outElif <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=elif.project" ]
  assertOutputContains "Error parsing project file dir-elif/elif.config:4" outElif

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]
  assertOutputContains "Warnings found while parsing the project file, else.project:" outElse
  assertOutputContains "- dir-else/else.config: Unrecognized section '_' on line 3" outElse

  return ()
