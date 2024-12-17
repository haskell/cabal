import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  outDefault <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=cabal.project" ]
  assertOutputContains "Error parsing project file cabal.project:3" outDefault
  assertOutputDoesNotContain "imported by:" outDefault

  outIf <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=if.project" ]
  assertOutputContains (normalizeWindowsOutput "Error parsing project file dir-if/if.config:3") outIf
  assertOutputContains "imported by:" outIf

  outElif <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=elif.project" ]
  assertOutputContains (normalizeWindowsOutput "Error parsing project file dir-elif/elif.config:4") outElif
  assertOutputContains "imported by:" outElif

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]
  assertOutputContains "Warnings found while parsing the project file, else.project:" outElse
  assertOutputContains (normalizeWindowsOutput "- dir-else/else.config: Unrecognized section '_' on line 3") outElse
  assertOutputContains "When using configuration from:" outElse

  return ()
