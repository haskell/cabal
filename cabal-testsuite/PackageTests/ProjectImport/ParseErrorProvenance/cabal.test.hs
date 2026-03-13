import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]
  msg <- readFileVerbatim "msg.expect.txt"
  assertOutputContains msg outElse


  outDefault <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=cabal.project" ]
  assertOutputContains "cabal.project:3:4: error:" outDefault
  assertOutputDoesNotContain "imported by:" outDefault

  outIf <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=if.project" ]
  assertOutputContains (normalizeWindowsOutput "dir-if/if.config:3:4: error:") outIf
  assertOutputContains "imported by:" outIf

  outElif <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=elif.project" ]
  assertOutputContains (normalizeWindowsOutput "dir-elif/elif.config:4:6: error:") outElif
  assertOutputContains "imported by:" outElif

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]
  assertOutputContains "Warnings found while parsing the project file, else.project:" outElse
  assertOutputContains (normalizeWindowsOutput "- dir-else/else.config:3:5: Invalid subsection \"_\"") outElse
  assertOutputContains "When using configuration from:" outElse

  return ()
