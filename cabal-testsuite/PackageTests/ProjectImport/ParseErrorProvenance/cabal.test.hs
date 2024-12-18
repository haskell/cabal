import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  let expectMulti =
        "When using configuration from:\n\
        \  - else.project\n\
        \  - dir-else/else.config\n\
        \The following errors occurred:\n\
        \  - The package location 'no-pkg-here' does not exist."

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]

  let expectSingle = filter (/= '\n') expectMulti

  log "Multiline string marking:"
  mapM_ log (lines . unConcatOutput $ concatOutput expectMulti)

  log "Pseudo multiline string marking:"
  mapM_ log (lines . unConcatOutput $ concatOutput expectSingle)

  assertOutputContainsMultiline expectMulti outElse
  assertOutputDoesNotContainMultiline expectSingle outElse

  assertOutputDoesNotContain expectMulti outElse
  assertOutputDoesNotContain expectSingle outElse

  assertOutputContainsOn unConcatOutput concatOutput unConcatOutput concatOutput expectMulti outElse
  assertOutputDoesNotContainOn id id unConcatOutput concatOutput expectSingle outElse

  assertOutputContainsOn id id id id expectMulti outElse
  assertOutputDoesNotContainOn id id id id expectSingle outElse

  return ()
