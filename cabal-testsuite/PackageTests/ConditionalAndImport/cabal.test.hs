import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . recordMode RecordMarked $ do
  let log = recordHeader . pure

  cabal "v2-run" [ "some-exe" ]

  log "checking cyclical loopback"
  cyclical <- fails $ cabal' "v2-build" [ "--project-file=cabal-cyclical.project" ]
  assertOutputContains "cyclical import of cabal-cyclical.project" cyclical

  log "checking bad conditional"
  badIf <- fails $ cabal' "v2-build" [ "--project-file=cabal-bad-conditional.project" ]
  assertOutputContains "Cannot set compiler in a conditional clause of a cabal project file" badIf

  return ()
