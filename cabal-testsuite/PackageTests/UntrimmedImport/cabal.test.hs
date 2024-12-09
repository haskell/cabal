import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  log "checking project import with trailing space"
  trailing <- fails $ cabal' "v2-build" [ "--project-file=trailing-space.project" ]
  assertOutputContains "import has whitespace" trailing
  assertOutputContains "'https://www.stackage.org/nightly-2024-12-05/cabal.config '" trailing

  log "checking project import with tabs and spaces"
  fails $ cabal "v2-build" [ "--project-file=tabs-and-spaces.project" ]
