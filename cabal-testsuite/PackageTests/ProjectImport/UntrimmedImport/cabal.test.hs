import Test.Cabal.Prelude

main = cabalTest . flakyIfCI 10950. recordMode RecordMarked $ do
  let log = recordHeader . pure

  log "checking project import with trailing space"
  trailing <- cabal' "v2-build" [ "--dry-run", "--project-file=trailing-space.project" ]
  assertOutputContains "import has leading or trailing whitespace" trailing
  assertOutputContains "'https://www.stackage.org/nightly-2024-12-05/cabal.config '" trailing

  log "checking project import with tabs and spaces"
  cabal "v2-build" [ "--dry-run", "--project-file=tabs-and-spaces.project" ]

  return ()
