import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  log "checking \"using config from message\" with URI imports"
  woopping <- fails $ cabal' "v2-build" [ "--dry-run", "--project-file=woops-0-packages-in-woops.project" ]

  -- Use assertRegex when the output is tainted by the temp directory, like
  -- this:
  --
  --   When using configuration from:
  --   - /tmp/cabal-testsuite-282695/woops-0.project
  --   - /tmp/cabal-testsuite-282695/woops-2.config etc
  assertRegex
    "Project configuration with URI imports is listed in full"
    "When using configuration from:(\n|\r\n) \
      \ .*woops-0-packages-in-woops\\.project(\n|\r\n) \
      \ .*with-ghc\\.config(\n|\r\n) \
      \ .*woops-0\\.config(\n|\r\n) \
      \ .*woops-2\\.config(\n|\r\n) \
      \ .*woops-4\\.config(\n|\r\n) \
      \ .*woops-6\\.config(\n|\r\n) \
      \ .*woops-8\\.config(\n|\r\n) \
      \ .*woops-1\\.config(\n|\r\n) \
      \ .*woops-3\\.config(\n|\r\n) \
      \ .*woops-5\\.config(\n|\r\n) \
      \ .*woops-7\\.config(\n|\r\n) \
      \ .*woops-9\\.config(\n|\r\n) \
      \ .*https://www.stackage.org/lts-21.25/cabal.config(\n|\r\n)"
    woopping

  assertOutputContains
    "The following errors occurred: \
    \  - The package directory 'woops' does not contain any .cabal file."
    woopping

  return ()
