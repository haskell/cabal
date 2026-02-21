import Test.Cabal.Prelude

main = cabalTest . flakyIfCI 10975 . flakyIfCI 10927 . recordMode RecordMarked $ do
  let log = recordHeader . pure

  out <- fails $ cabal' "v2-build" [ "all", "--dry-run" ]

  -- Use assertRegex when the output is tainted by the temp directory, like
  -- this:
  --
  --   When using configuration from:
  --   - /tmp/cabal-testsuite-282695/cabal.project
  assertRegex
    "Project configuration is listed in full and deduplicated"
    "When using configuration from:(\n|\r\n) \
      \ .*cabal\\.project(\n|\r\n) \
      \ .*a-very-extra\\.config(\n|\r\n) \
      \ .*an-extra\\.config(\n|\r\n) \
      \ .*with-ghc\\.config(\n|\r\n) \
      \ .*z-empty\\.config(\n|\r\n) \
      \ .*https://www.stackage.org/lts-21.25/cabal.config(\n|\r\n)"
    out

  return ()
