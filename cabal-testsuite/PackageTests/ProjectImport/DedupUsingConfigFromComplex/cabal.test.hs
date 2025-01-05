import Test.Cabal.Prelude

main = cabalTest $ do
  let log = recordHeader . pure

  log "checking \"using config from message\" with URI imports"
  out <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=no-pkgs.project" ]

  -- TODO: Make `BadPackageLocations` a `CabalInstallException` so that we can
  -- use the normal output recording here.

  -- Use assertRegex when the output is tainted by the temp directory, like
  -- this:
  --
  --   When using configuration from:
  --   - /tmp/cabal-testsuite-282695/cabal.project
  --   - /tmp/cabal-testsuite-282695/2.config etc
  assertRegex
    "Project configuration with URI imports is listed in full"
    "When using configuration from:(\n|\r\n) \
      \ .*no-pkgs\\.project(\n|\r\n) \
      \ .*0\\.config(\n|\r\n) \
      \ .*2\\.config(\n|\r\n) \
      \ .*4\\.config(\n|\r\n) \
      \ .*6\\.config(\n|\r\n) \
      \ .*8\\.config(\n|\r\n) \
      \ .*1\\.config(\n|\r\n) \
      \ .*3\\.config(\n|\r\n) \
      \ .*5\\.config(\n|\r\n) \
      \ .*7\\.config(\n|\r\n) \
      \ .*9\\.config(\n|\r\n) \
      \ .*with-ghc\\.config(\n|\r\n) \
      \ .*https://www.stackage.org/lts-21.25/cabal.config(\n|\r\n)"
    out

  log "checking that package directories and locations are reported in order"
  assertOutputContains
    "The following errors occurred: \
    \  - The package directory 'no-pkg-1' does not contain any .cabal file. \
    \    From project config no-pkgs.project \
    \  - The package location 'no-pkg-2-dir' does not exist. \
    \    From project config no-pkgs.project \
    \  - The package directory 'no-pkg-3' does not contain any .cabal file. \
    \    From project config no-pkgs.project \
    \  - The package location 'no-pkg-4-dir' does not exist. \
    \    From project config no-pkgs.project"
    out

  return ()
