import Test.Cabal.Prelude
import Data.Function ((&))

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  log "checking \"using config from message\" with URI imports"
  out <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=no-pkgs.project" ]

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

  "The following errors occurred:\n\
  \  - The package directory 'no-pkg-1' does not contain any .cabal file.\n\
  \  - The package location 'no-pkg-2-dir' does not exist.\n\
  \  - The package directory 'no-pkg-3' does not contain any .cabal file.\n\
  \  - The package location 'no-pkg-4-dir' does not exist."
    & flip (assertOn multilineNeedleHaystack) out

  return ()
