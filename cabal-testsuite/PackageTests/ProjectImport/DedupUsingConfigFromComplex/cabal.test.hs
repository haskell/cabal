import Test.Cabal.Prelude
import Data.Function ((&))
import Data.List (isInfixOf)

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  log "check \"using config from message\" with URI imports"
  out <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=no-pkgs.project" ]

  log "check project configuration with URI imports is listed in full and"
  log "check package directories and locations are reported in order"

  readFileVerbatim "errors.expect.txt"
    >>= flip (assertOn isInfixOf multilineNeedleHaystack) out . normalizePathSeparators

  return ()
