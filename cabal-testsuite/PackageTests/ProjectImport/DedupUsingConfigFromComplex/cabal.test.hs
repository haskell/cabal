import Test.Cabal.Prelude
import Data.Function ((&))

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  log "check \"using config from message\" with URI imports"
  out <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=no-pkgs.project" ]

  log "check project configuration with URI imports is listed in full and"
  log "check package directories and locations are reported in order"

  readVerbatimFile "errors.expect.txt" >>= flip (assertOn multilineNeedleHaystack) out

  return ()
