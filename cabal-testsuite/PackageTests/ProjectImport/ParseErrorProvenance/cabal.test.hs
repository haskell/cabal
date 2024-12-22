import Test.Cabal.Prelude
import System.Directory

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]

  msg <- readVerbatimFile "msg.expect.txt"
  let msgSingle = lineBreaksToSpaces msg

  log "Multiline string marking:"
  mapM_ log (lines . delimitLines $ encodeLf msg)

  log "Pseudo multiline string marking:"
  mapM_ log (lines . delimitLines $ encodeLf msgSingle)

  assertOn multilineNeedleHaystack msg outElse
  assertOn multilineNeedleHaystack{expectNeedleInHaystack = False} msgSingle outElse

  assertOutputContains msg outElse
  assertOutputDoesNotContain msgSingle outElse

  return ()
