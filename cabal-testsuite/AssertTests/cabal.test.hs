import Test.Cabal.Prelude
import Data.List (isInfixOf)
import System.Directory
import System.Exit

mkResult :: String -> Result
mkResult = Result ExitSuccess "run"

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  msg <- readFileVerbatim "msg.expect.txt"
  let out = mkResult msg
  let msgSingle = lineBreaksToSpaces msg

  log "Multiline string marking:"
  mapM_ log (lines . delimitLines $ encodeLf msg)
  assertOn isInfixOf multilineNeedleHaystack msg out
  assertOutputContains msg out
  assertOutputMatches "^When.*from:$" out
  assertOutputMatches "no[-]{1,1}pkg-here" out
  assertOutputMatches "else\\.project" out
  assertOutputMatches "else\\/else" out
  assertOutputMatches "^The f[lo]{4,}wing errors occurred[:]*$" out
  assertOutputDoesNotMatch "error occurred" out

  log "Pseudo multiline string marking:"
  mapM_ log (lines . delimitLines $ encodeLf msgSingle)
  assertOn isInfixOf multilineNeedleHaystack{expectNeedleInHaystack = False} msgSingle out
  assertOutputDoesNotContain msgSingle out

  return ()
