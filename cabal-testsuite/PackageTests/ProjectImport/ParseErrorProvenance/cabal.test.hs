import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]
  msg <- readFileVerbatim "msg.expect.txt"
  assertOutputContains msg outElse

  return ()
