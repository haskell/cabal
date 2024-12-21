import Test.Cabal.Prelude
import System.Directory

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure
  cwd <- liftIO getCurrentDirectory
  env <- getTestEnv
  let testDir = testCurrentDir env
  liftIO . putStrLn $ "Current working directory: " ++ cwd
  msg <- liftIO . readFile $ testDir </> "msg.txt"

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]

  let msgSingle = lineBreaksToSpaces msg

  log "Multiline string marking:"
  mapM_ log (lines . decodeLfMarkLines $ encodeLf msg)

  log "Pseudo multiline string marking:"
  mapM_ log (lines . decodeLfMarkLines $ encodeLf msgSingle)

  assertOn multilineNeedleHaystack msg outElse
  assertOn multilineNeedleHaystack{expectNeedleInHaystack = False} msgSingle outElse

  assertOutputDoesNotContain msg outElse
  assertOutputDoesNotContain msgSingle outElse

  return ()
