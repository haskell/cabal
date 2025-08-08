import Test.Cabal.Prelude
import Data.List (isInfixOf)

main = cabalTest . recordMode RecordMarked $ do
  liftIO $ skipIfWindows "I'm seeing extra newlines in the output on Windows"
  let log = recordHeader . pure

  -- If there is only one package in the project then the target could be inferred.
  log "checking repl command with a 'cabal.project' and no project options"
  defaultProject <- cabal' "repl" ["pkg-one"]
  assertOutputContains "the following will be built" defaultProject
  assertOutputContains "pkg-one-0.1" defaultProject
  assertOutputContains "Compiling Bar" defaultProject

  log "checking repl command with the 'all' target"
  allTarget <- cabal' "repl" ["all"]
  assertOutputContains "the following will be built" allTarget
  assertOutputContains "pkg-one-0.1" allTarget
  assertOutputContains "Compiling Bar" allTarget

  return ()
