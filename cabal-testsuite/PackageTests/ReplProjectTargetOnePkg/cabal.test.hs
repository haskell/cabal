import Test.Cabal.Prelude
import Data.List (isInfixOf)

main = cabalTest . recordMode RecordMarked $ do
  liftIO $ skipIfWindows "I'm seeing extra newlines in the output on Windows"
  let log = recordHeader . pure

  log "checking repl command with a 'cabal.project' and --ignore-project"
  ignored <- cabalWithStdin "repl" ["--ignore-project"] ""
  assertOutputContains "fake-package-0 (interactive) (lib) (first run)" ignored

  log "checking repl command with a 'cabal.project' and no project options"
  defaultProject <- cabalWithStdin "repl" [] ""
  assertOutputContains "the following will be built" defaultProject
  assertOutputContains "pkg-one-0.1" defaultProject

  log "checking repl command with a single package in 'cabal.project'"
  defaultProject <- cabalWithStdin "repl" [ "--project-file=cabal.project" ] ""
  assertOutputContains "the following will be built" defaultProject
  assertOutputContains "pkg-one-0.1" defaultProject

  log "checking repl command with the 'all' target"
  allTarget <- cabalWithStdin "repl" ["all"] ""
  assertOutputContains "the following will be built" allTarget
  assertOutputContains "pkg-one-0.1" allTarget

  return ()
