import Test.Cabal.Prelude
import Data.List (isInfixOf)

main = cabalTest . recordMode RecordMarked $ do
  liftIO $ skipIfWindows "I'm seeing extra newlines in the output on Windows"
  let log = recordHeader . pure

  log "checking repl command with a 'cabal.project' and --ignore-project"
  ignored <- cabal' "repl" ["--ignore-project"]
  assertOutputContains "fake-package-0 (lib) (first run)" ignored

  log "checking repl command with a 'cabal.project' and no project options"
  defaultProject <- cabal' "repl" []
  assertOutputContains "the following will be built" defaultProject
  assertOutputContains "pkg-one-0.1" defaultProject

  log "checking repl command with a single package in 'cabal.project'"
  defaultProject <- cabal' "repl" [ "--project-file=cabal.project" ]
  assertOutputContains "the following will be built" defaultProject
  assertOutputContains "pkg-one-0.1" defaultProject

  return ()
