import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  -- Triggers "Assertion failed"
  -- log "checking repl command with no project and --ignore-project"
  -- _ <- fails $ cabal' "repl" ["--ignore-project"]

  -- Triggers "Assertion failed"
  -- log "checking repl command with no project and no project options"
  -- _ <- fails $ cabal' "repl" []

  log "checking repl command with a missing project"
  missing <- fails $ cabal' "repl" [ "--project-file=missing.project" ]
  assertOutputContains "The given project file 'missing.project' does not exist." missing

  return ()
