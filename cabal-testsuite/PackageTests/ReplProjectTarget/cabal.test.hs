import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  log "checking repl command with a project using an implicit default 'cabal.project'"
  _ <- fails $ cabal' "repl" []

  log "checking repl command with a project using an explicit 'cabal.project'"
  _ <- fails $ cabal' "repl" [ "--project-file=some.project" ]

  log "checking repl command with a project listing packages in reverse order"
  _ <- fails $ cabal' "repl" [ "--project-file=reverse.project" ]

  log "checking repl command with a project with no packages"
  _ <- fails $ cabal' "repl" [ "--project-file=empty.project" ]

  log "checking repl command with a missing project"
  missing <- fails $ cabal' "repl" [ "--project-file=missing.project" ]
  assertOutputContains "The given project file 'missing.project' does not exist." missing

  return ()
