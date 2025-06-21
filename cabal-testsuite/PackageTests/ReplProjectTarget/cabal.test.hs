import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  log "checking repl command with a 'cabal.project' and --ignore-project"
  ignored <- fails $ cabal' "repl" ["--ignore-project"]
  assertOutputDoesNotContain "Configuration is affected by the following files:" ignored
  assertOutputDoesNotContain "- cabal.project" ignored

  log "checking repl command with a 'cabal.project' and no project options"
  defaultProject <- fails $ cabal' "repl" []
  assertOutputContains "Configuration is affected by the following files:" defaultProject
  assertOutputContains "- cabal.project" defaultProject

  log "checking repl command using an explicit 'some.project'"
  someProject <- fails $ cabal' "repl" [ "--project-file=some.project" ]
  assertOutputContains "Configuration is affected by the following files:" someProject
  assertOutputContains "- some.project" someProject

  log "checking repl command using an explicit 'reverse.project', listing packages in reverse order"
  reverseProject <- fails $ cabal' "repl" [ "--project-file=reverse.project" ]
  assertOutputContains "Configuration is affected by the following files:" reverseProject
  assertOutputContains "- reverse.project" reverseProject

  log "checking repl command with an 'empty.project' with no packages"
  emptyProject <- fails $ cabal' "repl" [ "--project-file=empty.project" ]
  assertOutputContains "Configuration is affected by the following files:" emptyProject
  assertOutputContains "- empty.project" emptyProject

  log "checking repl command with a missing 'missing.project'"
  missing <- fails $ cabal' "repl" [ "--project-file=missing.project" ]
  assertOutputContains "The given project file 'missing.project' does not exist." missing

  log "checking repl command with a single package in 'one.project'"
  one <- fails $ cabal' "repl" [ "--project-file=one.project" ]
  assertOutputContains "In order, the following will be built" one
  assertOutputContains "pkg-one-0.1 (interactive) (first run)" one

  return ()
