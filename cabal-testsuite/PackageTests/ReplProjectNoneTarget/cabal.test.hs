import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  -- The following output is not what we want but is the current behaviour that
  -- refers to the fake package.
  log "checking repl command with no project and --ignore-project"
  ignored <- cabalWithStdin "repl" ["--ignore-project"] ""
  assertOutputContains "fake-package-0 (interactive) (lib) (first run)" ignored

  -- The following output is not what we want but is the current behaviour that
  -- refers to the fake package.
  log "checking repl command with no project and no project options"
  noOptions <- cabalWithStdin "repl" [] ""
  assertOutputContains "fake-package-0 (interactive) (lib) (configuration changed)" noOptions

  log "checking repl command with a missing project"
  missing <- fails $ cabalWithStdin "repl" [ "--project-file=missing.project" ] ""
  assertOutputContains "The given project file 'missing.project' does not exist." missing

  return ()
