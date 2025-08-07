import Test.Cabal.Prelude
import Data.List (isInfixOf)

main = cabalTest . recordMode RecordMarked $ do
  liftIO $ skipIfWindows "I'm seeing extra newlines in the output on Windows"
  let log = recordHeader . pure

  log "checking repl command with a 'cabal.project' and --ignore-project"
  ignored <- cabal' "repl" ["--ignore-project"]
  assertOutputContains "fake-package-0 (lib) (first run)" ignored

  log "checking repl command with a 'cabal.project' and no project options"
  defaultProject <- fails $ cabal' "repl" []

  readFileVerbatim "default-repl.txt"
    >>= flip (assertOn isInfixOf multilineNeedleHaystack) defaultProject . normalizePathSeparators

  log "checking repl command using an explicit 'some.project'"
  someProject <- fails $ cabal' "repl" [ "--project-file=some.project" ]

  readFileVerbatim "some-repl.txt"
    >>= flip (assertOn isInfixOf multilineNeedleHaystack) someProject . normalizePathSeparators

  log "checking repl command using an explicit 'reverse.project', listing packages in reverse order"
  reverseProject <- fails $ cabal' "repl" [ "--project-file=reverse.project" ]

  readFileVerbatim "reverse-repl.txt"
    >>= flip (assertOn isInfixOf multilineNeedleHaystack) reverseProject . normalizePathSeparators

  log "checking repl command with an 'empty.project' with no packages"
  emptyProject <- fails $ cabal' "repl" [ "--project-file=empty.project" ]

  log "checking repl command with a missing 'missing.project'"
  missing <- fails $ cabal' "repl" [ "--project-file=missing.project" ]
  assertOutputContains "The given project file 'missing.project' does not exist." missing

  log "checking repl command with a missing 'missing.project'"
  dotMissing <- fails $ cabal' "repl" [ "--project-dir=.", "--project-file=missing.project" ]
  assertOutputContains "The given project directory/file combination './missing.project' does not exist." dotMissing

  return ()
