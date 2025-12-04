import Test.Cabal.Prelude
import Data.List (isInfixOf)

main = cabalTest . recordMode RecordMarked $ do
  liftIO $ skipIfWindows "I'm seeing extra newlines in the output on Windows"
  let log = recordHeader . pure

  log "checking repl command with a 'cabal.project' and --ignore-project"
  ignored <- cabalWithStdin "repl" ["--ignore-project"] ""
  assertOutputContains "fake-package-0 (interactive) (lib) (first run)" ignored

  log "checking repl command with a 'cabal.project' and no project options"
  defaultProject <- fails $ cabalWithStdin "repl" [] ""

  readFileVerbatim "default-repl.txt"
    >>= flip (assertOn isInfixOf multilineNeedleHaystack) defaultProject . normalizePathSeparators

  log "checking repl command using an explicit 'some.project'"
  someProject <- fails $ cabalWithStdin "repl" [ "--project-file=some.project" ] ""

  readFileVerbatim "some-repl.txt"
    >>= flip (assertOn isInfixOf multilineNeedleHaystack) someProject . normalizePathSeparators

  log "checking repl command using an explicit 'reverse.project', listing packages in reverse order"
  reverseProject <- fails $ cabalWithStdin "repl" [ "--project-file=reverse.project" ] ""

  readFileVerbatim "reverse-repl.txt"
    >>= flip (assertOn isInfixOf multilineNeedleHaystack) reverseProject . normalizePathSeparators

  log "checking repl command with an 'empty.project' with no packages"
  emptyProject <- fails $ cabalWithStdin "repl" [ "--project-file=empty.project" ] ""

  log "checking repl command with a missing 'missing.project'"
  missing <- fails $ cabalWithStdin "repl" [ "--project-file=missing.project" ] ""
  assertOutputContains "The given project file 'missing.project' does not exist." missing

  log "checking repl command with a missing 'missing.project'"
  dotMissing <- fails $ cabalWithStdin "repl" [ "--project-dir=.", "--project-file=missing.project" ] ""
  assertOutputContains "The given project directory/file combination './missing.project' does not exist." dotMissing

  -- "all" target only works if tested GHC is >= 9.4.1
  -- If we skip here, at least we will get a failure from the other parts of the test
  -- before we reach the skip.
  skipUnlessGhcVersion ">= 9.4.1"

  log "checking repl command with the 'all' target"
  allTarget <- cabalWithStdin "repl" ["all", "--enable-multi-repl"] ""

  readFileVerbatim "all-repl.txt"
    >>= flip (assertOn isInfixOf multilineNeedleHaystack) allTarget . normalizePathSeparators

  return ()
