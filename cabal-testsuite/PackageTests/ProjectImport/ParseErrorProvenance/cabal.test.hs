import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]
  assertOutputContainsOn unConcatOutput unConcatOutput concatOutput
    (concatOutput "When using configuration from:\n\
    \  - else.project\n\
    \  - dir-else/else.config\n\
    \The following errors occurred:\n\
    \  - The package location 'no-pkg-here' does not exist.")
    outElse

  return ()
