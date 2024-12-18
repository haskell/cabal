import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]
  assertOutputContains
    (concatOutput "When using configuration from: \
    \  - else.project \
    \  - dir-else/else.config \
    \The following errors occurred: \
    \  - The package location 'no-pkg-here' does not exist.")
    outElse

  return ()
