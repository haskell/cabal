import Test.Cabal.Prelude

singleOpts = ["--repl-options=-fwrite-interface"]
multiOpts = "--repl-options=-fdefer-typed-holes" : singleOpts
altProject = ("--project-file=alt.project" :)

main = do
  cabalTest' "single-repl-options" $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" singleOpts ":set"
    assertOutputContains "Ok, two modules loaded." res

  cabalTest' "alt-single-repl-options" $ do
    cabal' "clean" []
    -- We can 'cabal repl' without a target when the project has a single package.
    void $ cabalWithStdin "v2-repl" (altProject singleOpts) ":set"

  cabalTest' "multiple-repl-options" $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" multiOpts ":set"
    assertOutputContains "Ok, two modules loaded." res
    assertOutputContains "  -fwrite-interface" res
    assertOutputContains "  -fdefer-typed-holes" res

  cabalTest' "alt-multiple-repl-options" $ do
    cabal' "clean" []
    -- We can 'cabal repl' without a target when the project has a single package.
    void $ cabalWithStdin "v2-repl" (altProject multiOpts) ":set"

  cabalTest' "single-repl-options-multiple-flags" $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" ["--repl-options=-fdefer-typed-holes -fwrite-interface"] ":set"
    assertOutputContains "Ok, two modules loaded." res
    assertOutputContains "  -fwrite-interface" res
    assertOutputContains "  -fdefer-typed-holes" res

  cabalTest' "single-repl-options-multiple-flags-negative" $ do
    cabal' "clean" []
    res <- fails $ cabalWithStdin "v2-repl" ["--repl-options=-fwrite-interface -fdiagnostics-show-baret"] ":set"
    assertOutputDoesNotContain "Ok, two modules loaded." res
    assertOutputContains "unrecognised flag: -fdiagnostics-show-baret" res
    assertOutputContains "did you mean one of:" res

  cabalTest' "multiple-repl-options-multiple-flags" $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" [
      "--repl-options=-fforce-recomp -fwrite-interface",
        "--repl-options=-fdefer-type-errors -fdefer-typed-holes"
      ] ":set"
    assertOutputContains "Ok, two modules loaded." res
    assertOutputContains "  -fwrite-interface" res
    assertOutputContains "  -fforce-recomp" res
    assertOutputContains "  -fdefer-typed-holes" res
    assertOutputContains "  -fdefer-type-errors" res

