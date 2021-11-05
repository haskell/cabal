import Test.Cabal.Prelude
import Control.Monad.Reader (local)

main = do
  cabalTest' "single-repl-options" $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" ["--repl-options=-fwrite-interface"] ":set"
    assertOutputContains "Ok, two modules loaded." res
    assertOutputContains "-fwrite-interface" res
  cabalTest' "multiple-repl-options" $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" ["--repl-options=-fwrite-interface", "--repl-options=-fdiagnostics-show-caret"] ":set"
    assertOutputContains "Ok, two modules loaded." res
    assertOutputContains "-fwrite-interface" res
    assertOutputContains "-fdiagnostics-show-caret" res
  cabalTest' "single-repl-options-multiple-flags" $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" ["--repl-options=-fdiagnostics-show-caret -fwrite-interface"] ":set"
    assertOutputContains "Ok, two modules loaded." res
    assertOutputContains "-fwrite-interface" res
    assertOutputContains "-fdiagnostics-show-caret" res
  cabalTest' "single-repl-options-multiple-flags-negative" $ do
    cabal' "clean" []
    res <- local setTestAsNegative $ cabalWithStdin "v2-repl" ["--repl-options=-fdiagnostics-show-baret -fwrite-interface"] ":set"
    assertOutputDoesNotContain "Ok, two modules loaded." res
    assertOutputContains "-fwrite-interface" res
    assertOutputContains "-fdiagnostics-show-baret" res
    assertOutputContains "ghc: unrecognised flag: -fdiagnostics-show-baret" res
    assertOutputContains "did you mean one of:" res
  cabalTest' "multiple-repl-options-multiple-flags" $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" [
      "--repl-options=-fdiagnostics-show-caret -fwrite-interface",
        "--repl-options=-fdefer-type-errors -fdefer-typed-holes"
      ] ":set"
    assertOutputContains "Ok, two modules loaded." res
    assertOutputContains "-fwrite-interface" res
    assertOutputContains "-fdiagnostics-show-caret" res
      where
        setTestAsNegative env = env { testShouldFail = True }
