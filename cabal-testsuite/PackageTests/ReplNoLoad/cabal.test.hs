import Test.Cabal.Prelude

main = do
  cabalTest' "lib-normal" $ do
    cabal' "clean" []
    res <- cabalWithStdin "repl" [] ":show modules"
    assertOutputContains "Ok, one module loaded." res
  cabalTest' "lib-no-load" $ do
    cabal' "clean" []
    res <- cabalWithStdin "repl" ["--repl-no-load"] ":show modules"
    assertOutputDoesNotContain "Ok, one module loaded." res
  cabalTest' "exec-normal" $ do
    cabal' "clean" []
    res <- cabalWithStdin "repl" ["exec"] ":show modules"
    assertOutputContains "Ok, two modules loaded." res
  cabalTest' "exec-no-load" $ do
    cabal' "clean" []
    res <- cabalWithStdin "repl" ["--repl-no-load"] ":show modules"
    assertOutputDoesNotContain "Ok, two modules loaded." res
