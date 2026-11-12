import Test.Cabal.Prelude

main = do
  cabalTest' "all-no-load" $ do
    skipIfGhcVersion "< 9.14.1"
    cabal' "clean" []
    res <- cabalWithStdin "repl" ["--repl-no-load", "--enable-multi-repl", "all"] $ unlines
      [ ":show modules"
      , ":r ModuleA"
      , ":show modules"
      , ":r none"
      , ":r"
      ]
    assertOutputContains "Ok, no modules to be reloaded." res
    assertOutputContains "Ok, three modules reloaded." res
