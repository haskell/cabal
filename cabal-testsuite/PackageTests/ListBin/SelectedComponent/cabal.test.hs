import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/7679
main = cabalTest . void $ do
    res <- cabal' "list-bin" ["exe:testexe"]

    assertOutputContains "SelectedComponent-1.0.0" res
    assertOutputContains "testexe" res
