import System.FilePath ( joinPath )

import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/7679
-- https://github.com/haskell/cabal/issues/8400

main = cabalTest . void $ do
    res <- cabal' "list-bin" ["exe:testexe"]

    let path = joinPath ["SelectedComponent-1.0.0", "build", "testexe", "testexe"]
    assertOutputContains path res
