import Test.Cabal.Prelude

main = do
    putStrLn "We ride!"
    cabalTest $
        r <- cabal' "new-show-build-info" ["A"]
        assertOutputContains "Hi" r
