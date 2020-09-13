import Test.Cabal.Prelude

main = cabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    withProjectFile "cabal.external.project" $ do
        cabal "v2-build" ["mylib"]
