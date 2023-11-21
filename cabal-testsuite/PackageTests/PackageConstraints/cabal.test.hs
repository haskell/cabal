import Test.Cabal.Prelude

main = cabalTest $ withProjectFile "cabal.project" $ do
  cabal "build" ["foo"]
  fails $ cabal "build" ["foo", "-f+conflicting-constraints"]
  fails $ cabal "check" []
