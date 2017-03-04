import Test.Cabal.Prelude
main = cabalTest $ do
    withDelay $ do
        copySourceFileTo "q/q-broken.cabal.in" "q/q.cabal"
        fails $ cabal "new-build" ["q"]
        delay
        copySourceFileTo "q/q-fixed.cabal.in" "q/q.cabal"
        cabal "new-build" ["q"]
