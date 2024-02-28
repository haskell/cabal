import Test.Cabal.Prelude
main = cabalTest $ withDelay $ do
        copySourceFileTo "q/q-broken.cabal.in" "q/q.cabal"
        fails $ cabal "v2-build" ["q"]
        delay
        copySourceFileTo "q/q-fixed.cabal.in" "q/q.cabal"
        cabal "v2-build" ["q"]
