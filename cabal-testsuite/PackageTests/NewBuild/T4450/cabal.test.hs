import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnless "no v2-build compatible boot-Cabal" =<< hasNewBuildCompatBootCabal
    -- Custom Setups inconsistently report output depending
    -- on your boot GHC.
    recordMode DoNotRecord $ cabal "v2-build" ["foo"]
    recordMode DoNotRecord $ cabal "v2-build" ["dep"]
