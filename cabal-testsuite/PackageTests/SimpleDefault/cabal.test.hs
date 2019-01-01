import Test.Cabal.Prelude
main = cabalTest $ do
    recordMode DoNotRecord $ do
        -- TODO: Hack; see also CustomDep/cabal.test.hs
        withEnvFilter (/= "HOME") $ do
            cabal "v2-build" ["all"]
