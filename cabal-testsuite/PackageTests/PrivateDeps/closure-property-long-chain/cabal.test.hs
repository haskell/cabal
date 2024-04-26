import Test.Cabal.Prelude

main = do
  cabalTest $ recordMode DoNotRecord $ do

    -- Will violate closure property
    fails (cabal' "v2-build" ["libA"])
      >>= assertOutputContains "a private scope must contain its closure, but packages libC, libD, libE are not included in the private scope libA:P0"
