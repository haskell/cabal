import Test.Cabal.Prelude

main = do
  cabalTest $ recordMode DoNotRecord $ do

    -- Will violate closure property
    withProjectFile "cabal.project.1" $
      withRepo "repo" $
        fails (cabal' "v2-build" ["libA"])
          >>= assertOutputContains "a private scope must contain its closure, but package libC is not included in the private scope libA:G0"

    -- Must pick libC == 0.1
    withProjectFile "cabal.project.2" $
      withRepo "repo" $
        cabal' "v2-build" ["libA"]
          >>= assertOutputContains "libC-0.1.0.0"

    -- Shouldn't pick libB == 0.3 because it violates closure property
    withProjectFile "cabal.project.3" $
      withRepo "repo" $
        cabal' "v2-build" ["libA"]
          >>= assertOutputDoesNotContain "libB-0.3.0.0"

    -- Will be OKay with libB == 0.3 and libC == 0.2 because libC is in the closure
    withProjectFile "cabal.project.4" $
      withRepo "repo" $ do
        o <- cabal' "v2-build" ["libA"]
        assertOutputContains "libC-0.2.0.0" o
        assertOutputContains "libB-0.3.0.0" o
