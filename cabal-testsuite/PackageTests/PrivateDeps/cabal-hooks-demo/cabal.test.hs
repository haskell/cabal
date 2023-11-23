import Test.Cabal.Prelude

main =
  cabalTest $ recordMode DoNotRecord $
    withProjectFile "cabal.project" $
      withRepo "repo" $ do
        cabal "build" ["exe:hooks-exe", "--constraint=private.hooks-exe.L01:lib01 == 0.1.0.0"]
        exePath <- withPlan $ planExePath "hooks-exe" "hooks-exe"
        out1 <- cabal' "run" ["exe:main-prog", "--", exePath]

        assertOutputContains "0.1.0.0" out1
        assertOutputContains "hooks_show: A" out1
        assertOutputContains "hooks_inc: B" out1

        cabal "build" ["exe:hooks-exe", "--constraint=private.hooks-exe.L01:lib01 == 0.2.0.0"]
        out2 <- cabal' "run" ["exe:main-prog", "--", exePath]

        assertOutputContains "0.2.0.0" out2
        assertOutputContains "hooks_show: A {value = 5}" out2
        assertOutputContains "hooks_inc: B" out2
