import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  forM_ ["--v2-freeze-file", "--freeze-file"] $ \arg -> do
    cabal' "outdated" [arg] >>=
      (\out -> do
          assertOutputContains "base" out
          assertOutputContains "template-haskell" out
          assertOutputContains "binary" out)

    cabal' "outdated" [arg, "--ignore=base,template-haskell,binary"] >>=
      (\out -> do
          assertOutputDoesNotContain "base" out
          assertOutputDoesNotContain "template-haskell" out
          assertOutputDoesNotContain "binary" out)

    cabal' "outdated" [arg, "--minor=base,template-haskell,binary"] >>=
      (\out -> do
          assertOutputDoesNotContain "base" out
          assertOutputContains "template-haskell" out
          assertOutputContains "binary" out)

  fails $ cabal' "outdated" ["--project-file=cabal.project.missing.freeze", "--v2-freeze-file"]
  return ()
