import Test.Cabal.Prelude

-- #9331, guard PackageInfo functionality behind 3.12: make it a
-- build failure.
main = cabalTest $ do
  withProjectFile "cabal.project" $ do
    res <- recordMode DoNotRecord $ fails $ cabal' "v2-build" ["pkg"]
    assertOutputContains "[autogen-guard]" res

