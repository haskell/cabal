import Test.Cabal.Prelude

main = cabalTest $ do
  recordMode DoNotRecord $
    cabal' "install" ["t8848"]
      >>= assertOutputContains "Wrote tarball sdist to"
