import Test.Cabal.Prelude

-- Test that extra-prog-path overrides the path for pkg-config
main = do
  skipIfWindows "useless test (CI has no pkg-config already)"
  cabalTest $ do
    cdir <- testCurrentDir `fmap` getTestEnv
    fails $ cabal "v2-build" ["--extra-prog-path="++cdir]
