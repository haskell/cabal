import Test.Cabal.Prelude

-- Test that extra-prog-path overrides the path for pkg-config
main = cabalTest $ do
  -- skipIfWindows
  cdir <- testCurrentDir `fmap` getTestEnv
  fails $ cabal "v2-build" ["--extra-prog-path="++cdir]
