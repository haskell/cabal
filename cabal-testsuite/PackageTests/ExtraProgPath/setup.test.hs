import Test.Cabal.Prelude

-- Test that extra-prog-path overrides the path for pkg-config
main = cabalTest $ do
  -- skipped on windows because using a script to dummy up an executable doesn't work the same.
  skipIfWindows
  cdir <- testCurrentDir `fmap` getTestEnv
  fails $ cabal "v2-build" ["--extra-prog-path="++cdir]
