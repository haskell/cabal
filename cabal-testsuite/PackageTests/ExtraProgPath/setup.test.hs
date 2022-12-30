import Test.Cabal.Prelude

-- Test that setup shows all the 'autogen-modules' warnings.
main = cabalTest $ do
  skipIfWindows
  cdir <- testCurrentDir `fmap` getTestEnv
  fails $ cabal "v2-build" ["--extra-prog-path="++cdir]
