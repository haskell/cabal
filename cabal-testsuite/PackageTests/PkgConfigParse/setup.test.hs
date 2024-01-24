import Test.Cabal.Prelude

-- Test that invalid unicode in pkg-config output doesn't trip up cabal very much
main = cabalTest $ do
  -- skipped on windows because using a script to dummy up an executable doesn't work the same.
  skipIfWindows
  cdir <- testCurrentDir `fmap` getTestEnv
  res <- cabal' "v2-build" ["--extra-prog-path="++cdir, "-v2"]
  assertOutputContains "Some pkg-config packages have names containing invalid unicode: or" res
