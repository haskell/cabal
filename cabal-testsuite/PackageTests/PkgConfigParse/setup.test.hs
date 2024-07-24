import Test.Cabal.Prelude

-- Test that invalid unicode in pkg-config output doesn't trip up cabal very much
main = cabalTest $ expectBrokenIfWindows 10179 $ do
  cdir <- testCurrentDir `fmap` getTestEnv
  res <- cabal' "v2-build" ["--extra-prog-path="++cdir, "-v2"]
  assertOutputContains "Some pkg-config packages have names containing invalid unicode: or" res
