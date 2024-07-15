import Test.Cabal.Prelude
main = cabalTest $ do
  skipIf "8032 heisenbug profiling" (isLinux || isOSX)
  cabal "v2-build" ["exe:q"]
