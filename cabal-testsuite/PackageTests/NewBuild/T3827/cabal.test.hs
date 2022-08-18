import Test.Cabal.Prelude
main = cabalTest $ do
  linux <- isLinux
  osx <- isOSX
  skipIf "8032 heisenbug profiling" (linux || osx)
  cabal "v2-build" ["exe:q"]
