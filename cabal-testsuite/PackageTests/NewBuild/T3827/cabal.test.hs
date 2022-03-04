import Test.Cabal.Prelude
main = cabalTest $ do
  missesProfiling <- isGhcVersion ">= 9.2.1"
  osx <- isOSX
  missesProfilingOsx <- isGhcVersion ">= 8.10.7"
  expectBrokenIf (missesProfiling || osx && missesProfilingOsx) 8032 $
    cabal "v2-build" ["exe:q"]
