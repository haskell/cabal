import Test.Cabal.Prelude
main = cabalTest $ do
  heisenProfiling <- isGhcVersion ">= 9.2.0"
  linux <- isLinux
  missesProfilingLinux <- isGhcVersion ">= 9.0.2"
  osx <- isOSX
  missesProfilingOsx <- isGhcVersion ">= 8.10.7"
  skipIf "8032 heisenbug profiling" ((linux || osx) && heisenProfiling))
  expectBrokenIf (linux && missesProfilingLinux
                  || osx && missesProfilingOsx) 8032 $
    cabal "v2-build" ["exe:q"]
