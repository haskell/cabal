import Test.Cabal.Prelude
main = do
  cabalTest $ do
    ghcVer <- isGhcVersion ">= 9.4"
    expectBrokenIf (isWindows && ghcVer) 10189 $ do
      cabal "v2-build" ["all"]
      cabal "v2-test"  ["all"]
      cabal "v2-bench" ["all"]
