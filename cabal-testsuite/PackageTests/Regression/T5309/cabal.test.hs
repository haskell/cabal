import Test.Cabal.Prelude
main = cabalTest $ do
  skipIfWindows -- TODO: https://github.com/haskell/cabal/issues/6271
  cabal "v2-build" ["all"]
  cabal "v2-test"  ["all"]
  cabal "v2-bench" ["all"]
