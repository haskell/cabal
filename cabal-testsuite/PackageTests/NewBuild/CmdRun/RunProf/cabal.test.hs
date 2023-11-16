import Test.Cabal.Prelude

main = cabalTest $ do
  res <- cabal' "v2-run" ["--enable-profiling", "./main.hs"]
  assertOutputContains "2" res
