import Test.Cabal.Prelude
main = setupAndCabalTest $ do
  output <- cabal' "sdist" []
  assertOutputContains "Using a '*' character in 'data-dir'" output
