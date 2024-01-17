import Test.Cabal.Prelude

-- `cabal check` works when passing a directory to check.
main = cabalTest $ do
  cabal "check" ["target/"]
