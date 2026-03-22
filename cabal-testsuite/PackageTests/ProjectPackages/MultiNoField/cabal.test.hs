import Test.Cabal.Prelude

main = cabalTest $ do
  fails $ cabal "v2-build" ["--dry-run"]
  fails $ cabal "v2-build" ["--dry-run", "all"]
