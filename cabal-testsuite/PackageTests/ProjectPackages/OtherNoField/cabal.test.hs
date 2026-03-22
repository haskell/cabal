import Test.Cabal.Prelude

main = cabalTest $ do
  fails $ cabal "v2-build" ["--dry-run", "--project-file", "dir/cabal.project"]
  fails $ cabal "v2-build" ["--dry-run", "--project-file", "dir/cabal.project", "all"]
